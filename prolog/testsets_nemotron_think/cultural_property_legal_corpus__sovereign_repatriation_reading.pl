% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__sovereign_repatriation_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Reading of Cultural Property Law
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   The sovereign repatriation reading asserts that cultural artifacts
 *   removed during colonial rule are the sovereign property of successor
 *   states — modern nations claiming historical continuity with pre-colonial
 *   polities. Colonial acquisition is framed as illegitimate extraction;
 *   legitimate authority rests with states, not museums or communities. This
 *   reading operates through international law (UNESCO 1970, UNIDROIT 1995),
 *   bilateral treaties, and domestic legislation. It produces a tangled rope:
 *   genuine coordination of repatriation claims against fragmented holders,
 *   combined with asymmetric extraction of identity capital and artifacts
 *   from holding institutions. The constraint has strengthened over 1970–2025
 *   as normative consensus shifted.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.45).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.42).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Reading of Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e').
narrative_ontology:cs_kernel_codification('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', formalized).
narrative_ontology:cs_authority_grounding('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', lineage).
narrative_ontology:cs_interpretation_layer_present('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e').
narrative_ontology:cs_reading_relation('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', foundational, colonial_acquisition_illegitimate).
narrative_ontology:cs_axiom_status(colonial_acquisition_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', colonial_acquisition_illegitimate, conventional).
narrative_ontology:cs_axiom('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', foundational, successor_state_sovereignty_over_cultural_property).
narrative_ontology:cs_axiom_status(successor_state_sovereignty_over_cultural_property, holdable).
narrative_ontology:cs_axiom_grounding('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', successor_state_sovereignty_over_cultural_property, conventional).
narrative_ontology:cs_reference_frame('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', pre_colonial_sovereign_authority).
narrative_ontology:cs_drift_state('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', contemporary_repatriation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c097e5b2-8fe9-4bd1-ad3f-26e9da096a3e', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, post_colonial_governments).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, former_colonial_museums).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, source_community_activists).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_acquisition_illegitimate).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, state_succession_cultural_sovereignty).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, unesco_1970_convention_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States claiming historical continuity with pre-colonial polities. They initiate repatriation claims through diplomatic channels, domestic legislation, and international forums. They gain symbolic capital, political legitimacy, and physical artifacts. Their exit options are constrained by the need to maintain the legal framework — abandoning claims would lose the sovereignty assertion itself.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, agenda_setter).

% Governments of post-colonial nations that may not claim direct civilizational continuity but inherit the colonial border state. They benefit from the legal framework for repatriation as a tool of national identity construction and foreign policy leverage. Their position is more politically contingent than civilizational-successor states.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, post_colonial_governments, beneficiary,
    institutional, biographical, constrained, national).

% Major museums and collections (British Museum, Louvre, Met, Humboldt Forum, etc.) holding colonial-era artifacts. They bear costs of provenance research, legal defense, physical transfer, and loss of collection integrity. Their exit is constrained by public scrutiny, donor pressure, and the reputational cost of refusing all claims. Some adapt by negotiating long-term loans or shared custody.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions, payer,
    institutional, generational, constrained, global).

% National museums in former colonial powers (UK, France, Belgium, Netherlands, Germany) that hold the largest concentrations. They face state-level political pressure to repatriate but also hold institutional mandates of universal preservation. Their exit is constrained by national heritage laws they helped shape.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, former_colonial_museums, payer,
    organized, biographical, constrained, national).

% Communities maintaining direct cultural continuity with artifact creators, often within or across successor state borders. They claim authority based on living cultural practice, not state succession. The sovereign repatriation reading structurally excludes them by locating legitimate authority in the state, not the community. Their identity is fused to the artifacts — exit from the relationship is unthinkable.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities, excluded,
    moderate, generational, identity_locked, local).

% Institutions and scholars (ICOM, UNESCO secretariat elements, cosmopolitan museum directors) arguing artifacts belong to humanity and should remain where preservation and access are maximized. They are excluded from the sovereign repatriation framework's legitimacy calculus. Their exit is mobile — they can shift advocacy to other frameworks.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_heritage_advocates, excluded,
    organized, civilizational, mobile, global).

% ICJ, UNESCO Intergovernmental Committee, UNIDROIT, national courts hearing repatriation cases. They adjudicate between competing readings, develop customary law, and issue advisory opinions. They neither collect artifacts nor bear transfer costs — their position is analytical.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Local activists and cultural practitioners in successor states who pressure their own governments to pursue claims, but who may be marginalized when state-centric repatriation centers national museums rather than community access. They bear the cost of advocacy and the risk of state co-optation of returned artifacts.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, source_community_activists, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, source_community_activists, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legitimate legal authority for repatriation of colonial-era cultural property by locating sovereignty in successor states, providing a unified claimant against fragmented holding institutions.
% TRANSFER_FUNCTION: Moves cultural artifacts and symbolic capital from holding institutions (primarily in former colonial powers) to successor states; moves legal authority from colonial-era acquisition frameworks to post-colonial sovereignty doctrines.
% ABSENT_VOICES: Indigenous communities claiming authority based on living cultural continuity rather than state succession; universal heritage advocates prioritizing preservation and global access over geographic return. Both are structurally excluded by the state-centric legitimacy criterion.
% DISAPPEARANCE_RATIONALE: If the sovereign repatriation framework vanished, successor states would lose their primary legal basis for claims; holding institutions would revert to colonial-era title defenses; the international normative consensus (UNESCO 1970, UNIDROIT 1995) would fracture into ad hoc bilateral negotiations or universal heritage arguments.
% FOUNDING_PROBLEM: Colonial powers extracted cultural property through conquest, unequal treaties, and coercive purchase without consent of originating peoples; post-colonial states needed a legal framework to reclaim sovereign control over their cultural patrimony.
% FOUNDING_PROBLEM_CORROBORATION: UNESCO 1970 Convention and UNIDROIT 1995 Convention (ratified by 140+ states, not only claimant states); ICJ advisory opinions on cultural property; domestic repatriation laws in France (2020), Germany (2019), Netherlands (2021) enacted by governments not solely controlled by beneficiary elites; scholarly work by non-claimant-state legal scholars (e.g., Prott, O'Keefe, Francioni) affirming the illegitimacy of colonial acquisition.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).
:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45): holding institutions bear real costs (provenance research, legal defense, physical transfer, collection loss) but these are bounded and often negotiated. Suppression is moderate (0.42): the constraint suppresses the universal heritage and indigenous stewardship alternatives through state-centric legal standing, but does not eliminate them — they persist in parallel forums. Theater ratio is low-moderate (0.28): legal processes are genuine but some state performances are symbolic. Accessibility collapse is moderate (0.52): alternative frameworks remain live but are structurally disadvantaged. Resistance is moderate-high (0.58): holding institutions and former colonial states actively resist through legal delays, loan substitutes, and universal heritage rhetoric.
 *
 * PERSPECTIVAL GAP:
 *   The successor state seat experiences this as genuine coordination restoring sovereignty. The holding institution seat experiences it as enforced extraction of assets they stewarded. The indigenous community seat experiences it as a second dispossession — the state claims authority over artifacts the community holds sacred. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states are structural beneficiaries (d ~ 0.15): they collect artifacts and symbolic capital, set the legal agenda, and face constrained exit (abandoning claims loses sovereignty assertion). Holding institutions are targets (d ~ 0.85): they bear costs, face active enforcement, and have constrained exit (reputational/legal pressure). Indigenous communities are identity-locked excluded (d ~ 0.95): the framework structurally denies their standing; their identity is fused to artifacts the state claims. Universal heritage advocates are mobile excluded (d ~ 0.4): they lose influence but retain analytical exit. International legal bodies are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial extraction) remains live — artifacts remain in foreign institutions, and new claims emerge (Benin Bronzes, Parthenon Marbles, Hawaiian artifacts). The constraint has not atrophied; its enforcement has intensified. No mandatrophy resolution. The constraint is not a piton: theater_ratio is low and beneficiaries (successor states) actively maintain it for concentrated symbolic/political gain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the sovereign repatriation reading a distinct constraint from the universal heritage and indigenous stewardship readings, or are they observable variants of the same constraint?',
    'Apply ε-invariance test: if measuring extractiveness/suppression under each reading''s operational criteria yields structurally different values that cannot be reconciled by a single ε, they are distinct constraints. Current assessment: distinct — each reading produces different beneficiary/victim structures and different enforcement logics.',
    'If they are one constraint, the framework must model reading-dependent ε (forbidden by DP-001). If distinct, each gets its own story linked by network.affects_constraints. This story treats them as distinct per ε-invariance principle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate one constraint or three').

omega_variable(
    successor_state_legitimacy_boundary,
    'Which states qualify as legitimate successors for repatriation claims — only those with direct civilizational continuity, or all post-colonial border states?',
    'Track ICJ/UNESCO committee rulings and state practice: do claims by border states without civilizational continuity (e.g., modern Nigeria for Benin Bronzes vs. Kingdom of Benin descendants) succeed on the same legal basis?',
    'If only civilizational successors qualify, the beneficiary set narrows and extractiveness on holding institutions concentrates. If all post-colonial states qualify, the framework expands but dilutes the historical continuity premise — potentially shifting toward universal heritage logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_state_legitimacy_boundary, conceptual, 'Boundary of the beneficiary class within the sovereign repatriation reading').

omega_variable(
    indigenous_exclusion_mechanism,
    'Does the state-centric framework actively suppress indigenous stewardship claims, or merely fail to recognize them?',
    'Analyze domestic repatriation laws: do they require state-to-state transfer only, or do they mandate community consultation/control? Track cases where returned artifacts go to national museums vs. community institutions.',
    'If active suppression, the constraint''s suppression metric understates the harm to indigenous communities (internalized suppression). If mere non-recognition, the indigenous_stewardship_reading remains a parallel live option (coexists_with relation holds).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_exclusion_mechanism, empirical, 'Whether state-centric repatriation structurally excludes or actively suppresses indigenous authority').

omega_variable(
    enforcement_effectiveness_gap,
    'Can successor states actually enforce repatriation against resistant holding institutions, or is the constraint largely aspirational?',
    'Measure repatriation completion rates vs. claims filed; track institutional resistance tactics (long-term loans, shared custody, provenance disputes) that satisfy the letter but not the spirit.',
    'If enforcement is low, the constraint may be a scaffold (transitional) or piton (performative). Current trajectory shows rising completions (Benin Bronzes to Nigeria/Germany, Hawaiian artifacts, Maori remains) suggesting genuine enforcement capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_effectiveness_gap, empirical, 'Gap between legal authority and material transfer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cplc_srr_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(cplc_srr_tr_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(cplc_srr_tr_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(cplc_srr_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(cplc_srr_tr_t2020, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(cplc_srr_tr_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(cplc_srr_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(cplc_srr_be_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement(cplc_srr_be_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(cplc_srr_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(cplc_srr_be_t2020, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(cplc_srr_be_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2025, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cplc_srr_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(cplc_srr_su_t1985, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1985, 0.28).
narrative_ontology:measurement(cplc_srr_su_t2000, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(cplc_srr_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(cplc_srr_su_t2020, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement(cplc_srr_su_t2025, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.12).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form the cultural_property_legal_corpus constraint family. The sovereign_repatriation_reading and universal_heritage_reading coexist as live positions held by different institutional coalitions. The sovereign_repatriation_reading influences the indigenous_stewardship_reading by occupying the 'legitimate authority' position in international law, creating structural pressure on indigenous claims. All three share the referent (colonial-era cultural property) but instantiate different constraints with different ε, beneficiaries, and enforcement logics per ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__sovereign_repatriation_reading, institutional, 0.15).
constraint_indexing:directionality_override(cultural_property_legal_corpus__sovereign_repatriation_reading, institutional, 0.85).
constraint_indexing:directionality_override(cultural_property_legal_corpus__sovereign_repatriation_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
