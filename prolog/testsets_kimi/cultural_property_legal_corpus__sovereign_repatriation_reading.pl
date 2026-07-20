% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Reading of Cultural Property Legal Corpus
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint instantiates the sovereign_repatriation_reading of the
 *   cultural_property_legal_corpus kernel. It treats cultural artifacts as
 *   sovereign property of successor states that claim historical continuity
 *   with expropriated peoples, framing colonial acquisition as illegitimate
 *   extraction. This reading competes with the universal_heritage_reading
 *   (artifacts as humanity's shared patrimony) and the
 *   indigenous_stewardship_reading (artifacts as communal property of
 *   indigenous groups). The constraint operates through international
 *   treatiesâprimarily the 1970 UNESCO Convention and the 1995 UNIDROIT
 *   Conventionâand bilateral state agreements that establish legal pathways
 *   for restitution. Successor states are the primary beneficiaries,
 *   receiving physical custody and symbolic capital; holding institutions in
 *   former colonial powers are the primary payers, bearing deaccession costs
 *   and loss of collection assets. Indigenous communities occupy a contested
 *   position: they may benefit indirectly through state-mediated returns, but
 *   their own sovereignty claims are frequently subordinated to the successor
 *   state, making them secondary payers or excluded parties.
 *
 * KEY AGENTS:
 *   - Successor states (beneficiary/institutional): Claim historical continuity and assert ownership over cultural artifacts removed during colonialism.
 *   - Holding institutions (payer/institutional): Museums and cultural institutions in former colonial powers that face legal and diplomatic pressure to return collections.
 *   - Indigenous communities (payer/organized): Groups with direct cultural continuity to artifacts but limited standing in state-centric international frameworks.
 *   - International legal bodies (agenda_setter/institutional): UNESCO, UNIDROIT, and treaty bodies that formulate and administer the legal framework.
 *   - Art market dealers (excluded/powerful): Commercial actors excluded from state-centric restitution negotiations but affected by provenance requirements.
 *   - Academic archaeologists (observer/organized): Research community divided between access interests and ethical commitments to source communities.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.55).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.45).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Reading of Cultural Property Legal Corpus").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, '6e089032-745f-43ad-8ce7-6f20c361e515').
narrative_ontology:cs_kernel_codification('6e089032-745f-43ad-8ce7-6f20c361e515', formalized).
narrative_ontology:cs_authority_grounding('6e089032-745f-43ad-8ce7-6f20c361e515', lineage).
narrative_ontology:cs_interpretation_layer_present('6e089032-745f-43ad-8ce7-6f20c361e515').
narrative_ontology:cs_reading_relation('6e089032-745f-43ad-8ce7-6f20c361e515', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e089032-745f-43ad-8ce7-6f20c361e515', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('6e089032-745f-43ad-8ce7-6f20c361e515', foundational, colonial_acquisition_void_ab_initio).
narrative_ontology:cs_axiom_status(colonial_acquisition_void_ab_initio, holdable).
narrative_ontology:cs_axiom_grounding('6e089032-745f-43ad-8ce7-6f20c361e515', colonial_acquisition_void_ab_initio, conventional).
narrative_ontology:cs_axiom('6e089032-745f-43ad-8ce7-6f20c361e515', foundational, successor_state_continuity_confers_title).
narrative_ontology:cs_axiom_status(successor_state_continuity_confers_title, holdable).
narrative_ontology:cs_axiom_grounding('6e089032-745f-43ad-8ce7-6f20c361e515', successor_state_continuity_confers_title, conventional).
narrative_ontology:cs_reference_frame('6e089032-745f-43ad-8ce7-6f20c361e515', post_colonial_state_sovereignty).
narrative_ontology:cs_drift_state('6e089032-745f-43ad-8ce7-6f20c361e515', contemporary_indigenous_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6e089032-745f-43ad-8ce7-6f20c361e515', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, state_sovereignty_over_cultural_property).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_acquisition_illegitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim historical continuity with pre-colonial polities and assert sovereign ownership over cultural artifacts removed during colonial periods. Receive returned artifacts, symbolic capital, and international legal recognition of their historical narratives. Leverage UNESCO conventions, UNIDROIT, and bilateral treaties to secure restitution.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, beneficiary,
    institutional, generational, arbitrage, global).

% Museums and cultural institutions in former colonial powers that hold disputed collections. Face legal, diplomatic, and reputational pressure to deaccession and return artifacts. Bear direct costs of restitution, shipping, insurance, and loss of research prestige and visitor revenue associated with retained collections.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions, payer,
    institutional, generational, constrained, global).

% Communities with direct cultural continuity to the artifacts but limited standing in international frameworks that privilege sovereign state actors. May see artifacts returned to a successor state they do not recognize as legitimate, or see their own stewardship claims subordinated to state ownership. Have constrained ability to bypass state frameworks and deal directly with holding institutions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities, payer,
    organized, generational, constrained, national).

% UNESCO, UNIDROIT, and international treaty bodies that formulate conventions and adjudicate cultural property claims. Set the legal framework that prioritizes state-to-state restitution and mediates between claimant states and holding institutions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_legal_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Commercial actors who trade in cultural artifacts and benefit from unclear provenance and title chains. Structurally excluded from state-centric repatriation negotiations but materially affected by strengthened provenance requirements and restitution claims.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, art_market_dealers, excluded,
    powerful, biographical, arbitrage, global).

% Research community that studies the artifacts. Divided between scholarly commitments to preservation and access, and growing ethical commitments to source communities and restitution. Not a primary decision-maker in the legal framework.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, academic_archaeologists, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__sovereign_repatriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a state-centric legal framework for resolving colonial-era cultural property disputes, transferring custody from holding institutions to polities claiming historical continuity, and replacing unilateral retention with bilateral or multilateral restitution mechanisms.
% TRANSFER_FUNCTION: Moves physical custody and symbolic capital of cultural artifacts from museums and holding institutions in former colonial powers to successor states; transfers narrative authority over the object's meaning and legitimate ownership to the receiving state.
% ABSENT_VOICES: Indigenous communities and local heritage stewards who may possess stronger cultural continuity than the successor state but lack standing in international frameworks that privilege sovereign state actors; source communities that dispute the successor state's representational legitimacy.
% DISAPPEARANCE_RATIONALE: Without the sovereign repatriation framework, successor states would lose their primary legal instrument for claiming cultural property; holding institutions would face reduced diplomatic and legal pressure to deaccession; and the global regime would likely default toward possession-based or universal-heritage frameworks, reorganizing the distribution of cultural collections and symbolic authority.
% FOUNDING_PROBLEM: Colonial powers systematically removed cultural artifacts without consent, destroying local heritage continuity and concentrating cultural capital and interpretive authority in imperial metropoles; post-colonial international law required a mechanism to redress this historical extraction and restore cultural continuity.
% FOUNDING_PROBLEM_CORROBORATION: Post-colonial states and international legal historians corroborate the historical extraction. However, holding institutions and indigenous rights advocates contest whether the successor state is the appropriate restorative agent; external indigenous scholars and critical museum studies researchers attest that the founding problem has mutated into a new asymmetry where state claims override community self-determination.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the constraint generates real restitution of historically taken artifactsâgenuine coordination toward post-colonial justiceâwhile simultaneously concentrating symbolic and political capital in successor states and imposing significant deaccession costs on holding institutions. Suppression (0.45) reflects that alternative frameworks (universal heritage, indigenous stewardship) remain intellectually and institutionally live; they are debated but partly marginalized by the state-centrism of international law. Theater ratio (0.25) is relatively low because the legal instruments produce material transfers, though some state claims are performative gestures toward national identity rather than community restoration. Accessibility collapse (0.40) is incomplete: museums still retain much material, and indigenous stewardship models are gaining traction. Resistance (0.55) is substantial, coming from holding institutions defending collection integrity, and increasingly from indigenous critics who reject state succession as a proxy for their own claims.
 *
 * PERSPECTIVAL GAP:
 *   The successor state seat experiences the constraint as restorative justice and historical correction: it computes as a Rope or low-extraction Tangled Rope. The holding institution seat experiences it as expropriation of legitimately held scholarly collections: it computes as Snare or high-extraction Tangled Rope. The indigenous community seat experiences a bifurcated structure: if the successor state genuinely represents them, the constraint approaches Rope; if the state captures the artifacts and excludes community stewardship, it computes as Snare. The engine produces these divergences from the same structural data because directionality differs by seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states are structural beneficiaries (d near 0.0): they receive artifacts, symbolic capital, and international legal recognition of their historical narratives. Holding institutions are structural targets (d near 1.0): they bear deaccession costs, research disruption, and reputational damage, with constrained exit because international law increasingly recognizes repatriation claims. Indigenous communities are also high-d targets (d near 0.8): while they may receive indirect benefit, their self-determination is subordinated to state sovereignty, and their exit options are constrained by the state-centrism of the legal framework. International legal bodies sit near symmetric (d ~0.5) as agenda-setters who do not personally collect the transfer but administer it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the coordination function (restoring colonially extracted heritage) as pure extraction by acknowledging that successor states genuinely solve a collective-action problem: without a state-centric legal framework, holding institutions would face fragmented, case-by-case pressure without clear title transfer rules. However, it prevents mislabeling the arrangement as pure coordination by identifying the asymmetric extraction: holding institutions lose assets, and indigenous communities may be displaced as the rightful stewards. If the founding problem (colonial extraction) were fully solved and the arrangement persisted merely to accumulate state symbolic capital, it would degrade toward Piton or Snare; the contested founding_problem_status and moderate theater_ratio suggest it is still in the Tangled Rope phase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    successor_state_representativeness,
    'Does the successor state genuinely represent the expropriated people and their heritage interests, or does it reproduce colonial territorial logic by substituting state sovereignty for community stewardship?',
    'Comparative case studies of repatriation outcomes where the successor state delegates custody to communities versus retains central control; indigenous community testimony on whether state reception of artifacts restored their agency.',
    'If successor states routinely fail to represent community interests, the constraint''s beneficiary structure shifts from restorative justice to state identity-capital accumulation, increasing extractiveness toward indigenous communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_state_representativeness, empirical, 'Whether post-colonial state succession is a legitimate proxy for indigenous cultural continuity.').

omega_variable(
    international_law_enforcement_character,
    'Is the sovereign repatriation framework enforced through binding legal obligation or through moral and diplomatic pressure that functions as weak coordination?',
    'Quantitative analysis of repatriation case outcomes: proportion secured via court order versus bilateral negotiation versus voluntary museum policy; correlation with state power asymmetries.',
    'If enforcement is primarily diplomatic rather than legal, the constraint''s extraction may be overstated for powerful holding institutions and understated for weak successor states, altering the directionality profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_enforcement_character, empirical, 'Whether repatriation relies on binding law or diplomatic coordination.').

omega_variable(
    indigenous_standing_exclusion,
    'Are indigenous communities structurally excluded from the sovereign repatriation framework by the state-centrism of international law, or have they internalized state mediation as the legitimate pathway for cultural recovery?',
    'Survey of indigenous participation rates in UNESCO and UNIDROIT proceedings; analysis of cases where indigenous communities bypassed states to deal directly with holding institutions.',
    'If exclusion is structural, the constraint''s suppression score should be higher and indigenous communities classified as trapped or constrained targets. If internalized, the effective extraction persists even where formal standing is granted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_standing_exclusion, conceptual, 'Whether indigenous exclusion from international cultural property law is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cult_tr_t10, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(cult_tr_t20, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(cult_tr_t50, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(cult_be_t10, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(cult_be_t20, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(cult_be_t50, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 50, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(cultural_property_legal_corpus__sovereign_repatriation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% The cultural_property_legal_corpus kernel decomposes into three structurally distinct constraints because the colloquial label 'cultural property law' conflates incompatible normative premises: state sovereignty, universal heritage, and communal stewardship. Each reading has a different Îµ, different beneficiaries, and different failure modes. This story (sovereign_repatriation_reading) is linked to its siblings as mutually influencing constraints within the same legal-discursive field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
