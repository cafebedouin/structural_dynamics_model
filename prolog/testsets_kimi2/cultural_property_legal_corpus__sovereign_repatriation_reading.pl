% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   human_readable: Sovereign Repatriation Reading of Cultural Property Corpus
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   This constraint instantiates the sovereign_repatriation_reading of the
 *   cultural_property_legal_corpus kernel. It treats cultural artifacts
 *   removed during colonial periods as sovereign property of successor states
 *   that claim historical continuity with expropriated peoples, framing
 *   colonial acquisition as illegitimate extraction and legitimizing
 *   state-led repatriation claims. The reading is contested by universal
 *   museums (who defend retention) and by indigenous communities (who argue
 *   authority should rest below the state level). The constraint carries
 *   moderate extractiveness: it genuinely coordinates a post-colonial
 *   transfer order, but asymmetrically extracts collection capital and
 *   scholarly control from universal museums.
 *
 * KEY AGENTS:
 *   - successor_states: Primary beneficiary and agenda-setter (institutional/global) â claims historical continuity and drives repatriation authority
 *   - universal_museums: Primary target (institutional/national) â bears loss of collections, research access, and symbolic capital
 *   - indigenous_communities: Excluded voice (powerless/local) â culturally continuous but bypassed by state-centric legal framework
 *   - unesco_committees: Analytical observer (institutional/global) â facilitates the framework but neither collects nor pays
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.52).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.6).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Reading of Cultural Property Corpus").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, '06ac6882-47e1-420d-9b36-c9bd2189b6ec').
narrative_ontology:cs_kernel_codification('06ac6882-47e1-420d-9b36-c9bd2189b6ec', formalized).
narrative_ontology:cs_authority_grounding('06ac6882-47e1-420d-9b36-c9bd2189b6ec', lineage).
narrative_ontology:cs_interpretation_layer_present('06ac6882-47e1-420d-9b36-c9bd2189b6ec').
narrative_ontology:cs_reading_relation('06ac6882-47e1-420d-9b36-c9bd2189b6ec', cultural_property_legal_corpus__universal_heritage_reading, influences).
narrative_ontology:cs_reading_relation('06ac6882-47e1-420d-9b36-c9bd2189b6ec', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('06ac6882-47e1-420d-9b36-c9bd2189b6ec', foundational, colonial_acquisition_vitiates_title).
narrative_ontology:cs_axiom_status(colonial_acquisition_vitiates_title, holdable).
narrative_ontology:cs_axiom_grounding('06ac6882-47e1-420d-9b36-c9bd2189b6ec', colonial_acquisition_vitiates_title, deontological).
narrative_ontology:cs_axiom('06ac6882-47e1-420d-9b36-c9bd2189b6ec', foundational, successor_state_continuity_authority).
narrative_ontology:cs_axiom_status(successor_state_continuity_authority, holdable).
narrative_ontology:cs_axiom_grounding('06ac6882-47e1-420d-9b36-c9bd2189b6ec', successor_state_continuity_authority, conventional).
narrative_ontology:cs_reference_frame('06ac6882-47e1-420d-9b36-c9bd2189b6ec', pre_colonial_sovereign_legitimacy).
narrative_ontology:cs_drift_state('06ac6882-47e1-420d-9b36-c9bd2189b6ec', contemporary_repatriation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('06ac6882-47e1-420d-9b36-c9bd2189b6ec', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_museums).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, state_continuity_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_title_vitiation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert sovereign authority over cultural artifacts removed during colonial periods based on claimed historical continuity with pre-colonial polities. Drive bilateral negotiations, UNESCO convention compliance, and public diplomacy to secure physical return of objects, thereby gaining symbolic national capital, heritage tourism potential, and control over historical narratives. Abandoning the framework is politically costly domestically and diplomatically.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, beneficiary).

% Hold encyclopedic collections acquired during colonial eras. Face escalating legal, diplomatic, and reputational pressure to deaccession and return artifacts. Compliance means loss of collection objects, associated research access, exhibition draw, and institutional prestige; resistance means risking sanction, protest, and funding cuts. The range of acceptable alternatives to return is narrowing under shifting international norms.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_museums, payer,
    institutional, generational, constrained, national).

% Maintain direct cultural, spiritual, and genealogical relationships with the artifacts, yet are structurally bypassed by state-to-state repatriation frameworks. Watch successor states claim sovereign authority over heritage that may originate with their specific communities, sometimes without subsequent transfer to community stewardship. Cannot readily exit the identity relationship to the artifacts even when the legal framework excludes them from title and custody decisions.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities, excluded,
    powerless, generational, identity_locked, local).

% Facilitate international conventions, monitor state compliance, and provide dispute-resolution forums for cultural property claims. Neither gain nor lose artifacts themselves; they maintain the normative and procedural infrastructure that translates the sovereign repatriation principle into bureaucratic action.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, unesco_committees, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a formal international mechanism for transferring custody of cultural artifacts from former colonial holding institutions to successor states, creating a negotiated order out of contested ownership claims that would otherwise default to possession-by-force.
% TRANSFER_FUNCTION: Moves physical artifacts, display rights, curatorial control, and symbolic national capital from encyclopedic museums in former colonial powers to successor states claiming historical continuity with expropriated peoples.
% ABSENT_VOICES: Indigenous communities and local cultural groups who maintain direct cultural continuity with the artifacts but are excluded from state-to-state bilateral negotiations and intergovernmental UNESCO processes; they would argue for community-level stewardship rather than state sovereignty over heritage.
% DISAPPEARANCE_RATIONALE: If the sovereign repatriation framework vanished, ongoing returns would halt, successor states would lose the primary legal and moral lever for reclaiming artifacts, museums would revert to retention defaults, and the post-colonial diplomatic order around cultural property would lose its central coordinating norm.
% FOUNDING_PROBLEM: Colonial powers removed cultural artifacts without legitimate title transfer, concentrating the material heritage of colonized societies in metropolitan museums and denying source societies access to their own cultural production.
% FOUNDING_PROBLEM_CORROBORATION: UN General Assembly resolutions and UNESCO conventions from mixed-membership bodies corroborate the foundational injustice; post-colonial legal scholars attest to its persistence. However, encyclopedic museums and some legal historians contest the retroactive application of modern norms, and indigenous communitiesâwho would corroborate from outside the state-beneficiary seatâare structurally excluded by this reading's state-centric frame.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.52, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.52) because the constraint moves substantial symbolic and material capital from museums to states, yet it also solves a genuine coordination problem (contested ownership without a war-of-all-against-all). Suppression (0.60) reflects the active legal and diplomatic machinery required to overcome museum resistance and retention defaults. Theater ratio (0.35) captures the growing performative dimension: some repatriation gestures serve diplomatic signaling more than sustained restitution, though real returns are occurring. Accessibility collapse (0.55) registers that museums' alternative legal defenses (statutes of limitations, acquisition good faith) are narrowing under normative pressure. Resistance (0.55) reflects organized museum opposition and reluctant state implementation. The metrics and claim are authored independently: the structural claim is tangled_rope, and the metrics describe observed operation without tuning toward any computed output.
 *
 * PERSPECTIVAL GAP:
 *   The successor-state seat experiences the constraint as restorative justice and legitimate coordination: it returns wrongfully taken heritage to its rightful sovereign. The universal-museum seat experiences the same structure as extraction of curated capital, research infrastructure, and institutional identity. The indigenous-community seat experiences exclusion from a framework that claims to correct injustice while replicating top-down authority. The engine computes these divergences from the same structural data via directionality and exit-option asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Successor_states are declared beneficiaries with mobile exit options (they can modulate which claims to press), yielding low directionality and damped effective extraction. Universal_museums are declared victims with constrained exit options, yielding high directionality and amplified effective extraction. Indigenous_communities are not declared in either beneficiary or victim arrays at the base_properties layer because this reading structurally excludes them from the legal relationship; their exclusion is documented in absent_voices and the excluded role rather than routed through chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcolonial dispossessionâis contested but not dead. Successor_states actively benefit and pursue enforcement, and universal_museums actively resist, which prevents a piton diagnosis. If the problem were dead (all artifacts returned or claims abandoned) and the framework persisted purely as bureaucratic theater, the constraint would drift toward piton. Currently, the live contest between coordination and extraction sustains the tangled_rope classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_successor_legitimacy,
    'Do post-colonial successor states genuinely represent the expropriated peoples whose cultural heritage they claim, or do state boundaries and institutions replicate colonial constructions that may not align with community-level continuity?',
    'Comparative political analysis of state-versus-community representativeness in repatriation cases; empirical tracking of whether repatriated artifacts remain with state museums or devolve to communities.',
    'If successor states do not represent expropriated communities, this reading''s beneficiary structure is misaligned with its moral premise, potentially shifting classification toward extraction by state elites at community expense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_successor_legitimacy, empirical, 'Whether state continuity claims align with actual community representation.').

omega_variable(
    kernel_reading_decomposition,
    'This constraint is the sovereign_repatriation_reading of cultural_property_legal_corpus. How would the classification change if the indigenous_stewardship_reading or universal_heritage_reading were adopted instead?',
    'Cross-reading comparison: the indigenous reading would shift beneficiaries from states to communities and likely increase extractiveness from state museums; the universal reading would eliminate the extraction vector by distributing authority to cosmopolitan institutions regardless of origin.',
    'The sibling readings demonstrate that this kernel''s epsilon is reading-dependent; this reading''s moderate epsilon depends on state-centric framing that may be contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Structural delta across sibling readings of the same kernel.').

omega_variable(
    repatriation_cost_symmetry,
    'Are the costs of repatriation (shipping, insurance, research loss, display revenue) borne asymmetrically by holding institutions, or do successor states bear hidden costs (conservation infrastructure, diplomatic capital, legal fees)?',
    'Comparative cost accounting across bilateral repatriation agreements and longitudinal budget analysis of receiving state heritage ministries.',
    'If costs are symmetric, directionality shifts toward 0.5 and the constraint looks more like coordination; if asymmetric on the museum side, the extraction reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(repatriation_cost_symmetry, empirical, 'Symmetry of costs in repatriation transfers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cplcsr_tr_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cplcsr_tr_t10, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(cplcsr_tr_t20, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cplcsr_tr_t30, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(cplcsr_tr_t40, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(cplcsr_tr_t50, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(cplcsr_be_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cplcsr_be_t10, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(cplcsr_be_t20, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(cplcsr_be_t30, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(cplcsr_be_t40, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 40, 0.47).
narrative_ontology:measurement(cplcsr_be_t50, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cplcsr_su_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cplcsr_su_t10, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(cplcsr_su_t20, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(cplcsr_su_t30, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(cplcsr_su_t40, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(cplcsr_su_t50, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the cultural_property_legal_corpus kernel, decomposed per the epsilon-invariance principle because the sovereign_repatriation_reading, universal_heritage_reading, and indigenous_stewardship_reading carry different epsilon values, beneficiary structures, and directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
