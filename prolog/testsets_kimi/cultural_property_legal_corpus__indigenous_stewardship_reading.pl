% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Indigenous Stewardship Authority over Cultural Artifacts
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint instantiates the indigenous_stewardship_reading of the
 *   cultural_property_legal_corpus kernel. It asserts that cultural artifacts
 *   are sacred or communal property whose legitimate authority rests solely
 *   with indigenous communities maintaining cultural continuity, explicitly
 *   denying legitimacy to colonial successor states and museums. Under this
 *   reading, indigenous communities are the structural beneficiaries of
 *   repatriation and stewardship authority, while holding institutions and
 *   successor states are the targets of extraction, bearing the costs of
 *   restitution and lost curatorial control. The reading carries the highest
 *   epsilon in the kernel because it allocates authority to the party that
 *   currently holds the least institutional power, requiring the most active
 *   displacement of incumbent property regimes.
 *
 * KEY AGENTS:
 *   - indigenous_communities: Primary beneficiary (organized/identity_locked) â gains stewardship authority and physical repatriation
 *   - holding_institutions: Primary target (institutional/constrained) â bears costs of deaccession and loss of collection control
 *   - colonial_successor_states: Secondary target (institutional/constrained) â loses sovereign patrimony claims and tourism/revenue
 *   - un_mechanisms: Agenda-setting observer (institutional/analytical) â facilitates norm propagation without direct extraction
 *   - universal_heritage_advocates: Excluded voice (organized/constrained) â argues for universal access but marginalized in this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.72).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Indigenous Stewardship Authority over Cultural Artifacts").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '90eeb106-eec0-44a7-b402-33d7d2367b3f').
narrative_ontology:cs_kernel_codification('90eeb106-eec0-44a7-b402-33d7d2367b3f', formalized).
narrative_ontology:cs_authority_grounding('90eeb106-eec0-44a7-b402-33d7d2367b3f', practice).
narrative_ontology:cs_interpretation_layer_present('90eeb106-eec0-44a7-b402-33d7d2367b3f').
narrative_ontology:cs_reading_relation('90eeb106-eec0-44a7-b402-33d7d2367b3f', cultural_property_legal_corpus__universal_heritage_reading, influences).
narrative_ontology:cs_reading_relation('90eeb106-eec0-44a7-b402-33d7d2367b3f', cultural_property_legal_corpus__sovereign_repatriation_reading, forecloses).
narrative_ontology:cs_axiom('90eeb106-eec0-44a7-b402-33d7d2367b3f', foundational, indigenous_cultural_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_cultural_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('90eeb106-eec0-44a7-b402-33d7d2367b3f', indigenous_cultural_sovereignty, deontological).
narrative_ontology:cs_axiom('90eeb106-eec0-44a7-b402-33d7d2367b3f', foundational, colonial_derivation_nullity).
narrative_ontology:cs_axiom_status(colonial_derivation_nullity, holdable).
narrative_ontology:cs_axiom_grounding('90eeb106-eec0-44a7-b402-33d7d2367b3f', colonial_derivation_nullity, conventional).
narrative_ontology:cs_reference_frame('90eeb106-eec0-44a7-b402-33d7d2367b3f', indigenous_cultural_continuity_framework).
narrative_ontology:cs_drift_state('90eeb106-eec0-44a7-b402-33d7d2367b3f', post_colonial_succession, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('90eeb106-eec0-44a7-b402-33d7d2367b3f', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert sacred and communal ownership of cultural artifacts; seek physical repatriation and ongoing curatorial authority. Their cultural, ceremonial, and linguistic continuity is inseparable from control over these objects, making exit equivalent to cultural dissolution.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, beneficiary,
    organized, generational, identity_locked, global).

% Museums, universities, and private collections that hold artifacts acquired during colonial eras. Under this reading they lack legitimate authority and face demands for physical restitution, bearing costs of deaccession, loss of exhibition revenue, and diminished institutional prestige.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, holding_institutions, payer,
    institutional, generational, constrained, global).

% Nation-states that inherited colonial territorial claims and assert sovereign patrimony over cultural property within their borders. This reading denies their legitimacy, imposing costs of lost national heritage claims, tourism revenue, and legal title.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, payer,
    institutional, civilizational, constrained, national).

% International bodies such as the UN Permanent Forum on Indigenous Issues and UNESCO mechanisms that develop and promote legal frameworks recognizing indigenous cultural rights. They facilitate norm propagation and monitoring without directly collecting or paying extraction.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, un_mechanisms, agenda_setter,
    institutional, generational, analytical, global).

% Scholars, museum professionals, and institutions arguing that cultural artifacts belong to humanity as a whole and should be preserved for universal access regardless of geographic origin. Their priority of decontextualized preservation is marginalized in this reading in favor of community-bound stewardship.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_heritage_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the allocation of cultural authority and physical stewardship to communities maintaining continuity, resolving the post-colonial vacuum of legitimate ownership by centering indigenous normative systems over successor-state or universal-institutional frameworks.
% TRANSFER_FUNCTION: Moves physical artifacts, curatorial authority, and legal title from colonial successor states and holding institutions to indigenous communities, transferring the power to withhold or grant access and the economic value of associated cultural tourism.
% ABSENT_VOICES: Universal heritage advocates who argue for decontextualized preservation and global public access; private collectors and market intermediaries operating outside institutional frameworks; museum visitors whose access may be restricted by community protocols after repatriation.
% DISAPPEARANCE_RATIONALE: If indigenous stewardship authority vanished overnight, museum and state holdings would revert to uncontested sovereign or institutional control, repatriation claims would lose their primary legal and moral foundation, and the global cultural property regime would reorganize around state or universal-access paradigms.
% FOUNDING_PROBLEM: Colonial extraction of cultural artifacts severed sacred and communal bonds, leaving indigenous communities dispossessed of heritage necessary for ceremonial, linguistic, and social continuity; successor states and museums inherited these holdings without legitimate authority under indigenous normative systems.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous communities and post-colonial legal scholars attest the problem is live and ongoing. Successor states and holding institutions often attest the problem is historically resolved through acquisition statutes and preservation duties; no independent neutral party exists outside the beneficiary and payer sets, though some international human rights bodies partially corroborate the indigenous framing.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88) because the constraint systematically strips legally recognized property rights from museums and states without compensation, reallocating them to communities that previously held no formal title under Western legal frameworks. Suppression is substantial (0.72) because the constraint's operation depends on overriding long-standing property law, museum trust law, and sovereign immunity through active legal and political campaigns. Theater ratio is moderate (0.35): while many repatriation claims remain legally unenforced or stalled in bureaucratic process, the underlying coordination function (restoring sacred continuity) is genuine and not merely cover. Resistance is high (0.78) because incumbent holders possess significant legal, financial, and political resources to resist deaccession. The temporal measurements run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter/beneficiary seat (indigenous communities) experiences this constraint as restorative coordination correcting historical extraction. The payer seats (museums, successor states) experience the same constraint as expropriation that threatens institutional viability, tourism economies, and legal predictability. The UN mechanisms seat sees it as normative reconciliation. These divergences are structural: identity-locked exit for communities (cultural survival is non-negotiable) versus constrained but materially costly exit for institutions (legal compliance or reputational damage).
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities are structural beneficiaries with low directionality: the constraint subsidizes their cultural continuity and authority. Holding institutions and successor states are structural targets with high directionality: the constraint extracts tangible assets, legal authority, and revenue from them. The UN mechanisms seat sits near analytical (d â 0.5) because it coordinates without direct cost or benefit. Universal heritage advocates are excluded from the constraint's beneficiary structure and would experience high directionality if included.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misreading this constraint as pure extraction (snare) because the coordination function â repairing broken cultural continuity and restoring sacred stewardship â is structurally genuine and not a cover story. Conversely, preventing misreading as rope prevents ignoring the severe, asymmetric extraction imposed on incumbent holders. The temporal measurements show extraction deepening as legal frameworks mature, indicating the coordination function did not atrophy into a piton; the mandate remains live but increasingly enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'How would the classification change if this constraint were read through the universal_heritage or sovereign_repatriation sibling readings of the same kernel?',
    'Compare sibling constraint stories for the same corpus; divergence in beneficiary/victim sets and epsilon values measures the kernel''s structural instability across readings.',
    'If the kernel is fundamentally contested, no single reading achieves stable mountain or rope status; persistent type oscillation across readings indicates the corpus is a political battleground rather than a settled coordination mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame uncertainty about structural delta across sibling readings').

omega_variable(
    enforcement_dependency_on_state_apparatus,
    'Does indigenous stewardship authority depend on the same state and international legal apparatus it denies legitimacy to?',
    'Empirical tracking of repatriation cases: where indigenous communities succeed without state enforcement (direct museum negotiation) versus state-compelled return.',
    'If enforcement always routes through states, the reading''s core premise is operationally contradicted, suggesting high theater ratio and potential piton drift; if communities enforce directly, extraction profile aligns with the tangled rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_dependency_on_state_apparatus, empirical, 'Whether legitimate community authority is self-executing or state-mediated').

omega_variable(
    cultural_continuity_operationalization,
    'How is cultural continuity operationalized and verified in repatriation proceedings, and who gatekeeps that standard?',
    'Ethnographic and legal review of continuity tests in national repatriation tribunals and UN mechanisms.',
    'If continuity tests are administered by non-indigenous institutions, the constraint replicates extraction through definitional control; if self-certifying, the extraction profile shifts toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_continuity_operationalization, conceptual, 'Who controls the legitimacy boundary of cultural continuity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cplc_isr_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.6).
narrative_ontology:measurement(cplc_isr_tr_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(cplc_isr_tr_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(cplc_isr_tr_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(cplc_isr_tr_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(cplc_isr_tr_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(cplc_isr_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cplc_isr_be_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(cplc_isr_be_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(cplc_isr_be_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(cplc_isr_be_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(cplc_isr_be_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 50, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(cplc_isr_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(cplc_isr_su_t10, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(cplc_isr_su_t20, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(cplc_isr_su_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(cplc_isr_su_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(cplc_isr_su_t50, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, identity_coordination).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, sovereign_repatriation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the cultural_property_legal_corpus kernel; sibling readings instantiate rival authority allocations (universal heritage, sovereign repatriation) from the same underlying legal and normative debates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
