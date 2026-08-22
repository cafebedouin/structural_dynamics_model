% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Substrate
 *   domain: constitutional/law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the living document reading of Magna Carta
 *   (kernel: magna_carta_1215). It treats the charter not as a fixed feudal
 *   contract but as an adaptive constitutional substrate whose original
 *   meaning is legitimately superseded by interpretive tradition and
 *   precedential accumulation. The constraint operates as a meta-constraint
 *   on interpretive authority: it scaffolds contestation between the baronial
 *   privilege reading and the universal rights reading without resolving
 *   their tension, incorporating both into an evolving tradition. This
 *   reading coexists with its siblings within broader legal discourse but
 *   structurally privileges the common law judiciary and legal profession as
 *   the agents of constitutional development.
 *
 * KEY AGENTS:
 *   - common_law_judiciary (agenda_setter/beneficiary): institutional power, constrained exit â administers precedential accumulation and derives authority from interpretive supremacy.
 *   - legal_profession (beneficiary): organized, constrained exit â derives professional identity and economic sustenance from the tradition's complexity.
 *   - originalist_interpreters (payer): organized, constrained exit â bear the cost of methodological marginalization.
 *   - formal_amendment_advocates (payer): moderate, constrained exit â bear the cost of democratic bypass.
 *   - feudal_contract_historians (excluded): moderate, constrained exit â structurally absent from constitutional practice despite historical accuracy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.55).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.45).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Substrate").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional/law/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, 'e630b59e-519c-4c7a-8952-32b1e5f83399').
narrative_ontology:cs_kernel_codification('e630b59e-519c-4c7a-8952-32b1e5f83399', fixed_text).
narrative_ontology:cs_authority_grounding('e630b59e-519c-4c7a-8952-32b1e5f83399', lineage).
narrative_ontology:cs_interpretation_layer_present('e630b59e-519c-4c7a-8952-32b1e5f83399').
narrative_ontology:cs_reading_relation('e630b59e-519c-4c7a-8952-32b1e5f83399', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('e630b59e-519c-4c7a-8952-32b1e5f83399', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('e630b59e-519c-4c7a-8952-32b1e5f83399', foundational, original_meaning_legitimately_superseded).
narrative_ontology:cs_axiom_status(original_meaning_legitimately_superseded, holdable).
narrative_ontology:cs_axiom_grounding('e630b59e-519c-4c7a-8952-32b1e5f83399', original_meaning_legitimately_superseded, conventional).
narrative_ontology:cs_axiom('e630b59e-519c-4c7a-8952-32b1e5f83399', foundational, precedential_accumulation_constitutes_development).
narrative_ontology:cs_axiom_status(precedential_accumulation_constitutes_development, holdable).
narrative_ontology:cs_axiom_grounding('e630b59e-519c-4c7a-8952-32b1e5f83399', precedential_accumulation_constitutes_development, instrumental).
narrative_ontology:cs_reference_frame('e630b59e-519c-4c7a-8952-32b1e5f83399', common_law_adaptive_tradition).
narrative_ontology:cs_drift_state('e630b59e-519c-4c7a-8952-32b1e5f83399', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e630b59e-519c-4c7a-8952-32b1e5f83399', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legal_profession).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, originalist_interpreters).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, formal_amendment_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers precedential accumulation and interprets Magna Carta clauses as evolved constitutional principles; bound by stare decisis yet possesses authority to distinguish, extend, and reshape doctrine. Derives institutional power and legitimacy from being the primary seat of interpretive supersession.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, common_law_judiciary, beneficiary).

% Derives professional authority, training curricula, and economic sustenance from the complexity and continuity of the interpretive tradition; membership and advancement depend on fluency in precedential reasoning and evolutionary constitutional narrative.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Advocate for fixed original meaning and textual constraint; structurally marginalized within the common law tradition because their methodological commitment is treated as historically interesting but constitutionally superseded by accumulated precedent.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_interpreters, payer,
    organized, generational, constrained, national).

% Hold that constitutional development should occur through formal democratic enactment rather than judicial interpretation; their preferred mechanism is bypassed by interpretive evolution, leaving them with advocacy but limited institutional traction.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, formal_amendment_advocates, payer,
    moderate, generational, constrained, national).

% Historians who emphasize the baronial privilege and feudal contract reading of Magna Carta; their historical account is acknowledged in scholarship but treated as constitutionally irrelevant in contemporary legal practice, excluding them from the interpretive conversation.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, feudal_contract_historians, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__living_document_reading, common_law_judiciary).
narrative_ontology:fixing_cost_class(magna_carta_1215__living_document_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional continuity and adaptation across centuries without requiring formal textual amendment; allows historical commitments to evolve in response to new political and social conditions through accumulated judicial interpretation.
% TRANSFER_FUNCTION: Moves interpretive authority from fixed textual meaning and democratic amendment processes to the common law judiciary and legal profession, who determine how historical constitutional texts apply to contemporary problems.
% ABSENT_VOICES: Feudal contract historians and strict textual formalists who would insist on the original baronial privilege scope or fixed amendment mechanisms are present in historical scholarship but structurally excluded from the living constitutional conversation; their objections are bracketed as historically accurate but legally non-binding.
% DISAPPEARANCE_RATIONALE: If the interpretive tradition vanished overnight, constitutional adjudication would lose its primary source of doctrinal evolution; courts would face a radical break in precedent, litigants would lose accumulated protections, and originalist and formalist methodologies would rush to fill the vacuum â the common law constitutional order would reorganize around textual fixity or democratic amendment.
% FOUNDING_PROBLEM: How to maintain constitutional continuity and adaptability across centuries when formal amendment is difficult or politically impossible, and written texts age beyond their original social context.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the benefiting judiciary attest that Magna Carta was reinterpreted repeatedly to serve new political needs across successive centuries; constitutional theorists from non-common-law traditions corroborate that the adaptability problem is genuine, though they note alternative solutions such as civil-law codification or popular constitutional amendment.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate because the living document reading genuinely coordinates constitutional adaptation but also concentrates interpretive authority in the judiciary and legal profession at the expense of fixed-meaning and democratic alternatives. Suppression (0.45) is moderate because originalist and formalist alternatives are not eliminated â they coexist as legitimate positions within discourse â but they are structurally disadvantaged in binding adjudication. Theater_ratio (0.25) is low-to-moderate because the coordination function is real and substantial, though some maintenance of continuity rhetoric is performative. Accessibility_collapse (0.5) reflects that once inside the common law tradition, textual-originalist alternatives seem legally inaccessible despite their intellectual coherence. Resistance (0.55) captures sustained originalist and democratic pushback against judicial supremacy in constitutional development. The measurement series tracks gradual extraction accumulation as the interpretive tradition matured from the early modern period to the present.
 *
 * PERSPECTIVAL GAP:
 *   The common law judiciary and legal profession experience this constraint as genuine coordination â a necessary mechanism for constitutional continuity and justice across centuries. The originalist and formal amendment seats experience it as asymmetric extraction â their preferred methodologies and institutions are overridden by a self-perpetuating interpretive class. The engine computes this divergence from the structural data: agenda_setter/beneficiary seats with constrained but institutional power derive low directionality, while payer seats with organized or moderate power and constrained exit derive high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The common law judiciary is the primary beneficiary (d near 0.0) because the constraint subsidizes its authority by legitimating interpretive supersession of fixed meaning. The legal profession is a secondary beneficiary (low d) because the tradition's complexity sustains their professional niche. Originalist interpreters are targets (high d) because the constraint extracts methodological legitimacy from them. Formal amendment advocates are targets (moderate-high d) because the constraint extracts democratic control over constitutional change. The feudal contract historians are excluded entirely, receiving no directional assignment because they are outside the conversation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the living document reading as pure coordination (rope) by requiring the identification of victims who bear the cost of interpretive supersession â originalists and formal amendment advocates. It also prevents mislabeling it as pure extraction (snare) by requiring an honest coordination function (constitutional adaptation across centuries) and acknowledging that enforcement is not total suppression but structural tilting. The tangled_rope classification is earned because the same precedential structure that coordinates adaptation also extracts authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_document_coordination_or_capture,
    'Does the living document reading of Magna Carta function primarily as a coordination mechanism for constitutional adaptation, or as a capture mechanism concentrating interpretive authority in the judiciary?',
    'Comparative analysis of jurisdictions with and without strong living-document traditions, measuring judicial power relative to legislative and popular mechanisms; historical analysis of whether interpretive evolution tracks societal needs or judicial class interests.',
    'If capture, the constraint reclassifies toward snare or tangled_rope with higher extraction; if genuine coordination, it moves toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_document_coordination_or_capture, conceptual, 'Coordination function versus extractive capture in living constitutionalism').

omega_variable(
    sibling_reading_subsumption,
    'Do the baronial privilege and universal rights readings genuinely coexist as independent positions within the living document framework, or are they reduced to historical footnotes within a progressive narrative controlled by the interpretive tradition?',
    'Discourse analysis of constitutional rhetoric and judicial opinions to measure whether sibling readings are cited as live authorities or merely as historical antecedents.',
    'If reduced to footnotes, the pluralism claim is theatrical and theater_ratio should rise; if genuinely coexisting, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_subsumption, conceptual, 'Whether sibling readings are genuinely coexistent or theatrically retained').

omega_variable(
    cs_framing_alternative,
    'Is the commitment system better framed as lineage (authority from Magna Carta text and its transmission) or as practice (authority from ongoing common law methods independent of the text)?',
    'Historical sociology of the legal profession to determine whether appeals to Magna Carta are necessary legitimating moves or optional rhetorical gestures.',
    'If practice, the kernel is less fixed_text and more implicit/distributed, changing the authority_grounding and potentially the cs_pattern classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_alternative, conceptual, 'Alternative CS framing under-determination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_living_tr_t0, magna_carta_1215__living_document_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mc_living_tr_t20, magna_carta_1215__living_document_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(mc_living_tr_t40, magna_carta_1215__living_document_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(mc_living_tr_t60, magna_carta_1215__living_document_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(mc_living_tr_t80, magna_carta_1215__living_document_reading, theater_ratio, 80, 0.25).

% Extraction over time
narrative_ontology:measurement(mc_living_be_t0, magna_carta_1215__living_document_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(mc_living_be_t20, magna_carta_1215__living_document_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(mc_living_be_t40, magna_carta_1215__living_document_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(mc_living_be_t60, magna_carta_1215__living_document_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(mc_living_be_t80, magna_carta_1215__living_document_reading, base_extractiveness, 80, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_1215__living_document_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% The Magna Carta 1215 kernel decomposes into three structurally distinct readings: baronial privilege (feudal contract), living document (adaptive substrate), and universal rights (transhistorical precedent). Each reading has a different epsilon, beneficiary structure, and classification. This story instantiates the living document reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
