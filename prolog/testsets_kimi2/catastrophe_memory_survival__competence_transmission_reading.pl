% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual Encoding of Practical Survival Knowledge (Competence Transmission Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story instantiates the competence_transmission_reading of
 *   the catastrophe_memory_survival kernel. The standing arrangement under
 *   contest is the ritual system's claim to encode and transmit practical
 *   survival knowledgeâtiming, resource management, family protocols,
 *   adaptation strategies. From this reading, the constraint coordinates
 *   genuine survival-relevant information for diaspora communities who
 *   extract adaptive capacity from ritual performance, while form-bound
 *   communities bear the cost of maintaining ritual form after the practical
 *   content has been lost or rendered obsolete by environmental change and
 *   literacy. The sibling readings are symbol_survival_reading (identity
 *   continuity as the primary function) and hybrid_encoding_reading
 *   (dual-register symbolic and practical operation). This reading is clean
 *   and Îµ-invariant: it does not average across readings or hedge across the
 *   kernel contest.
 *
 * KEY AGENTS:
 *   - diaspora_communities: Primary beneficiary (organized/global/constrained) â receives practical adaptive capacity encoded in ritual
 *   - form_bound_communities: Primary payer (organized/regional/identity_locked) â maintains ritual form without receiving practical survival content
 *   - ritual_specialists: Agenda setter (organized/regional/constrained) â administers ritual and controls interpretation of encoded knowledge
 *   - academic_observers: Analytical observer (institutional/global/analytical) â empirically assesses whether practical knowledge is actually transmitted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual Encoding of Practical Survival Knowledge (Competence Transmission Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, '66a4a3c9-2c92-4417-887a-5493c42a926a').
narrative_ontology:cs_kernel_codification('66a4a3c9-2c92-4417-887a-5493c42a926a', distributed).
narrative_ontology:cs_authority_grounding('66a4a3c9-2c92-4417-887a-5493c42a926a', practice).
narrative_ontology:cs_interpretation_layer_present('66a4a3c9-2c92-4417-887a-5493c42a926a').
narrative_ontology:cs_reading_relation('66a4a3c9-2c92-4417-887a-5493c42a926a', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('66a4a3c9-2c92-4417-887a-5493c42a926a', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('66a4a3c9-2c92-4417-887a-5493c42a926a', foundational, ritual_encodes_actionable_competence).
narrative_ontology:cs_axiom_status(ritual_encodes_actionable_competence, holdable).
narrative_ontology:cs_axiom_grounding('66a4a3c9-2c92-4417-887a-5493c42a926a', ritual_encodes_actionable_competence, empirically_contingent).
narrative_ontology:cs_axiom('66a4a3c9-2c92-4417-887a-5493c42a926a', foundational, adaptive_function_primary_over_identity).
narrative_ontology:cs_axiom_status(adaptive_function_primary_over_identity, holdable).
narrative_ontology:cs_axiom_grounding('66a4a3c9-2c92-4417-887a-5493c42a926a', adaptive_function_primary_over_identity, instrumental).
narrative_ontology:cs_reference_frame('66a4a3c9-2c92-4417-887a-5493c42a926a', practical_competence_continuity).
narrative_ontology:cs_drift_state('66a4a3c9-2c92-4417-887a-5493c42a926a', contemporary_literate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66a4a3c9-2c92-4417-887a-5493c42a926a', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, form_bound_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive practical survival knowledge encoded in ritual performanceâtiming, resource management, family protocols, adaptation strategiesâduring dispersal and resettlement. This transmission provides adaptive capacity in unfamiliar environments. Exit from the ritual system is possible but risks losing access to embedded knowledge that is not replicated in explicit institutional forms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% Maintain ritual form, performance obligations, and intergenerational transmission infrastructure in ancestral environments. The practical survival content once embedded in these rituals has been lost or rendered obsolete by environmental change and literacy, yet the community continues to bear the labor, time, and social cost of ritual maintenance. Exit would require abandoning a fused collective identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, form_bound_communities, payer,
    organized, generational, identity_locked, regional).

% Administer ritual performance, adjudicate correct practice, and control the authoritative interpretation of what the ritual encodes. Their status and role continuity depend on maintaining the claim that practical knowledge is present and transmissible. They enforce form and sanction deviation, but do not personally capture the primary extraction surplus.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_specialists, agenda_setter,
    organized, generational, constrained, regional).

% Study ritual systems ethnographically and experimentally to determine whether practical survival knowledge is actually transmitted or retrospectively projected. Their findings can corroborate or undermine the competence-transmission claim, but they do not participate in ritual maintenance or benefit from its persistence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, academic_observers, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves and transmits practical survival knowledgeâtiming, resource management, family protocols, adaptation strategiesâacross catastrophic disruptions and dispersal events without reliance on literacy or stable institutions.
% TRANSFER_FUNCTION: Moves practical knowledge and adaptive heuristics from the ritual system and its maintaining communities to diaspora communities; moves maintenance labor, performance cost, and identity-preservation burden from form-bound communities to the ritual system itself.
% ABSENT_VOICES: Explicit-knowledge institutions (modern education, disaster-management bureaucracies) that do not rely on ritual encoding; reformist voices within form-bound communities who recognize content loss but lack authority to alter ritual form; communities that abandoned ritual and rely entirely on formal knowledge transmission.
% DISAPPEARANCE_RATIONALE: If the ritual system vanished overnight, diaspora communities would lose a source of embedded adaptive heuristics tuned to disruption; form-bound communities would shed costly maintenance obligations but also lose identity structure; the distribution of survival knowledge across oral, disrupted populations would reorganize around explicit institutional memory.
% FOUNDING_PROBLEM: Catastrophe-induced dispersal and infrastructure collapse in pre-literate or marginally literate societies, requiring reliable transmission of actionable knowledge across generations without written records or stable political institutions.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and disaster-studies researchers outside the benefiting diaspora communities attest that some ritual systems encode empirically testable practical knowledge. Form-bound communities attest to the ongoing cost of maintenance but do not corroborate that practical benefits still flow to them; their testimony supports the dead-problem reading.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) because the asymmetry is real but bounded: diaspora communities receive genuine coordination benefits, while form-bound communities pay maintenance costs for hollowed form. Suppression is moderate (0.48) because ritual persistence relies on social sanction, identity fusion, and ritual-specialist authority rather than explicit violence. Theater_ratio is moderate (0.45) and rising: as practical content degrades over the interval, ritual performance increasingly becomes symbolic maintenance without operational survival function. Accessibility_collapse is elevated (0.62) because alternatives (explicit institutional knowledge, literacy, formal disaster management) exist but are culturally and identity-wise inaccessible to form-bound communities. Resistance is low-moderate (0.32) because reform voices within form-bound communities are marginalized by ritual authorities and identity-lock dynamics. All measurement series share a single time grid to prevent misaligned temporal sampling.
 *
 * PERSPECTIVAL GAP:
 *   The diaspora seat should compute as rope-leaning: it experiences genuine coordination (receiving adaptive knowledge) with manageable extraction. The form-bound seat should compute as snare-leaning: it experiences identity-locked maintenance of a form that no longer delivers the practical benefit that justifies the cost. The ritual_specialist seat is near symmetric or mildly beneficiary (status and role continuity). The engine's per-seat classification will diverge accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities are declared beneficiaries (low d, subsidized by the constraint's knowledge transmission). Form-bound communities are declared victims/payers (high d, extraction target). Ritual specialists are agenda_setters with constrained exit; they capture status but not the primary extraction surplus. Academic observers are analytical with arbitrage-grade exit (analytical). The directionality derivation from beneficiary/victim declarations plus exit options produces the correct d for all seats without override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâcatastrophic memory loss in pre-literate societiesâis contested. In literate, institutionally complex societies, explicit knowledge storage has partly superseded ritual transmission. However, the arrangement persists because form-bound communities are identity-locked to ritual performance, and diaspora communities continue to draw adaptive heuristics from it. This prevents mislabeling as pure rope (because extraction is asymmetric and the founding problem is partially dead) or pure snare (because genuine coordination benefits flow to diaspora communities). The classification as tangled_rope captures the hybrid reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    content_form_separability,
    'Can the practical survival knowledge encoded in ritual be separated from ritual form and transmitted explicitly without loss of efficacy?',
    'Comparative disaster-resilience studies tracking communities with explicit institutional knowledge versus ritual-only transmission; ethnographic experiments testing whether ritual-naive individuals can operationalize the knowledge when decoupled from performance.',
    'If separable, the ritual form is not structurally necessary for the coordination function, and much of the form-bound communities'' maintenance cost is pure extraction; if inseparable, the extraction is partly the necessary price of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_form_separability, empirical, 'Whether practical knowledge is structurally bound to ritual form').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the pressure on form-bound communities to maintain ritual structural (social sanction, ritual-specialist authority) or internalized (identity fusion making exit unthinkable)?',
    'Post-exit trajectory analysis: if suppression persists after structural sanctions are removed (e.g., migration to secular contexts), the mechanism is partially internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests, and the constraint''s extraction is carried by the target even after apparent exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in ritual maintenance').

omega_variable(
    kernel_reading_contest,
    'Does this reading''s empirical claimâthat ritual encodes actionable practical knowledgeâhold across the full population of rituals under the kernel, or only a subset?',
    'Corpus-wide ethnographic coding of ritual content for practical survival referents, separating instances with demonstrable operational content from instances with purely symbolic referents.',
    'If only a subset carries practical content, the competence_transmission_reading is a partial reading of the kernel, and the hybrid or symbol readings capture the remainder; if most carry practical content, the reading is broadly vindicated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Scope of the competence-transmission claim across the kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 60, 0.51).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.38).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 100, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_survival kernel, instantiating the practical-competence framing. Sibling readings instantiate symbolic-continuity and hybrid-dual-register framings. Each reading carries its own Îµ, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
