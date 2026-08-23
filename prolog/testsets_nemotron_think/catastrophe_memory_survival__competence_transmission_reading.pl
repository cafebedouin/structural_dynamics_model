% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Ritual as Practical Survival Knowledge Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story models the competence_transmission reading of the
 *   catastrophe_memory_survival kernel: the claim that ritual's core survival
 *   function is encoding and transmitting practical knowledge (timing of
 *   migrations, resource management during scarcity, family protocols for
 *   dispersal, adaptation strategies for new environments). The reading
 *   posits a genuine coordination function — the ritual solves the problem of
 *   preserving actionable knowledge across generations and displacements —
 *   but also identifies asymmetric extraction: source communities that
 *   maintain the ritual form lose the practical content (becoming victims of
 *   formalization), while diaspora communities that receive the transmitted
 *   knowledge gain adaptive capacity (beneficiaries). The engine will compute
 *   per-seat classifications from this structural data; the claimed type
 *   (tangled_rope) is the author's structural judgment, independent of the
 *   metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.5).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.3).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Practical Survival Knowledge Transmission").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, '6ace31d4-8f06-473a-aa4e-01493914e5b0').
narrative_ontology:cs_kernel_codification('6ace31d4-8f06-473a-aa4e-01493914e5b0', distributed).
narrative_ontology:cs_authority_grounding('6ace31d4-8f06-473a-aa4e-01493914e5b0', practice).
narrative_ontology:cs_interpretation_layer_present('6ace31d4-8f06-473a-aa4e-01493914e5b0').
narrative_ontology:cs_reading_relation('6ace31d4-8f06-473a-aa4e-01493914e5b0', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('6ace31d4-8f06-473a-aa4e-01493914e5b0', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('6ace31d4-8f06-473a-aa4e-01493914e5b0', foundational, practical_knowledge_primary).
narrative_ontology:cs_axiom_status(practical_knowledge_primary, holdable).
narrative_ontology:cs_axiom_grounding('6ace31d4-8f06-473a-aa4e-01493914e5b0', practical_knowledge_primary, empirically_contingent).
narrative_ontology:cs_axiom('6ace31d4-8f06-473a-aa4e-01493914e5b0', foundational, ritual_as_epistemic_carrier).
narrative_ontology:cs_axiom_status(ritual_as_epistemic_carrier, holdable).
narrative_ontology:cs_axiom_grounding('6ace31d4-8f06-473a-aa4e-01493914e5b0', ritual_as_epistemic_carrier, conventional).
narrative_ontology:cs_reference_frame('6ace31d4-8f06-473a-aa4e-01493914e5b0', ritual_as_competence_transmission).
narrative_ontology:cs_drift_state('6ace31d4-8f06-473a-aa4e-01493914e5b0', contemporary_diaspora_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ace31d4-8f06-473a-aa4e-01493914e5b0', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, source_communities).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, ritual_encodes_practical_knowledge).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, diaspora_adaptation_depends_on_ritual_transmission).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They maintain the ritual form (ceremonies, calendars, oral formulas) but have lost the practical survival knowledge that the ritual once encoded. The ritual specialists are drawn from their midst, but the knowledge has become esoteric or lost. They bear the cost of maintaining the ritual complex (time, resources, cognitive load) without the adaptive benefit. Exit is constrained: abandoning the ritual means losing communal identity and cohesion, but maintaining it without the practical content is a net loss.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, source_communities, payer,
    moderate, generational, constrained, local).

% They receive the practical survival knowledge through the ritual (timing of migrations, resource management, family protocols, adaptation strategies) and use it to navigate new environments. The ritual is their primary vehicle for preserving this knowledge across generations because written records are vulnerable and oral transmission is fragile. They gain adaptive capacity without bearing the full maintenance cost of the ritual complex in the homeland. Exit is mobile: they can adopt alternative adaptation strategies (formal education, institutional support) if the ritual fails to transmit useful knowledge.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    moderate, generational, mobile, global).

% They are the custodians of the ritual, responsible for its performance, interpretation, and transmission. They set the agenda for what constitutes correct practice and what knowledge is encoded. Their authority depends on the ritual's perceived efficacy. Many specialists no longer possess the practical knowledge (it has become formulaic), but their identity and livelihood are fused with the ritual. Exit is identity_locked: leaving the role means abandoning their professional identity, communal status, and often their primary social framework.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, ritual_specialists, agenda_setter,
    organized, biographical, identity_locked, regional).

% They study the ritual as a cultural phenomenon, documenting the practical knowledge embedded in it and its transmission dynamics. They are not subject to the constraint's extraction or coordination — they analyze it from outside. Their work can influence the constraint by documenting the practical content (potentially aiding diaspora communities) or by framing the ritual as purely symbolic (reinforcing the symbol_survival reading).
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, anthropologists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The ritual solves the problem of transmitting practical survival knowledge (timing, resource management, family protocols, adaptation strategies) across generations and geographies, especially in diaspora contexts where written records may be lost and oral transmission is fragile.
% TRANSFER_FUNCTION: The ritual moves practical survival knowledge from source communities (who hold the knowledge but lose it through formalization and specialist mediation) to diaspora communities (who need it for adaptation), with ritual specialists as the transmission channel. The source communities pay the maintenance cost; the diaspora communities receive the adaptive benefit.
% ABSENT_VOICES: The source communities that have lost the practical content but maintain the form are often not heard in the discourse about the ritual's function; they are the ones who bear the cost of maintaining the ritual without the practical benefit. Their voices are absent because the ritual specialists (who control the narrative) and the diaspora communities (who benefit) both have incentives to emphasize the ritual's efficacy.
% DISAPPEARANCE_RATIONALE: If the ritual disappeared overnight, the transmission of practical survival knowledge would be disrupted, especially for diaspora communities that rely on it as a primary vehicle. Source communities would lose the remaining structure that preserves any residual knowledge and coordinates collective memory. The world would rearrange: new knowledge-transmission mechanisms would need to emerge, or adaptive capacity would decline.
% FOUNDING_PROBLEM: The need to preserve and transmit practical survival knowledge across generations and through displacement, when written records are vulnerable and oral transmission is fragile.
% FOUNDING_PROBLEM_CORROBORATION: Ethnographic studies of diaspora communities (e.g., Jewish, Armenian, African diasporas) show rituals encoding practical knowledge; the claim is corroborated by scholars outside the benefiting communities (e.g., anthropologists of ritual and memory such as Maurice Bloch, Jack Goody, and contemporary scholars of cultural transmission). The symbol_survival reading's proponents (e.g., theorists of ritual as identity maintenance) contest the primacy of practical content but do not deny its presence.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.5, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.5) because the ritual transfers practical knowledge from source communities (who lose it through formalization and specialization) to diaspora communities (who need it for adaptation). Suppression is low-moderate (0.3) because enforcement is primarily normative (community expectation, specialist authority) rather than coercive. Theater ratio is moderate (0.4) because a growing share of ritual performance is symbolic maintenance rather than practical transmission. Accessibility collapse is moderate (0.6) because alternative knowledge-transmission channels (written records, formal education) exist but are historically fragile in diaspora contexts. Resistance is low (0.2) because the ritual's survival value makes participants complicit in its maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The source_community payer seat experiences the constraint as extraction (they maintain the costly ritual form while losing the practical knowledge that justified it). The diaspora_community beneficiary seat experiences it as coordination (they receive survival knowledge they could not otherwise preserve). The ritual_specialist agenda_setter seat experiences it as both: they administer the transmission but may not themselves possess the practical knowledge, creating a structural gap between form and content. The engine computes this divergence from the declared roles, exit options, and power levels.
 *
 * DIRECTIONALITY LOGIC:
 *   Source communities are structural targets (d near 1.0): they bear the cost of maintaining the ritual complex while the practical knowledge decays. Their exit is constrained — they cannot abandon the ritual without losing identity and community cohesion. Diaspora communities are structural beneficiaries (d near 0.0): they receive the practical knowledge as a subsidy. Their exit is mobile — they can adopt alternative adaptation strategies. Ritual specialists are near-symmetric (d ~0.5): they gain status and authority from administering the ritual but bear the burden of preserving its form. Their exit is identity_locked — their professional and communal identity is fused with the ritual.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting practical survival knowledge across displacement) is live — diaspora communities still face adaptation challenges. However, the ritual's practical content has atrophied in many source communities, creating a mandatrophy risk: the arrangement persists as a coordination mechanism for identity (symbol_survival reading) while its original practical function degrades. The competence_transmission reading flags this as a tangled_rope because the coordination function (knowledge transmission) is real but the extraction (source communities losing content) is asymmetric. If the practical content fully decays, the constraint would drift toward snare (pure extraction via identity maintenance) or piton (theatrical maintenance of a dead function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_frame,
    'Is this constraint one reading of the contested kernel ''catastrophe_memory_survival'', and how do sibling readings change the structural classification?',
    'Compare the ε, beneficiary/victim structure, and coordination function across the three declared readings: competence_transmission, symbol_survival, and hybrid_encoding. The engine computes per-reading classifications; divergence indicates the kernel is contested.',
    'If the symbol_survival reading computes as mountain (low ε, no victims) and competence_transmission computes as tangled_rope (moderate ε, victims and beneficiaries), the kernel itself is not a single constraint but a family. The false_summit_mountain signature would not fire on the symbol reading because it declares no beneficiaries; the competence reading''s victims and beneficiaries are the structural signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame, conceptual, 'This constraint is the competence_transmission reading of the catastrophe_memory_survival kernel; sibling readings are symbol_survival_reading and hybrid_encoding_reading.').

omega_variable(
    practical_vs_symbolic_primacy,
    'Is the practical knowledge transmission the primary function of the ritual, or is the symbolic boundary-maintenance primary, with practical content being a secondary or spurious attribution?',
    'Ethnographic and historical analysis of ritual content across generations: measure the proportion of ritual elements that encode actionable survival information (timing, resource management, family protocols) versus purely symbolic markers. Track whether diaspora communities that maintain the ritual show measurable adaptive advantages correlated with practical content retention.',
    'If practical content is primary, the constraint is a genuine coordination mechanism with extractive overhead (tangled_rope). If symbolic is primary and practical content is post-hoc rationalization, the coordination story is cover and the constraint is a snare (extraction via identity maintenance). The hybrid reading predicts both are necessary and inseparable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_vs_symbolic_primacy, empirical, 'Primacy of practical vs. symbolic function in ritual''s survival value.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (enforcement of ritual form by specialists and community norms) or internalized (participants believe the ritual form itself is the survival mechanism, so they self-suppress deviations)?',
    'Post-migration suppression trajectory: compare communities that voluntarily simplified the ritual (dropping practical content) versus those that were forced to simplify. If suppression persists after external enforcement is removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more extractive than the raw suppression score indicates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cms_ctr_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(cms_ctr_tr_t0, observed).
narrative_ontology:measurement(cms_ctr_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(cms_ctr_tr_t20, observed).
narrative_ontology:measurement(cms_ctr_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement_basis(cms_ctr_tr_t40, observed).
narrative_ontology:measurement(cms_ctr_tr_t60, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(cms_ctr_tr_t60, observed).
narrative_ontology:measurement(cms_ctr_tr_t80, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 80, 0.38).
narrative_ontology:measurement_basis(cms_ctr_tr_t80, observed).
narrative_ontology:measurement(cms_ctr_tr_t100, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 100, 0.4).
narrative_ontology:measurement_basis(cms_ctr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(cms_ctr_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(cms_ctr_be_t0, observed).
narrative_ontology:measurement(cms_ctr_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(cms_ctr_be_t20, observed).
narrative_ontology:measurement(cms_ctr_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement_basis(cms_ctr_be_t40, observed).
narrative_ontology:measurement(cms_ctr_be_t60, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement_basis(cms_ctr_be_t60, observed).
narrative_ontology:measurement(cms_ctr_be_t80, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 80, 0.48).
narrative_ontology:measurement_basis(cms_ctr_be_t80, observed).
narrative_ontology:measurement(cms_ctr_be_t100, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 100, 0.5).
narrative_ontology:measurement_basis(cms_ctr_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(cms_ctr_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(cms_ctr_su_t0, observed).
narrative_ontology:measurement(cms_ctr_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(cms_ctr_su_t20, observed).
narrative_ontology:measurement(cms_ctr_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement_basis(cms_ctr_su_t40, observed).
narrative_ontology:measurement(cms_ctr_su_t60, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 60, 0.25).
narrative_ontology:measurement_basis(cms_ctr_su_t60, observed).
narrative_ontology:measurement(cms_ctr_su_t80, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 80, 0.28).
narrative_ontology:measurement_basis(cms_ctr_su_t80, observed).
narrative_ontology:measurement(cms_ctr_su_t100, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 100, 0.3).
narrative_ontology:measurement_basis(cms_ctr_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_survival kernel decomposes into three constraint stories linked by structural dependence: the competence_transmission reading (this story) provides the practical-knowledge extraction that the hybrid_encoding reading depends on; the symbol_survival reading provides the identity-coordination baseline that both other readings reference. The competence reading influences the hybrid reading by showing that practical knowledge alone can account for survival, putting pressure on the hybrid's claim that both registers are necessary. The symbol reading coexists with the competence reading as a competing interpretation held by different communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__competence_transmission_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
