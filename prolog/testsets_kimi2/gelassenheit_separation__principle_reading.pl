% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__principle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__principle_reading, []).

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
 *   constraint_id: gelassenheit_separation__principle_reading
 *   human_readable: Gelassenheit Separation Principle Reading (Functional Isolation)
 *   domain: religious_studies/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   This constraint is the principle_reading of the contested
 *   gelassenheit_separation kernel. It instantiates the Anabaptist
 *   technology-governance principle that separation from 'the world' is
 *   achieved by avoiding structural entanglement in worldly systems,
 *   permitting tools like solar or pneumatic equipment when they are
 *   functionally isolated, while forbidding internet and insurance regardless
 *   of isolation claims because they embed ongoing dependency. The reading is
 *   distinguished from its artifact_reading sibling (which forbids technology
 *   by surface appearance) and its consequence_reading sibling (which
 *   evaluates by effects on visiting and mutual aid).
 *
 * KEY AGENTS:
 *   - bishopric_leadership: Primary agenda_setter (powerful/constrained) â interprets and enforces the Ordnung's technology distinctions, bound by tradition
 *   - ordnung_community: Primary beneficiary (organized/constrained) â the collective whose identity is preserved by maintained boundaries
 *   - member_households: Primary target (moderate/identity_locked) â bear uninsured risk and economic friction from prohibitions
 *   - youth_before_baptism: Excluded voice (powerless/trapped) â subject to rules with no deliberative seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, 0.48).
domain_priors:suppression_score(gelassenheit_separation__principle_reading, 0.52).
domain_priors:theater_ratio(gelassenheit_separation__principle_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gelassenheit_separation__principle_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__principle_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__principle_reading, "Gelassenheit Separation Principle Reading (Functional Isolation)").
narrative_ontology:topic_domain(gelassenheit_separation__principle_reading, "religious_studies/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__principle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__principle_reading, '525ed7ce-e159-48bf-8bec-3bf219b7715c').
narrative_ontology:cs_kernel_codification('525ed7ce-e159-48bf-8bec-3bf219b7715c', fixed_text).
narrative_ontology:cs_authority_grounding('525ed7ce-e159-48bf-8bec-3bf219b7715c', lineage).
narrative_ontology:cs_interpretation_layer_present('525ed7ce-e159-48bf-8bec-3bf219b7715c').
narrative_ontology:cs_reading_relation('525ed7ce-e159-48bf-8bec-3bf219b7715c', gelassenheit_separation__artifact_reading, forecloses).
narrative_ontology:cs_reading_relation('525ed7ce-e159-48bf-8bec-3bf219b7715c', gelassenheit_separation__consequence_reading, coexists_with).
narrative_ontology:cs_axiom('525ed7ce-e159-48bf-8bec-3bf219b7715c', foundational, functional_isolation_suffices).
narrative_ontology:cs_axiom_status(functional_isolation_suffices, holdable).
narrative_ontology:cs_axiom_grounding('525ed7ce-e159-48bf-8bec-3bf219b7715c', functional_isolation_suffices, theological).
narrative_ontology:cs_axiom('525ed7ce-e159-48bf-8bec-3bf219b7715c', foundational, structural_entanglement_defiles_not_appearance).
narrative_ontology:cs_axiom_status(structural_entanglement_defiles_not_appearance, holdable).
narrative_ontology:cs_axiom_grounding('525ed7ce-e159-48bf-8bec-3bf219b7715c', structural_entanglement_defiles_not_appearance, theological).
narrative_ontology:cs_reference_frame('525ed7ce-e159-48bf-8bec-3bf219b7715c', anabaptist_separation).
narrative_ontology:cs_drift_state('525ed7ce-e159-48bf-8bec-3bf219b7715c', digital_ubiquity, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('525ed7ce-e159-48bf-8bec-3bf219b7715c', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__principle_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__principle_reading, ordnung_community).
narrative_ontology:constraint_victim(gelassenheit_separation__principle_reading, member_households).
narrative_ontology:constraint_vindicates(gelassenheit_separation__principle_reading, biblical_nonconformity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the Ordnung's technology rules, distinguishing tools that create ongoing dependency on external systems from those that can operate independently. Must maintain communal consensus and spiritual legitimacy while adapting rulings to new technologies, bound by tradition and the threat of schism.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, bishopric_leadership, agenda_setter,
    powerful, generational, constrained, local).

% The church district as a collective body whose identity depends on visible separation from modern worldly institutions. Benefits from preserved boundaries and the avoidance of structural dependencies that could dissolve communal autonomy and spiritual distinctiveness.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, ordnung_community, beneficiary,
    organized, generational, constrained, local).

% Live under bishopric rulings that prohibit insurance and internet even when technical isolation is possible, forcing them to bear uninsured medical costs, educational limitations, and economic inefficiency as a condition of membership. Leaving means shunning and loss of family identity.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, member_households, payer,
    moderate, biographical, identity_locked, local).

% Young people growing up in the community who must observe household technology restrictions before baptism and have no formal voice in Ordnung deliberations. During rumspringa they may temporarily explore outside technologies, but sustained use requires leaving the community and risking shunning.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__principle_reading, youth_before_baptism, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__principle_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__principle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the communal identity and spiritual separation (Gelassenheit) of an Anabaptist community from worldly structures by evaluating technology according to whether it creates ongoing dependency on external systems, rather than by surface resemblance to modernity.
% TRANSFER_FUNCTION: Transfers risk and inconvenience from the communal boundary-maintaining apparatus onto individual households, who must bear uninsured losses and forgo informational and economic tools even when functional isolation is technically achievable.
% ABSENT_VOICES: Young members desiring internet access for education or livelihoods; households that would choose insurance if permitted; technologists and engineers who could demonstrate functional isolation; ex-members who experienced the costs and were not heard in Ordnung deliberation.
% DISAPPEARANCE_RATIONALE: If the principle vanished, households would adopt insurance and internet where functionally isolated, the community's structural distinctness would erode into mainstream economic and informational networks, and the bishopric's authority over technology would collapse.
% FOUNDING_PROBLEM: How to maintain Gelassenheitâyieldedness to God and separation from the worldâwhen surrounded by modernizing English society whose technologies and institutions penetrate every domain of material life.
% FOUNDING_PROBLEM_CORROBORATION: Anabaptist historians and theologians attest the founding problem as live; outside sociologists of religion corroborate that boundary maintenance remains central to Amish identity. Some internal reformers and young members argue functional isolation already solves the problem, while traditionalists argue any accommodation threatens dissolution.
narrative_ontology:disappearance_verdict(gelassenheit_separation__principle_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__principle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__principle_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__principle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__principle_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__principle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gelassenheit_separation__principle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gelassenheit_separation__principle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the functional-isolation criterion permits a significant range of tools, but it remains substantial because insurance and internet are categorically forbidden even when isolation is technically achievable, forcing households to bear real risk and inefficiency. Suppression is moderate (0.52): enforcement relies on communal monitoring, bishopric authority, and the threat of shunning rather than state violence, but the identity-locked exit makes it psychologically heavy. Theater ratio is moderate (0.35): some enforcement activity has shifted toward visible boundary maintenance rather than spiritual substance as technology has proliferated. Accessibility collapse is moderate-high (0.60) because while leaving is physically possible, the identity and family costs make the alternative nearly unthinkable for committed members. Resistance is low (0.30): pushback is contained by the community's insularity and the doctrinal framing.
 *
 * PERSPECTIVAL GAP:
 *   The bishopric and the communal beneficiary seat experience the constraint as preservation of a sacred identity and necessary spiritual defense, computing toward coordination. Member households experience the same structure as risk-bearing and material constraint, computing toward extraction. The divergence is driven by exit options: the bishopric is constrained by tradition but retains interpretive authority, while households are identity-locked into compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   The ordnung_community is the declared beneficiary (low d): it receives the preserved boundary and communal identity. Member households are the declared victims/payers (high d): they bear the material costs of prohibited insurance and internet. The bishopric_leadership sits near the agenda_setter middleânot a personal financial beneficiary, but structurally subsidized by the authority the constraint grants. Youth are excluded from the conversation entirely (no directional flow).
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy and the explicit coordination function, this constraint could be misread as a snare (pure extraction via religious authority) or a rope (pure coordination of identity). The tangled_rope classification is warranted because the coordination functionâpreserving a coherent communal way of life against assimilationist pressureâis genuine and historically grounded, while the asymmetric extractionâhouseholds forced to bear uninsured losses even when functional isolation is possibleâis equally real and requires active enforcement to maintain. The claim/metric independence is respected: the claimed type is tangled_rope and the metrics describe a moderately extractive, moderately theatrical, actively enforced arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_isolation_viability,
    'Can technologies like the internet or commercial insurance ever be truly functionally isolated from the worldly systems they connect to, or does adoption necessarily create structural dependency?',
    'Technical audit of whether off-grid internet nodes or mutual-aid substitutes eliminate structural dependency, combined with ethnographic study of actual household practice in communities that have experimented with partial adoption.',
    'If functional isolation is technically impossible, the principle reading converges with the artifact reading and base extractiveness rises toward the higher sibling epsilon; if possible, the principle reading maintains its distinct lower-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_isolation_viability, empirical, 'Whether functional isolation is technically achievable for prohibited technologies').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (shunning, expulsion, economic ostracism) or internalized (religious identity fused with compliance)?',
    'Post-exit trajectory study: if compliance and guilt persist after structural barriers are removed, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression exceeds the structural measure and member households carry the constraint even in thought after leaving; if purely structural, the constraint is more fragile than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_reading_location,
    'This constraint is the principle_reading of kernel gelassenheit_separation. Sibling readings (artifact_reading, consequence_reading) would change the victim set and coordination boundaries: artifact_reading expands prohibition to surface appearance, raising epsilon; consequence_reading evaluates by communal-practice effects rather than structural dependency. Where is the disagreement structurally located?',
    'Cross-affiliation comparison of Ordnung technology rulings across Amish districts to determine which structural element (appearance, function, or community effect) actually drives prohibition decisions.',
    'If appearance drives decisions, artifact_reading is the truer descriptor and this reading''s lower epsilon is mis-specified; if community effect drives them, consequence_reading dominates and the victim set shifts to households whose tool use disrupts visiting patterns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural ambiguity between sibling readings of the Gelassenheit separation kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__principle_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gelassenheit_principle_tr_t0, gelassenheit_separation__principle_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gelassenheit_principle_tr_t10, gelassenheit_separation__principle_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(gelassenheit_principle_tr_t20, gelassenheit_separation__principle_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(gelassenheit_principle_tr_t30, gelassenheit_separation__principle_reading, theater_ratio, 30, 0.31).
narrative_ontology:measurement(gelassenheit_principle_tr_t40, gelassenheit_separation__principle_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(gelassenheit_principle_tr_t50, gelassenheit_separation__principle_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(gelassenheit_principle_be_t0, gelassenheit_separation__principle_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(gelassenheit_principle_be_t10, gelassenheit_separation__principle_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement(gelassenheit_principle_be_t20, gelassenheit_separation__principle_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(gelassenheit_principle_be_t30, gelassenheit_separation__principle_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(gelassenheit_principle_be_t40, gelassenheit_separation__principle_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(gelassenheit_principle_be_t50, gelassenheit_separation__principle_reading, base_extractiveness, 50, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gelassenheit_principle_su_t0, gelassenheit_separation__principle_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gelassenheit_principle_su_t10, gelassenheit_separation__principle_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(gelassenheit_principle_su_t20, gelassenheit_separation__principle_reading, suppression_requirement, 20, 0.49).
narrative_ontology:measurement(gelassenheit_principle_su_t30, gelassenheit_separation__principle_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(gelassenheit_principle_su_t40, gelassenheit_separation__principle_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(gelassenheit_principle_su_t50, gelassenheit_separation__principle_reading, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__principle_reading, gelassenheit_separation__consequence_reading).

% DUAL FORMULATION NOTE:
% This constraint is the principle_reading of the gelassenheit_separation kernel, distinguished by its functional-isolation criterion. The artifact_reading and consequence_reading instantiate the same kernel with different evaluation criteria (appearance vs community effect), producing different epsilon values and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
