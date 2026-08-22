% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__allegorical_ancient_near_east, []).

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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature
 *   domain: religious_studies/biblical_hermeneutics
 *
 * SUMMARY:
 *   This constraint models the institutionalized interpretive tradition that
 *   reads Genesis 1-2 as Ancient Near Eastern mythopoetic literature rather
 *   than historical or scientific discourse. Enforced through biblical
 *   studies curricula, peer review, and mainline denominational teaching, it
 *   decouples the text from empirical cosmology and biology. The coordination
 *   functionâpreserving religious credibility in a scientific ageâis
 *   genuine, but the arrangement also extracts authority from literalist and
 *   confessional communities and consolidates it in the historical-critical
 *   guild. The reading is one of three contested readings of the
 *   genesis_creation_narrative kernel.
 *
 * KEY AGENTS:
 *   - Historical-critical guild: agenda_setter with institutional power and constrained exitâadministers the method and collects epistemic authority.
 *   - Mainline religious institutions: beneficiary with organized powerâgains social credibility at the cost of textual normativity.
 *   - Scientific communities: beneficiary with institutional powerâexperiences reduced religious opposition to empirical claims.
 *   - Literalist religious communities: primary target with organized power and identity-locked exitâbears delegitimization and loss of cultural authority.
 *   - Confessional theologians: secondary target with moderate power and constrained exitâmarginalized within mainline institutions for holding pre-critical commitments.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.45).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.44).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.45).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics").

domain_priors:requires_active_enforcement(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '56641a5c-b721-4cac-9311-4086b84b48dc').
narrative_ontology:cs_kernel_codification('56641a5c-b721-4cac-9311-4086b84b48dc', fixed_text).
narrative_ontology:cs_authority_grounding('56641a5c-b721-4cac-9311-4086b84b48dc', expertise).
narrative_ontology:cs_interpretation_layer_present('56641a5c-b721-4cac-9311-4086b84b48dc').
narrative_ontology:cs_reading_relation('56641a5c-b721-4cac-9311-4086b84b48dc', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('56641a5c-b721-4cac-9311-4086b84b48dc', genesis_creation_narrative__theistic_evolutionary, coexists_with).
narrative_ontology:cs_axiom('56641a5c-b721-4cac-9311-4086b84b48dc', foundational, text_as_ane_myth).
narrative_ontology:cs_axiom_status(text_as_ane_myth, holdable).
narrative_ontology:cs_axiom_grounding('56641a5c-b721-4cac-9311-4086b84b48dc', text_as_ane_myth, empirically_contingent).
narrative_ontology:cs_axiom('56641a5c-b721-4cac-9311-4086b84b48dc', foundational, no_adjudicative_authority_over_cosmology).
narrative_ontology:cs_axiom_status(no_adjudicative_authority_over_cosmology, holdable).
narrative_ontology:cs_axiom_grounding('56641a5c-b721-4cac-9311-4086b84b48dc', no_adjudicative_authority_over_cosmology, conventional).
narrative_ontology:cs_reference_frame('56641a5c-b721-4cac-9311-4086b84b48dc', ane_mythopoetic_reference).
narrative_ontology:cs_drift_state('56641a5c-b721-4cac-9311-4086b84b48dc', postliberal_challenges_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('56641a5c-b721-4cac-9311-4086b84b48dc', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_guild).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, mainline_religious_institutions).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, scientific_communities).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, literalist_religious_communities).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, confessional_theologians).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_method).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, non_overlapping_magisteria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the methodological norms for biblical interpretation in universities, seminaries, and peer-reviewed journals. Trains graduate students, controls hiring and tenure, and administers the historical-critical framework that classifies Genesis 1-2 as Ancient Near Eastern myth. Benefits from institutional prestige, research funding, and epistemic authority derived from this interpretive monopoly.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_guild, agenda_setter,
    institutional, generational, constrained, global).

% Retain educated members and public credibility by endorsing a reading that accommodates modern cosmology and biology. Avoid costly conflicts with scientific education and bioethical consensus, preserving social relevance at the price of ceding textual normativity to the scholarly guild.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, mainline_religious_institutions, beneficiary,
    organized, generational, constrained, national).

% Face reduced institutional opposition from mainline religion on cosmology and evolutionary biology because the dominant religious reading decouples scriptural authority from empirical claims. The constraint removes a historical source of political and educational friction.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, scientific_communities, beneficiary,
    institutional, civilizational, analytical, global).

% Bear the cost of delegitimization; their inerrant reading is classified as pre-scientific or fundamentalist within academic and mainline discourse. Their communal identity is fused with literal textual authority, making exit from the mythopoetic framework existentially costly. They fund parallel institutions to maintain their reading.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literalist_religious_communities, payer,
    organized, generational, identity_locked, global).

% Work within religious institutions that have adopted the mythopoetic framework. Their theological commitments to textual normativity and traditional doctrine are treated as unscholarly or pre-critical, limiting publication opportunities, career advancement, and influence within mainline seminaries.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, confessional_theologians, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_guild).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of maintaining religious community coherence and public credibility in a scientific modernity by decoupling Genesis 1-2 from empirical cosmological and biological claims, allowing simultaneous participation in religious and scientific institutions.
% TRANSFER_FUNCTION: Moves interpretive authority, institutional prestige, and curricular control from confessional and literalist religious communities to the historical-critical scholarly guild and science-affirming mainline religious institutions.
% ABSENT_VOICES: Young-earth creationist scientists and biblicist theologians are structurally excluded from peer-reviewed biblical studies discourse and mainline seminary curricula; their objections are dismissed as pre-critical or fundamentalist rather than engaged as live alternatives.
% DISAPPEARANCE_RATIONALE: If the mythopoetic reading vanished, mainline denominations would face renewed science-religion conflict, biblical studies would lose its dominant methodological foundation, and literalist communities would regain epistemic parity in religious education and public discourse.
% FOUNDING_PROBLEM: The Enlightenment and rise of modern historical and natural sciences created a crisis of authority for biblical texts in educated Western society; the mythopoetic reading was developed to preserve textual reverence and religious community without scientific conflict.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of biblical scholarship and non-confessional religion scholars attest the crisis narrative and the historical emergence of the mythopoetic solution; confessional theologians and postliberal critics outside the benefiting parties dispute that the crisis required this specific solution, arguing it represents a capitulation to modernity that hollows out the text's theological substance.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.45, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).
:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects moderate authority transfer: the guild gains institutional control while literalist communities lose epistemic standing. Suppression (0.44) captures the active enforcement of historical-critical norms in academia and mainline seminaries, where literalist readings are excluded from respectable discourse. Theater ratio (0.22) is low but rising, indicating that some scholarly activity now defends methodological hegemony rather than advancing inquiry. Accessibility collapse (0.65) is elevated because once the ANE myth frame is adopted, literalist alternatives appear intellectually non-viable within the scholarly horizon. Resistance (0.50) reflects robust counter-mobilization through alternative seminaries, popular apologetics, and political movements. Measurements share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The historical-critical guild experiences the constraint as genuine coordinationâsolving the science-religion conflict through disciplined interpretationâwhile literalist communities experience the same structure as enforced extraction that strips their sacred text of authority. The engine computes this divergence from the structural data: identical enforcement mechanisms produce opposite directionality depending on beneficiary versus victim position and exit options (analytical versus identity-locked).
 *
 * DIRECTIONALITY LOGIC:
 *   The guild, mainline institutions, and scientific communities sit near the beneficiary end: the constraint subsidizes their social and epistemic position. Literalist communities and confessional theologians sit near the target end: the constraint extracts authority and opportunity from them. The divergence is amplified by scope (global for the guild, national for mainline institutions) and by exit modulationâidentity-locked literalists experience much higher effective extraction than mobile or analytical beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope rather than snare because the coordination function is structurally genuine: without some mechanism for decoupling Genesis from empirical science, mainline religious institutions would face genuine collective-action problems retaining members in scientific modernity. The extraction is asymmetric but not total. It is not a rope because victims are identifiable and enforcement is required to maintain the hegemony against resistant literalist alternatives. It is not a scaffold because it carries no sunset clause and has become a permanent methodological foundation rather than a transitional support.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the allegorical ANE reading of Genesis logically foreclose the literal young earth reading, or do they coexist as incommensurable language games?',
    'Philosophical analysis of the logical relationship between mythopoetic genre claims and literal historical claims; empirical study of whether individuals can simultaneously hold both frameworks without contradiction.',
    'If foreclosed, the allegorical reading functions as a stronger authority constraint with higher effective suppression; if merely coexisting, the extraction from literalist communities is lower than structurally measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Logical relationship between mythopoetic and literal readings of the same kernel').

omega_variable(
    authority_transfer_mechanism,
    'Is the authority consolidated by the historical-critical guild a genuine extraction from literalist communities, or a legitimate epistemic division of labor?',
    'Comparative sociology of religion measuring whether guild prestige correlates with literalist community marginalization, resource loss, and institutional closure.',
    'If a legitimate division of labor, the constraint trends toward rope; if a zero-sum authority transfer, tangled_rope or snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_transfer_mechanism, empirical, 'Whether authority consolidation is extraction or legitimate specialization').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of literalist readings structural (institutional barriers in academia and seminaries) or internalized (literalists adopting scholarly skepticism toward their own tradition)?',
    'Post-exit suppression trajectory: if literalists who leave mainline institutions recover their literal reading, suppression was structural; if they retain internalized historical-critical habits, suppression was partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and extraction is more deeply embedded in agent cognition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gene_tr_t8, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 8, 0.12).
narrative_ontology:measurement(gene_tr_t16, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 16, 0.15).
narrative_ontology:measurement(gene_tr_t24, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 24, 0.18).
narrative_ontology:measurement(gene_tr_t32, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 32, 0.2).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(gene_be_t8, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 8, 0.3).
narrative_ontology:measurement(gene_be_t16, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(gene_be_t24, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 24, 0.4).
narrative_ontology:measurement(gene_be_t32, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 32, 0.42).
narrative_ontology:measurement(gene_be_t40, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 40, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gene_su_t8, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(gene_su_t16, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 16, 0.35).
narrative_ontology:measurement(gene_su_t24, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(gene_su_t32, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 32, 0.42).
narrative_ontology:measurement(gene_su_t40, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 40, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionary).

% DUAL FORMULATION NOTE:
% The genesis_creation_narrative kernel decomposes into three structurally distinct constraints: allegorical_ancient_near_east (ANE myth, high decoupling from science), literal_young_earth (inerrant chronicle, high conflict with science), and theistic_evolutionary (compatible framework, partial decoupling). Each has a distinct epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
