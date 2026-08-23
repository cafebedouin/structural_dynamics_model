% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__exogenous_override_reading, []).

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
 *   constraint_id: plural_marriage_mandate__exogenous_override_reading
 *   human_readable: Federal Coercion of Plural Marriage Abandonment (Exogenous Override Reading)
 *   domain: religious institutional history / political theology
 *
 * SUMMARY:
 *   This constraint story instantiates the exogenous_override_reading of the
 *   plural_marriage_mandate kernel. It treats the 1890 Manifesto not as
 *   legitimate prophetic reinterpretation but as the terminal capitulation of
 *   a church crushed by federal coercion. Beginning with the Edmunds Act
 *   (1882) and escalating through the Edmunds-Tucker Act (1887)âwhich
 *   authorized imprisonment of polygamists and seizure of church
 *   propertyâthe federal government conditioned the church's survival on
 *   public abandonment of plural marriage. The Manifesto is read as theater:
 *   a document framed as voluntary revelation that masks raw state coercion.
 *   The federal government is the structural beneficiary (territorial
 *   conformity, statehood on its terms); practicing polygamists are the
 *   victims (imprisonment, family dissolution, forced apostasy); church
 *   leadership occupies a dual position as both coerced victim and
 *   administrator of the new prohibition.
 *
 * KEY AGENTS:
 *   - Federal government: agenda setter and enforcer (institutional/analytical) â captures territorial conformity
 *   - Practicing polygamists: primary targets (powerless/identity_locked) â bear religious suppression costs
 *   - Church leadership: dual-positioned payer/agenda setter (institutional/constrained) â administers abandonment under duress
 *   - Monogamist church members: incidental beneficiaries (organized/mobile) â gain normalized civic status
 *   - Historical analyst: observer (analytical) â evaluates the coercion record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, 0.82).
domain_priors:suppression_score(plural_marriage_mandate__exogenous_override_reading, 0.91).
domain_priors:theater_ratio(plural_marriage_mandate__exogenous_override_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(plural_marriage_mandate__exogenous_override_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__exogenous_override_reading, snare).
narrative_ontology:human_readable(plural_marriage_mandate__exogenous_override_reading, "Federal Coercion of Plural Marriage Abandonment (Exogenous Override Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__exogenous_override_reading, "religious institutional history / political theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__exogenous_override_reading, '4386c5c6-a315-4757-8fc3-275ab5d18136').
narrative_ontology:cs_kernel_codification('4386c5c6-a315-4757-8fc3-275ab5d18136', formalized).
narrative_ontology:cs_authority_grounding('4386c5c6-a315-4757-8fc3-275ab5d18136', lineage).
narrative_ontology:cs_interpretation_layer_present('4386c5c6-a315-4757-8fc3-275ab5d18136').
narrative_ontology:cs_reading_relation('4386c5c6-a315-4757-8fc3-275ab5d18136', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('4386c5c6-a315-4757-8fc3-275ab5d18136', plural_marriage_mandate__institutional_pragmatism_reading, influences).
narrative_ontology:cs_axiom('4386c5c6-a315-4757-8fc3-275ab5d18136', foundational, coerced_abandonment_absent_divine_authority).
narrative_ontology:cs_axiom_status(coerced_abandonment_absent_divine_authority, holdable).
narrative_ontology:cs_axiom_grounding('4386c5c6-a315-4757-8fc3-275ab5d18136', coerced_abandonment_absent_divine_authority, deontological).
narrative_ontology:cs_axiom('4386c5c6-a315-4757-8fc3-275ab5d18136', foundational, state_coercion_corrupts_ecclesiastical_legitimacy).
narrative_ontology:cs_axiom_status(state_coercion_corrupts_ecclesiastical_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('4386c5c6-a315-4757-8fc3-275ab5d18136', state_coercion_corrupts_ecclesiastical_legitimacy, deontological).
narrative_ontology:cs_reference_frame('4386c5c6-a315-4757-8fc3-275ab5d18136', divine_command_perpetuity).
narrative_ontology:cs_drift_state('4386c5c6-a315-4757-8fc3-275ab5d18136', post_manifesto_1890, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('4386c5c6-a315-4757-8fc3-275ab5d18136', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__exogenous_override_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__exogenous_override_reading, monogamist_church_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__exogenous_override_reading, church_leadership).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, federal_supremacy_over_territorial_law).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__exogenous_override_reading, manifesto_as_voluntary_revelation_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploys federal criminal prosecution, imprisonment, property seizure under the Edmunds and Edmunds-Tucker Acts, and conditions statehood on abandonment of plural marriage. Achieves territorial legal conformity and political submission of the Utah Territory. Can cease enforcement at any time but maintains the threat to secure compliance.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Religious practitioners for whom plural marriage is a divine commandment fused to personal and communal identity. They face federal imprisonment, fines, disenfranchisement, and family dissolution. Openly continuing the practice is impossible under active federal enforcement; abandoning it constitutes apostasy from their own theological framework.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, practicing_polygamists, payer,
    powerless, biographical, identity_locked, national).

% Administers the 1890 Manifesto prohibiting plural marriages under direct federal duress. Bears the doctrinal cost of reclassifying a former divine requirement while enforcing the new prohibition on members to prevent institutional destruction. Their agency is structurally compressed between federal guns and member conscience.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, church_leadership, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__exogenous_override_reading, church_leadership, agenda_setter).

% Members who did not practice plural marriage and benefited from the reduced federal persecution of the church body after the Manifesto, gaining normalized civic status and eventual statehood, though they did not direct the change.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, monogamist_church_members, beneficiary,
    organized, biographical, mobile, national).

% Evaluates the historical record of federal correspondence, prosecutorial discretion, and legislative debate to distinguish coerced capitulation from endogenous doctrinal development. Neither collects from nor pays into the constraint.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__exogenous_override_reading, historical_analyst, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__exogenous_override_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claimed coordination function is the enforcement of national legal uniformity and territorial integration under federal supremacy. In this reading, that function is the cover story for religious suppression.
% TRANSFER_FUNCTION: Moves abandonment of a religious practice and public doctrinal conformity from practicing polygamists and church leadership to the federal government, in exchange for cessation of property seizure and imprisonment.
% ABSENT_VOICES: Practicing polygamists who were driven underground or excommunicated for refusing to abandon the practice; dissenting members who viewed the Manifesto as pure political capitulation rather than revelation. They are not present in the official doctrinal record.
% DISAPPEARANCE_RATIONALE: If federal imprisonment and property seizure threats vanished, the constraint forcing abandonment would lose all coercive force. The church would likely revert to the previously mandated divine practice, rearranging Mormon kinship structures, theological claims, and territorial politics.
% FOUNDING_PROBLEM: Federal need to assert legal supremacy over Utah Territory, eliminate plural marriage as a visible practice, and secure statehood on terms of national legal conformity.
% FOUNDING_PROBLEM_CORROBORATION: Congressional debate on the Edmunds-Tucker Act, federal prosecutorial records, and statehood admission documents attest the federal motive. External historians and legal scholars corroborate that territorial integration and Republican Party political supremacy were the driving forces, not theological evolution within Mormonism.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__exogenous_override_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint forces abandonment of a core religious practice under threat of violence and property loss. Suppression is very high (0.91) due to federal criminal penalties, imprisonment, disenfranchisement, and institutional asset seizure. Theater ratio is high (0.65) because the Manifesto's public framing as prophetic revelation masks the documented federal duress that produced it. Accessibility collapse is high (0.88) because open practice became impossible once federal enforcement targeted it. Resistance is moderate (0.45) because overt resistance was crushed by state power, though underground persistence continued. Measurements share one time grid to prevent misalignment.
 *
 * PERSPECTIVAL GAP:
 *   The federal government seat experiences the constraint as legitimate law enforcement and territorial integration; the practicing polygamist seat experiences it as violent religious suppression destroying families and conscience. The church leadership seat experiences a schismatic gap: externally enforcing compliance, internally knowing the doctrine was surrendered to state power. The engine should compute these seats differently from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the declared beneficiary (d near 0.0) because territorial conformity and legal supremacy flow to it. Practicing polygamists are declared victims with identity_locked exit (d near 1.0), amplifying effective extraction because their religious identity fuses them to the target position. Church leadership is intermediate: their constrained exit and institutional power place them in the middle of the directionality range, but their payer role pulls them toward the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâterritorial integration and elimination of plural marriage as a barrier to statehoodâwas resolved by 1896. Yet the suppression machinery persisted well beyond its territorial objective: the Second Manifesto (1904), continued prosecutions, and cultural stigma extended the constraint's life after its mandate expired. This is mandatrophy: the coercion outlived its purpose and became inertial suppression of a religious minority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_origin_ambiguity,
    'Is the 1890 Manifesto best explained by direct federal coercion, a genuine prophetic revelation, or an internal strategic decision to legitimate capitulation?',
    'Archival discovery of federal-church correspondence contemporaneous with the Manifesto''s drafting; analysis of prosecutorial discretion patterns immediately pre- and post-1890.',
    'Resolution would either confirm the snare classification (exogenous coercion) or shift toward tangled_rope (if internal strategy mixed with external pressure) or mountain-like doctrinal legitimacy (if genuine revelation is historically substantiated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_origin_ambiguity, conceptual, 'Ambiguity about the causal origin of the Manifesto').

omega_variable(
    suppression_internalization,
    'Did federal coercion become internalized as theological guilt and shame, making suppression partially self-enforcing within the community after overt federal pressure declined?',
    'Oral history and sociological study of descendant communities (e.g., fundamentalist groups versus mainstream LDS) to measure persistent internalized stigma against plural marriage independent of legal threat.',
    'If suppression is partly internalized, the constraint''s effective extraction exceeds the structural measureâvictims carry the suppression with them after federal enforcement eases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    church_leadership_complicity,
    'To what extent was church leadership a coerced victim versus a strategic actor exploiting the federal crisis to consolidate monogamist authority?',
    'Biographical and administrative record analysis of leadership deliberations, factional alignments, and post-Manifesto power consolidation within the church hierarchy.',
    'If leadership was primarily strategic, the directionality for that seat shifts toward beneficiary; if primarily coerced, it remains near the target end. This changes the seat-divergence profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(church_leadership_complicity, preference, 'Ambiguity about church leadership''s agency under coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__exogenous_override_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t0, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(plur_tr_t7, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 7, 0.35).
narrative_ontology:measurement(plur_tr_t14, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 14, 0.65).
narrative_ontology:measurement(plur_tr_t21, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 21, 0.7).
narrative_ontology:measurement(plur_tr_t28, plural_marriage_mandate__exogenous_override_reading, theater_ratio, 28, 0.68).

% Extraction over time
narrative_ontology:measurement(plur_be_t0, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(plur_be_t7, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 7, 0.65).
narrative_ontology:measurement(plur_be_t14, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 14, 0.82).
narrative_ontology:measurement(plur_be_t21, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 21, 0.8).
narrative_ontology:measurement(plur_be_t28, plural_marriage_mandate__exogenous_override_reading, base_extractiveness, 28, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t0, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(plur_su_t7, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 7, 0.88).
narrative_ontology:measurement(plur_su_t14, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 14, 0.91).
narrative_ontology:measurement(plur_su_t21, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 21, 0.85).
narrative_ontology:measurement(plur_su_t28, plural_marriage_mandate__exogenous_override_reading, suppression_requirement, 28, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
