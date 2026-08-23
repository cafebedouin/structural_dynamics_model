% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at Divine Mandate: Pharaoh as Cosmic Source
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint is the divine_mandate_reading of the maat_order_principle
 *   kernel. In this reading, Ma'at is not a mutual or distributed obligation
 *   but a unidirectional flow from cosmic divine order through Pharaoh to
 *   society. The ruler embodies Ma'at and is definitionally incapable of
 *   violating it, placing the royal institution outside the constraint
 *   structure that binds all other actors. This reading suppresses
 *   alternative kernels (reciprocity, distributed maintenance) and justifies
 *   extraction as cosmic necessity. It is authored as a snare because the
 *   coordination function (social order) serves as cover for asymmetric
 *   extraction: the ruler is structurally exempt from the obligations imposed
 *   on subjects.
 *
 * KEY AGENTS:
 *   - pharaonic_institution: Primary beneficiary and agenda-setter (institutional/arbitrage) â defines Ma'at's content and is exempt from its constraints
 *   - priestly_class: Secondary beneficiary and enforcement apparatus (organized/constrained) â ritualizes the divine mandate and receives patronage
 *   - common_subjects: Primary payer (powerless/trapped) â bear obligations, labor, and surplus extraction without reciprocal accountability mechanism
 *   - alternative_interpreters: Excluded voices (moderate/constrained) â hold reciprocity or distributed readings, suppressed by ideological enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.82).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.85).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, snare).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at Divine Mandate: Pharaoh as Cosmic Source").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '22d370b7-b2fe-4262-84d1-3d11741c7201').
narrative_ontology:cs_kernel_codification('22d370b7-b2fe-4262-84d1-3d11741c7201', fixed_text).
narrative_ontology:cs_authority_grounding('22d370b7-b2fe-4262-84d1-3d11741c7201', lineage).
narrative_ontology:cs_interpretation_layer_present('22d370b7-b2fe-4262-84d1-3d11741c7201').
narrative_ontology:cs_reading_relation('22d370b7-b2fe-4262-84d1-3d11741c7201', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('22d370b7-b2fe-4262-84d1-3d11741c7201', maat_order_principle__distributed_maintenance_reading, influences).
narrative_ontology:cs_axiom('22d370b7-b2fe-4262-84d1-3d11741c7201', foundational, pharaoh_is_maat_incarnate).
narrative_ontology:cs_axiom_status(pharaoh_is_maat_incarnate, holdable).
narrative_ontology:cs_axiom_grounding('22d370b7-b2fe-4262-84d1-3d11741c7201', pharaoh_is_maat_incarnate, theological).
narrative_ontology:cs_axiom('22d370b7-b2fe-4262-84d1-3d11741c7201', foundational, royal_action_definitionally_maat_compliant).
narrative_ontology:cs_axiom_status(royal_action_definitionally_maat_compliant, holdable).
narrative_ontology:cs_axiom_grounding('22d370b7-b2fe-4262-84d1-3d11741c7201', royal_action_definitionally_maat_compliant, theological).
narrative_ontology:cs_reference_frame('22d370b7-b2fe-4262-84d1-3d11741c7201', cosmic_divine_monarchy).
narrative_ontology:cs_drift_state('22d370b7-b2fe-4262-84d1-3d11741c7201', post_new_kingdom, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('22d370b7-b2fe-4262-84d1-3d11741c7201', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, pharaonic_institution).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, priestly_class).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, common_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Embodies Ma'at by definition and stands as the unilateral source of cosmic order for society. Defines the content of Ma'at, commands surplus extraction, and is structurally exempt from the constraint that binds all other actors. Can redefine ritual, theology, and law to maintain this position.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaonic_institution, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, pharaonic_institution, beneficiary).

% Administers the ritual and textual apparatus that legitimates Pharaoh's unique Ma'at embodiment. Receives state patronage, temple endowments, and social prestige in exchange for suppressing alternative readings. Cannot abandon the royal theology without losing institutional support.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, priestly_class, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, priestly_class, agenda_setter).

% Bear the obligation to maintain Ma'at through labor, tax, and conformity to hierarchy. Receive no reciprocal accountability mechanism from Pharaoh under this reading; their compliance is framed as cosmic necessity rather than negotiated exchange.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, common_subjects, payer,
    powerless, biographical, trapped, local).

% Hold reciprocity or distributed-maintenance readings of Ma'at that would constrain royal action. Structurally excluded from official discourse, temple theology, and scribal curriculum; their suppression is necessary for the divine mandate reading to persist.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, alternative_interpreters, excluded,
    moderate, biographical, constrained, regional).

% Record and transmit official ideology while possessing the literacy to perceive tensions between divine mandate rhetoric and administrative reality. Their observations remain largely internal to the scribal stratum.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, scribal_observers, observer,
    moderate, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaonic_institution).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains hierarchical social order by grounding all legitimacy in a unidirectional flow of cosmic authority from the divine realm through the king to society, centralizing interpretation and suppressing rival cosmologies.
% TRANSFER_FUNCTION: Transfers material surplus, labor obligation, and interpretive submission from common subjects to the pharaonic institution and its priestly apparatus, justified as necessary to sustain cosmic order.
% ABSENT_VOICES: Advocates of reciprocity and distributed maintenance readings are structurally excluded; they would argue that Ma'at imposes mutual obligations or shared responsibility, but their voices are suppressed by royal and priestly enforcement of the unilateral flow model.
% DISAPPEARANCE_RATIONALE: If the divine mandate reading vanished, the ideological exemption of Pharaoh would collapse. Subjects would no longer accept unilateral obligation as cosmic necessity; the state would require re-legitimation through reciprocity, raw coercion, or distributed religious authority.
% FOUNDING_PROBLEM: The need to unify a geographically extended Nile Valley polity under a single authority and justify centralized surplus extraction for monumental state projects, temple construction, and administrative hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: External to the pharaonic and priestly beneficiaries, dissenting literary texts (e.g., Admonitions of Ipuwer, Eloquent Peasant) and archaeological evidence of provincial temple autonomy corroborate that centralized divine mandate was contested. Modern Egyptological analysis supports the reading that the constraint shifted from distributed to centralized extraction over time.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the arrangement transfers substantial surplus and obligation to the royal institution while exempting it from constraint. Suppression is high (0.85) because alternative readings (reciprocity, distributed) must be actively suppressed to maintain the unilateral flow narrative. Theater ratio rises to 0.70 because priestly ritual and royal iconography performatively maintain the cosmic necessity claim even as administrative practice diverges. Accessibility collapse is high (0.75) because within this reading no conceptual alternative exists (Pharaoh IS Ma'at). Resistance is moderate (0.35) because while subjects may resist specific taxes or labor, challenging the divine mandate reading itself is dangerous and rare.
 *
 * PERSPECTIVAL GAP:
 *   The pharaonic seat experiences the constraint as cosmic identity (they ARE Ma'at, not constrained by it). The subject seat experiences the same structure as unilateral obligation enforced by priestly ideology. The priestly class experiences intermediate directionality: they benefit from patronage but are constrained in interpretive autonomy. The engine should compute divergent per-seat classifications: the pharaonic seat may compute as mountain-like (self-defining, zero extraction on self) while subject seats compute as snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Pharaoh has d near 0.0 (full beneficiary, actually source). Common subjects have d near 1.0 (full target). The priestly class sits in between: beneficiaries of patronage (d ~0.3) but also constrained by royal oversight. Alternative interpreters, if admitted, would have d near 1.0 as targets of suppression. The structural derivation from beneficiary/victim plus exit options produces this: pharaonic institution with arbitrage exit and agenda-setter/beneficiary role yields very low d; common subjects with trapped exit and payer role yield very high d.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by distinguishing the genuine coordination potential of Ma'at (social order, justice, balance) from the specific divine-mandate reading that exempts the ruler. The constraint is not a rope or tangled rope because the coordination function is not mutual: Pharaoh is definitionally outside the system. The reciprocity reading of the same kernel would likely compute as tangled rope or rope; this reading is a snare because it uses the coordination story (cosmic order) as cover for pure extraction by an exempt elite.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmic_necessity_vs_constructed_ideology,
    'Is the divine mandate reading a genuine theological commitment or a constructed ideology of extraction?',
    'Comparative analysis with other ancient Near Eastern kingship ideologies and archaeological evidence of royal accountability mechanisms or their absence.',
    'If purely constructed, confirms snare classification; if genuinely held as theology, may shift toward tangled_rope (genuine coordination belief plus extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmic_necessity_vs_constructed_ideology, conceptual, 'Whether the reading is theological conviction or extraction cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative readings structural (state violence, institutional exclusion) or internalized (subjects genuinely believe Pharaoh cannot err)?',
    'Analysis of dissent texts, evidence of popular rebellion versus passive acceptance, and post-collapse ideological persistence.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    sibling_reading_structural_delta,
    'How would classification change if the reciprocity_reading or distributed_maintenance_reading were adopted as the operative constraint?',
    'Compare beneficiary-victim structures and directionality under alternative readings of the same kernel.',
    'Reciprocity reading would likely reclassify as tangled_rope or rope with symmetric obligations; distributed reading would flatten extraction across all seats. This confirms the present reading is a distinct constraint with distinct epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural difference between kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(maat_tr_t6, maat_order_principle__divine_mandate_reading, theater_ratio, 6, 0.35).
narrative_ontology:measurement(maat_tr_t12, maat_order_principle__divine_mandate_reading, theater_ratio, 12, 0.5).
narrative_ontology:measurement(maat_tr_t18, maat_order_principle__divine_mandate_reading, theater_ratio, 18, 0.58).
narrative_ontology:measurement(maat_tr_t24, maat_order_principle__divine_mandate_reading, theater_ratio, 24, 0.65).
narrative_ontology:measurement(maat_tr_t30, maat_order_principle__divine_mandate_reading, theater_ratio, 30, 0.7).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(maat_be_t6, maat_order_principle__divine_mandate_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(maat_be_t12, maat_order_principle__divine_mandate_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(maat_be_t18, maat_order_principle__divine_mandate_reading, base_extractiveness, 18, 0.75).
narrative_ontology:measurement(maat_be_t24, maat_order_principle__divine_mandate_reading, base_extractiveness, 24, 0.8).
narrative_ontology:measurement(maat_be_t30, maat_order_principle__divine_mandate_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(maat_su_t6, maat_order_principle__divine_mandate_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(maat_su_t12, maat_order_principle__divine_mandate_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement(maat_su_t18, maat_order_principle__divine_mandate_reading, suppression_requirement, 18, 0.8).
narrative_ontology:measurement(maat_su_t24, maat_order_principle__divine_mandate_reading, suppression_requirement, 24, 0.85).
narrative_ontology:measurement(maat_su_t30, maat_order_principle__divine_mandate_reading, suppression_requirement, 30, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% The maat_order_principle kernel decomposes into three structurally distinct constraints: the divine_mandate_reading (this file) centralizes Ma'at in Pharaoh as unilateral source; the reciprocity_reading imposes mutual obligations; and the distributed_maintenance_reading distributes responsibility across all social levels. Their epsilon values and beneficiary/victim structures differ widely.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
