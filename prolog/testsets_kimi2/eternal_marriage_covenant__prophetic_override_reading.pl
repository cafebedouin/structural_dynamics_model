% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Prophetic Override of Eternal Marriage Covenant (1890 Manifesto Reading)
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   This is the prophetic_override_reading of the eternal_marriage_covenant
 *   kernel. The constraint is the doctrinal mechanismâcontinuing revelation
 *   allowing the living prophet to supersede prior canonâactivated in 1890
 *   to suspend plural marriage under federal existential threat. The reading
 *   treats the Manifesto not as mere political accommodation but as a genuine
 *   exercise of prophetic authority that overrides D&C 132. Sibling readings
 *   include the immutable_commandment_reading (which holds D&C 132 as
 *   eternally binding) and the temporal_accommodation_reading (which treats
 *   the Manifesto as civil obedience without doctrinal supersession).
 *
 * KEY AGENTS:
 *   - Church President/Prophet: agenda_setter with arbitrage-grade exitâcan receive new revelation that rewrites prior commands.
 *   - Church Institutional Leadership: beneficiaryâcollects survival and continuity without bearing the personal cost of abandoning covenants.
 *   - Polygamist Practitioners: primary payerâidentity-locked, had acted on the prior commandment and must now reverse their lives under coercive pressure.
 *   - Doctrinal Hardliners: secondary payerâidentity-locked, resist the override as betrayal and face exclusion.
 *   - Federal Government: external observerâapplied the pressure that activated the mechanism but does not participate in its theological logic.
 *   - Church Membership at Large: mixed beneficiary/payerâgains institutional survival, pays in cognitive dissonance and acceptance of leadership prerogative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.62).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.75).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Prophetic Override of Eternal Marriage Covenant (1890 Manifesto Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '547dbddd-759e-4019-800f-efa61398fabc').
narrative_ontology:cs_kernel_codification('547dbddd-759e-4019-800f-efa61398fabc', fixed_text).
narrative_ontology:cs_authority_grounding('547dbddd-759e-4019-800f-efa61398fabc', lineage).
narrative_ontology:cs_interpretation_layer_present('547dbddd-759e-4019-800f-efa61398fabc').
narrative_ontology:cs_reading_relation('547dbddd-759e-4019-800f-efa61398fabc', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('547dbddd-759e-4019-800f-efa61398fabc', eternal_marriage_covenant__temporal_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('547dbddd-759e-4019-800f-efa61398fabc', foundational, living_prophet_supersedes_canon).
narrative_ontology:cs_axiom_status(living_prophet_supersedes_canon, holdable).
narrative_ontology:cs_axiom_grounding('547dbddd-759e-4019-800f-efa61398fabc', living_prophet_supersedes_canon, theological).
narrative_ontology:cs_axiom('547dbddd-759e-4019-800f-efa61398fabc', foundational, revelation_conditional_on_circumstance).
narrative_ontology:cs_axiom_status(revelation_conditional_on_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('547dbddd-759e-4019-800f-efa61398fabc', revelation_conditional_on_circumstance, theological).
narrative_ontology:cs_reference_frame('547dbddd-759e-4019-800f-efa61398fabc', living_prophetic_authority_supremacy).
narrative_ontology:cs_drift_state('547dbddd-759e-4019-800f-efa61398fabc', post_manifesto_1890_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('547dbddd-759e-4019-800f-efa61398fabc', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_membership_at_large).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, polygamist_practitioners).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, doctrinal_hardliners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, church_membership_at_large).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the authority to receive new revelation that supersedes prior canonized scripture; exercised this authority in 1890 to issue the Manifesto suspending plural marriage under federal existential threat. The authority is self-reinforcing: its exercise validates its existence.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_president_prophet, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefits from institutional survival and retention of property and legal standing; enforces the prophetic override through ecclesiastical discipline and public teaching. Bears no direct cost of the doctrinal reversal.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institutional_leadership, beneficiary,
    institutional, generational, constrained, global).

% Receives continued existence of the church and legal legitimacy of their community. Bears the cognitive and ritual cost of accepting that a previously essential covenant can be set aside by leadership fiat without scriptural emendation.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_membership_at_large, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(eternal_marriage_covenant__prophetic_override_reading, church_membership_at_large, payer).

% Had entered plural marriages under commandment of D&C 132 as a requirement for exaltation; instructed to abandon the practice and existing unions under threat of federal prosecution and excommunication. Prior sacrifices and covenants are not theologically invalidated but are practically delegitimized.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, polygamist_practitioners, payer,
    moderate, biographical, identity_locked, national).

% Hold that D&C 132 is immutable eternal law; resist the Manifesto as doctrinal betrayal rather than legitimate supersession. Face excommunication, loss of community, and stigmatization for insisting the prior revelation remains binding.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, doctrinal_hardliners, payer,
    moderate, biographical, identity_locked, national).

% Applied the Edmunds-Tucker Act and anti-polygamy prosecution to seize church assets and imprison practitioners; created the existential pressure that activated the prophetic override mechanism. Does not participate in the theological logic but observes and enforces civil compliance.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eternal_marriage_covenant__prophetic_override_reading, church_president_prophet).
narrative_ontology:fixing_cost_class(eternal_marriage_covenant__prophetic_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the institutional church under existential legal and political threat by allowing the leadership to pivot doctrinal practice without removing the foundational text from canon, thereby preventing schism or federal destruction.
% TRANSFER_FUNCTION: Moves authority from the fixed text of D&C 132 to the living prophet; moves the compliance burden and cognitive cost from the institutional center to practitioners who must abandon their covenants and to hardliners who must accept contradiction or leave.
% ABSENT_VOICES: Imprisoned polygamists and their families who could not comply without destroying their households; fundamentalist dissenters who formed splinter movements after excommunication; federal prosecutors who enforced the external pressure but were excluded from the theological justification.
% DISAPPEARANCE_RATIONALE: If the prophetic override doctrine disappeared, the 1890 pivot could not have occurred without either renouncing D&C 132 entirely (causing theological schism) or maintaining polygamy and facing federal dissolution. The institutional structure depended on this specific mechanism.
% FOUNDING_PROBLEM: How to maintain doctrinal coherence and collective survival when a prior revelation commanding plural marriage as essential for exaltation becomes existentially threatening under federal anti-polygamy enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Federal congressional records and prosecutorial archives document the asset seizures and imprisonment campaign. Excommunicant memoirs and fundamentalist histories attest the existential pressure from outside the benefiting hierarchy. Independent academic historians corroborate the threat to church property and leadership liberty.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial because the doctrine extracts from practitioners and hardliners by nullifying the practical force of a covenant they had been taught was essential for exaltation, while leaving the text in place. Suppression (0.75) is high because the constraint's persistence depends on federal prosecution plus ecclesiastical discipline against resisters. Theater_ratio (0.40) reflects that the prophetic authority performance is real but increasingly ritualized after the survival crisis passes. Accessibility_collapse (0.80) is high because once the prophetic override is accepted as legitimate, alternatives collapse within the faith framework. Resistance (0.55) captures the hardliner/fundamentalist schism. The claim/metric gap is deliberate: the church frames this as necessary survival coordination, while the metrics describe asymmetric extraction from identity-locked payers.
 *
 * PERSPECTIVAL GAP:
 *   The hierarchy and membership experience the constraint as salvation of the collective; the practitioner and hardliner seats experience it as doctrinal betrayal and extraction of their prior compliance. The engine computes this divergence from the structural dataâlow directionality for institutional beneficiaries, high directionality for identity-locked payers.
 *
 * DIRECTIONALITY LOGIC:
 *   The prophet and institutional leadership are structural beneficiaries (low d): they gain survival and authority. Polygamist practitioners and doctrinal hardliners are targets (high d): they bear the cost of the reversal and are identity-locked into the community by family structure and theological self-concept. Membership sits near symmetric: they benefit from survival but bear the cost of accepting prophetic fiat over textual permanence. Federal government is analytical, outside the directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the constraint as pure coordination (rope) because the cost is asymmetrically borne by practitioners who had acted on the prior revelation, and it prevents mislabeling as pure extraction (snare) because the federal threat to institutional destruction was historically real. The classification captures both the genuine coordination problem (collective survival) and the asymmetric extraction (individual covenant abandonment).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_override_kernel_location,
    'Does the 1890 Manifesto constitute doctrinal supersession by prophetic authority, temporary civil accommodation, or immutable commandment defiance?',
    'Theological analysis of official First Presidency statements, subsequent canonized declarations, and the 1904 Second Manifesto to determine whether the church treats the original Manifesto as nullifying D&C 132, suspending it, or merely obeying civil law.',
    'Resolves which kernel reading (prophetic_override, temporal_accommodation, or immutable_commandment) best matches the institutional commitment structure; shifts classification between tangled_rope and scaffold or snare depending on the locus of authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_override_kernel_location, conceptual, 'Structural ambiguity between prophetic supersession and civil accommodation readings of the same event.').

omega_variable(
    internalized_suppression_hardliners,
    'Is the compliance of doctrinal hardliners who remain in the church due to internalized prophetic loyalty or structural coercion (excommunication threat)?',
    'Post-exit trajectory analysis: do hardliners who leave the church continue to accept the prophetic override as legitimate, or do they reject it entirely? Persistent internalized acceptance indicates suppression exceeds structural measure.',
    'If internalized, effective extraction is higher than structural suppression suggests; if purely structural, the constraint is more brittle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_hardliners, empirical, 'Ambiguity between internalized and structural suppression mechanisms for dissenters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t0, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(eter_tr_t5, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(eter_tr_t10, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(eter_tr_t15, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 15, 0.45).
narrative_ontology:measurement(eter_tr_t20, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(eter_tr_t25, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(eter_tr_t30, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(eter_be_t0, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(eter_be_t5, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(eter_be_t10, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(eter_be_t15, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(eter_be_t20, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(eter_be_t25, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(eter_be_t30, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t0, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(eter_su_t5, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(eter_su_t10, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(eter_su_t15, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 15, 0.85).
narrative_ontology:measurement(eter_su_t20, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(eter_su_t25, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 25, 0.6).
narrative_ontology:measurement(eter_su_t30, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the eternal_marriage_covenant kernel. The three readings (immutable_commandment, prophetic_override, temporal_accommodation) form a constraint family decomposed from the single natural-language concept of the 1890 Manifesto and its theological status. Each reading carries a distinct epsilon, beneficiary/victim structure, and structural classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
