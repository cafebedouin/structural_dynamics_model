% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: CA3 Scope via Customary International Law Evolution
 *   domain: legal/humanitarian
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions imposes minimum
 *   humanitarian protections in armed conflicts "not of an international
 *   character." The constraint's scope—which conflicts qualify—is contested.
 *   This story instantiates the ICRC customary reading: CA3 scope is
 *   determined by evolving state practice and opinio juris, monitored and
 *   codified by the ICRC through customary law studies. This reading treats
 *   CA3 as a procedurally flexible coordination mechanism that expands
 *   without formal amendment as states' actual behavior and legal opinions
 *   shift. It contrasts with two sibling readings: the state-centric reading
 *   (CA3 applies only to conflicts meeting strict organization/intensity
 *   thresholds, excluding low-level violence) and the expansive human rights
 *   reading (CA3 applies to any organized armed violence as a non-derogable
 *   floor, regardless of classification schemes). The three readings compete
 *   in state practice and interpretive authority; they instantiate different
 *   constraints with different beneficiaries and extraction profiles.
 *
 * KEY AGENTS:
 *   - ICRC as interpretive custodian and monitor of state practice
 *   - States collectively as generators of opinio juris through conduct and legal statements
 *   - Non-state armed groups, structurally excluded from opinio juris formation but bound by the emerging custom
 *   - Civilian populations in non-international conflicts, beneficiaries of scope expansion but without voice
 *   - State security establishments paying the cost of operational constraint
 *   - International courts applying CA3 beyond its treaty-text scope
 *   - Human rights advocates documenting evidence but excluded from formal custom-determination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.38).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.28).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "CA3 Scope via Customary International Law Evolution").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "legal/humanitarian").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'c5659b9b-aad3-4239-8583-64139728dc0c').
narrative_ontology:cs_kernel_codification('c5659b9b-aad3-4239-8583-64139728dc0c', fixed_text).
narrative_ontology:cs_authority_grounding('c5659b9b-aad3-4239-8583-64139728dc0c', extraction).
narrative_ontology:cs_interpretation_layer_present('c5659b9b-aad3-4239-8583-64139728dc0c').
narrative_ontology:cs_reading_relation('c5659b9b-aad3-4239-8583-64139728dc0c', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5659b9b-aad3-4239-8583-64139728dc0c', common_article_3_scope__expansive_human_rights_reading, influences).
narrative_ontology:cs_axiom('c5659b9b-aad3-4239-8583-64139728dc0c', foundational, customary_law_as_independent_source).
narrative_ontology:cs_axiom_status(customary_law_as_independent_source, holdable).
narrative_ontology:cs_axiom_grounding('c5659b9b-aad3-4239-8583-64139728dc0c', customary_law_as_independent_source, conventional).
narrative_ontology:cs_axiom('c5659b9b-aad3-4239-8583-64139728dc0c', foundational, state_practice_as_evidence_of_evolution).
narrative_ontology:cs_axiom_status(state_practice_as_evidence_of_evolution, holdable).
narrative_ontology:cs_axiom_grounding('c5659b9b-aad3-4239-8583-64139728dc0c', state_practice_as_evidence_of_evolution, conventional).
narrative_ontology:cs_axiom('c5659b9b-aad3-4239-8583-64139728dc0c', secondary, icrc_as_custodian_of_custom_monitoring).
narrative_ontology:cs_axiom_status(icrc_as_custodian_of_custom_monitoring, holdable).
narrative_ontology:cs_axiom_grounding('c5659b9b-aad3-4239-8583-64139728dc0c', icrc_as_custodian_of_custom_monitoring, conventional).
narrative_ontology:cs_reference_frame('c5659b9b-aad3-4239-8583-64139728dc0c', treaty_text_as_living_baseline).
narrative_ontology:cs_drift_state('c5659b9b-aad3-4239-8583-64139728dc0c', contemporary_non_international_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c5659b9b-aad3-4239-8583-64139728dc0c', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc_interpretive_authority).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, state_actors_with_opinio_juris_voice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, affected_civilian_populations).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, state_actors_with_opinio_juris_voice).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, state_security_establishment).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_international_law_as_binding_source).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, gradual_norm_expansion_without_amendment).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, state_practice_opinio_juris_nexus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The International Committee of the Red Cross operates as the custodian of CA3 interpretation through its commentaries, customary law studies (particularly the 2005 ICRC Study on Customary International Humanitarian Law), and advisory role to states. It monitors state practice through factual reporting, synthesizes opinio juris from state conduct and statements, and publishes evolving interpretations that influence subsequent state behavior. The ICRC itself does not bear costs of the constraint's scope—it administers interpretation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc_interpretive_authority, agenda_setter,
    institutional, generational, analytical, universal).

% States contribute to and are bound by the customary law evolution. Those whose actual practice aligns with emerging norms benefit from legitimacy and predictability as the customary rule solidifies around their conduct. States whose practice diverges (e.g., denying CA3 applicability to asymmetric conflicts) pay through reputational cost, ICRC scrutiny, and the gradual delegitimation of their position as other states coalesce around an expanded interpretation. States have exit in theory (withdraw from customary law) but it is identity-locked: sovereignty itself depends on recognition within the international legal order.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, state_actors_with_opinio_juris_voice, beneficiary,
    organized, generational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__icrc_customary_reading, state_actors_with_opinio_juris_voice, payer).

% Non-state armed groups cannot formally participate in opinio juris formation (which is a state practice measure), yet the customary scope evolution directly determines whether CA3 binds them. They are structurally excluded from the interpretive process that sets their obligations. Their inclusion in scope depends on states and the ICRC recognizing their practice as evidence of custom, not on their own voice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, armed_groups_non_state_actors, excluded,
    moderate, biographical, trapped, universal).

% The primary intended beneficiaries of CA3 protections. The customary scope expansion (via ICRC-tracked state practice) determines whether they receive minimum humanitarian guarantees in their actual conflict. A narrower state-centric reading would exclude many civilians in non-traditional conflicts from CA3 coverage; a broader customary reading based on evolving opinio juris includes them. They have no formal voice in scope determination.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, affected_civilian_populations, beneficiary,
    powerless, immediate, trapped, universal).

% NGOs and human rights bodies argue for expansive CA3 application but are excluded from formal opinio juris determination, which remains state-centric by definition. They influence the interpretive environment through advocacy and case documentation but do not vote on custom.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, human_rights_advocacy_communities, excluded,
    organized, generational, constrained, global).

% Military and intelligence officials in states whose counterinsurgency and counterterrorism doctrines depend on narrow CA3 scope interpretation. A customary expansion that brings non-international armed conflicts under full CA3 scrutiny increases operational costs, requires training in humanitarian law, enables greater ICRC access, and constrains tactical options. They pay through operational friction but cannot exit (locked into state's international legal obligations).
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, state_security_establishment, payer,
    powerful, biographical, constrained, national).

% Academic commentators on customary international law. They synthesize state practice, debate opinio juris evidence, publish in law reviews, and influence how the next generation of state lawyers understands the evolving scope. They are neither beneficiaries nor payers but contribute to the epistemic environment the ICRC reads.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_legal_scholars, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__icrc_customary_reading, icrc_interpretive_authority).
narrative_ontology:fixing_cost_class(common_article_3_scope__icrc_customary_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for expanding humanitarian protection without requiring formal treaty amendment: as states' actual behavior evolves and legal opinions shift, the customary reading of CA3 adjusts to recognize new norms, allowing the international legal order to adapt to new conflict forms (asymmetric warfare, internal conflicts, transnational armed groups) without the friction and veto power of formal treaty renegotiation.
% TRANSFER_FUNCTION: Transfers interpretive authority from formal treaty text (which is static) to state practice and opinio juris (which are dynamic). The ICRC codifies and legitimizes this transfer by publishing customary law studies that frame evolving state conduct as evidence of binding custom, shifting the burden: a state must now affirmatively justify deviations from the emerging consensus rather than invoking the treaty's literal scope.
% ABSENT_VOICES: Non-state armed groups have no vote in opinio juris formation, despite being bound by an evolving custom they cannot shape. Affected civilian populations in non-international conflicts cannot participate in determining whether they receive CA3 protections. Human rights advocacy communities document evidence but do not adjudicate custom. The excluded seats would argue for direct representation in scope determination rather than mediation through state practice alone.
% DISAPPEARANCE_RATIONALE: If the ICRC-tracked customary reading vanished and CA3 scope reverted to formal treaty text only, states would likely narrow their interpretation to the treaty's literal language, excluding many contemporary conflicts. Humanitarian protection in internal armed conflicts, asymmetric warfare, and hybrid conflicts would contract sharply. The mechanism for norm evolution without amendment would disappear, forcing either deadlocked treaty renegotiation or acceptance of a fixed, outdated scope.
% FOUNDING_PROBLEM: CA3 was drafted in 1949 to address conflicts between recognized states and between states and organized resistance movements. By the 1970s-80s, most armed conflicts were non-international (internal or involving non-state actors), yet the treaty text's scope language had not adapted. A formal amendment process was blocked by states with strategic interests in narrow application. The constraint emerged to allow customary evolution to solve what formal amendment could not.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's 2005 Customary International Humanitarian Law study explicitly identifies this gap: state practice has diverged from treaty text, courts have applied CA3 beyond its literal scope in non-international armed conflicts, and humanitarian reality no longer matches the 1949 categories. International Court of Justice rulings (Nicaragua case, 2004 Wall Advisory Opinion) have affirmed customary law as a binding independent source. States themselves treat customary CA3 as binding in their conduct, even where they resist formal amendment. Scholars documenting actual military compliance report that states follow evolved customary norms despite treaty text ambiguity.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint benefits the ICRC's interpretive authority and states that want legitimacy for expanded protection, while imposing cost on states whose doctrine relies on narrow scope. The cost is asymmetric: states with aggressive counterinsurgency doctrines pay more than humanitarian-minded states. Suppression is low (0.28) because the mechanism is not coercive—it operates through interpretive consensus and state practice alignment, not through force. Theater is minimal (0.12) because the constraint's operation is substantively what it claims to be: reading evolved state practice as custom. The measurement series show extraction rising from 1949-2005 (the period of actual conflict-form shift and ICRC study publication) then plateauing 2005-2024 (the reading has stabilized in practice; further expansion requires new state conduct, not ICRC reinterpretation). Suppression rises slightly through the same arc, then flattens, consistent with an interpretive stabilization. The grid's one shared time set covers all three metrics at each point.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC and humanitarian-aligned states' perspective, this is genuine coordination: a legal mechanism allowing humanitarian norms to evolve to match changed conflict realities without formal amendment deadlock. From the narrow-scope state's perspective (especially those prosecuting counterinsurgency), this is extraction masked as customary law: an interpretive process that ratchets up their humanitarian obligations without formal consent, locking them into practices that originated elsewhere. From non-state armed groups' perspective, this is pure extraction: binding custom they cannot shape, determined entirely by states' practice and legal opinions. The engine computes these divergent types from the structural data—each seat's power, exit options, beneficiary/victim status—independently of the claimed type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality traces structural asymmetry. The ICRC benefits (collects interpretive authority and legitimacy) at low cost (research and publication, no operational risk—analytical exit_options). States form a bloc with mixed directionality: humanitarian-aligned states benefit from predictable, expansive interpretation that matches their conduct; narrow-scope states bear cost (operational friction, delegitimation for deviations). Non-state armed groups are full targets (bound by custom they cannot influence, identity_locked in conflict). Civilian populations are beneficiaries (protected) but powerless and trapped. This structure explains why the constraint is claimed as rope by the benefiting seats but computes as tangled_rope or snare from the payer seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (conflicts evolve faster than treaties; formal amendment is blocked) is live: non-international armed conflicts still dominate, asymmetric warfare is still the norm, and formal amendment of CA3 remains blocked by states with veto interests. The constraint's mandate—providing a mechanism for humanitarian norm expansion without amendment—has NOT outlived its function. Mandatrophy would arise only if: (a) formal amendment becomes politically feasible and absorbs the customary function, or (b) conflicts stabilize into a fixed set and future adaptation becomes unnecessary. Neither has occurred. The constraint is performing its founding function: enabling incremental humanitarian norm expansion through monitored state practice. Classification as rope (genuine coordination solving a real deadlock) is supported by the mandatrophy state being live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opinio_juris_measurement_ambiguity,
    'How much of the ICRC''s ''opinio juris'' reading reflects actual state legal opinion versus state conduct ICRC interprets as evidence of opinion?',
    'Comparative analysis of state legal briefs, treaty negotiations, and official statements versus ICRC-inferred custom from military conduct and treaty practice.',
    'If opinio juris is largely ICRC-inferred from conduct (rather than formally expressed), the constraint becomes more extractive: states are bound by norms they never formally endorsed. If opinio juris is documented in state statements, the coordination framing is stronger. The ambiguity maps to omega_c: framing under-determination (is custom read from conduct or from declaration?).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opinio_juris_measurement_ambiguity, conceptual, 'Whether opinio juris is state-expressed or ICRC-inferred from conduct.').

omega_variable(
    non_state_actor_binding_legitimacy,
    'By what legitimate mechanism are non-state armed groups bound by a customary norm (opinio juris) to which they cannot contribute? Is this extraction or enforcement of pre-existing law?',
    'Philosophical/legal analysis of whether customary international law binds non-state actors independently of their consent. Empirical check: do non-state actors treated as bound by CA3 customary reading achieve greater compliance than those the reading excludes?',
    'If binding non-state actors without their voice is extraction, the constraint''s type from the non-state seat shifts toward snare. If customary law legitimately binds all parties regardless of voice (a structural rule of the international legal system), the constraint is a roof, not extractive. This maps omega_c (is the legitimacy frame pre-existing or constructed-for-this-constraint?).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_state_actor_binding_legitimacy, conceptual, 'Whether non-state binding via custom is legitimate constraint or exclusionary extraction.').

omega_variable(
    icrc_epistemic_monopoly_drift,
    'Over the 1949-2024 interval, has the ICRC''s role drifted from synthesizing state practice to actively directing it—moving from custodian to agenda-setter?',
    'Temporal analysis of ICRC publications and their uptake: do states cite ICRC studies as authoritative sources of custom (custodian role) or do ICRC studies appear to precede and shape state behavior (agenda-setter role)?',
    'If drift toward agenda-setter has occurred, extractiveness increases (the ICRC collects interpretive authority not merely from monitoring state practice but from directing future practice). The constraint shifts from rope (coordination mechanism) toward tangled_rope (the ICRC enforces a reading it also authors).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(icrc_epistemic_monopoly_drift, empirical, 'Whether ICRC role has drifted from custodian to agenda-setter over the interval.').

omega_variable(
    kernel_reading_relation_foreclosure,
    'Does the ICRC customary reading logically foreclose the state-centric reading, or can both coexist as competing frameworks within states'' opinio juris formation?',
    'Examine whether a state holding the state-centric reading (narrow scope, intensity thresholds) can simultaneously hold that state practice creates customary law (ICRC framework). If yes, coexistence is possible (different frames, same legal reasoning). If no, foreclosure is possible (accepting customary law as evolving rules out fixed thresholds).',
    'If foreclosure is real, the ICRC reading is structurally displacing the state-centric reading, not coexisting. This is a kernel-level finding about which reading dominates. If coexistence is possible, both readings remain live and contend within the international legal order.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relation_foreclosure, conceptual, 'Whether ICRC and state-centric readings are logically compatible or mutually foreclosing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.08).
narrative_ontology:measurement_basis(comm_tr_t1949, projected).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__icrc_customary_reading, theater_ratio, 1977, 0.1).
narrative_ontology:measurement_basis(comm_tr_t1977, observed).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__icrc_customary_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement_basis(comm_tr_t1990, observed).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__icrc_customary_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement_basis(comm_tr_t2005, observed).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__icrc_customary_reading, theater_ratio, 2015, 0.12).
narrative_ontology:measurement_basis(comm_tr_t2015, observed).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.12).
narrative_ontology:measurement_basis(comm_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.15).
narrative_ontology:measurement_basis(comm_be_t1949, projected).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1977, 0.22).
narrative_ontology:measurement_basis(comm_be_t1977, observed).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement_basis(comm_be_t1990, observed).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2005, 0.36).
narrative_ontology:measurement_basis(comm_be_t2005, observed).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2015, 0.37).
narrative_ontology:measurement_basis(comm_be_t2015, observed).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.38).
narrative_ontology:measurement_basis(comm_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement_basis(comm_su_t1949, projected).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1977, 0.24).
narrative_ontology:measurement_basis(comm_su_t1977, observed).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1990, 0.26).
narrative_ontology:measurement_basis(comm_su_t1990, observed).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2005, 0.27).
narrative_ontology:measurement_basis(comm_su_t2005, observed).
narrative_ontology:measurement(comm_su_t2015, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2015, 0.28).
narrative_ontology:measurement_basis(comm_su_t2015, observed).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.28).
narrative_ontology:measurement_basis(comm_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__icrc_customary_reading, 0.18).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, icrc_customary_law_study_2005).

% DUAL FORMULATION NOTE:
% Common Article 3 scope is contested across three readings instantiated as separate constraints. This story (ICRC customary reading) treats scope as evolving via state practice and opinio juris, coordinated by ICRC monitoring. The state-centric reading treats scope as fixed by treaty text and formalized thresholds. The expansive human rights reading treats scope as non-derogable minimum applying to all organized violence. The three constraints share a kernel (CA3 text, the international legal question 'which conflicts?') but diverge sharply on mechanism (evolving custom vs. fixed text vs. rights floor), beneficiaries (ICRC + aligned states vs. narrow-scope states vs. all affected populations), and extraction (moderate in customary reading, lower in state-centric, variable in human rights). Network edges link the readings: the ICRC reading influences both siblings by establishing customary-law precedent that pressure-tests their claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
