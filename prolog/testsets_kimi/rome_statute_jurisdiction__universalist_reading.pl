% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__universalist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__universalist_reading
 *   human_readable: Rome Statute Universalist Jurisdiction Mandate
 *   domain: international_law/treaty_interpretation
 *
 * SUMMARY:
 *   The Rome Statute's universalist reading asserts that the International
 *   Criminal Court possesses jurisdiction over nationals of non-party states
 *   when crimes occur on the territory of a state party or when the UN
 *   Security Council refers a situation under Chapter VII. This reading
 *   treats the Statute as establishing a universal criminal justice mandate
 *   that transcends sovereign consent, grounding itself in the Nuremberg
 *   legacy and the erga omnes nature of core crimes. Non-party
 *   statesâincluding major powers and the African Union blocâhave
 *   contested this reading as an unauthorized extraction of sovereignty. The
 *   constraint coordinates a genuine anti-impunity function for victims while
 *   simultaneously extracting sovereign authority from non-consenting states.
 *
 * KEY AGENTS:
 *   - icc_office_of_prosecutor: Agenda setter (institutional/global) â asserts and enforces universal jurisdiction over non-party nationals
 *   - non_party_states: Primary payer (institutional/global) â sovereignty overridden without consent, bear the structural cost
 *   - victims_of_core_crimes: Primary beneficiary (powerless/local) â promised recourse regardless of state consent
 *   - unsc_permanent_members: Secondary agenda setter and selective beneficiary (institutional/global) â trigger jurisdiction over others while shielding themselves via veto
 *   - state_parties: Secondary beneficiary (institutional/global) â benefit from expanded norm coverage without bearing the sovereignty penalty
 *   - nationals_of_non_party_states: Payer (powerless/national) â face prosecution without their state having ratified the Statute
 *   - african_union_bloc: Excluded voice (organized/continental) â challenges asymmetric application but is marginalized in interpretive discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, 0.65).
domain_priors:suppression_score(rome_statute_jurisdiction__universalist_reading, 0.58).
domain_priors:theater_ratio(rome_statute_jurisdiction__universalist_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__universalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__universalist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__universalist_reading, "Rome Statute Universalist Jurisdiction Mandate").
narrative_ontology:topic_domain(rome_statute_jurisdiction__universalist_reading, "international_law/treaty_interpretation").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__universalist_reading, '26e79727-a6ba-4709-83d7-471926754804').
narrative_ontology:cs_kernel_codification('26e79727-a6ba-4709-83d7-471926754804', formalized).
narrative_ontology:cs_authority_grounding('26e79727-a6ba-4709-83d7-471926754804', lineage).
narrative_ontology:cs_interpretation_layer_present('26e79727-a6ba-4709-83d7-471926754804').
narrative_ontology:cs_reading_relation('26e79727-a6ba-4709-83d7-471926754804', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('26e79727-a6ba-4709-83d7-471926754804', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('26e79727-a6ba-4709-83d7-471926754804', foundational, universal_jurisdiction_transcends_consent).
narrative_ontology:cs_axiom_status(universal_jurisdiction_transcends_consent, holdable).
narrative_ontology:cs_axiom_grounding('26e79727-a6ba-4709-83d7-471926754804', universal_jurisdiction_transcends_consent, deontological).
narrative_ontology:cs_reference_frame('26e79727-a6ba-4709-83d7-471926754804', international_criminal_supremacy).
narrative_ontology:cs_drift_state('26e79727-a6ba-4709-83d7-471926754804', contemporary_multipolar_order, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('26e79727-a6ba-4709-83d7-471926754804', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, state_parties).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__universalist_reading, nationals_of_non_party_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts jurisdiction over nationals of non-party states for core crimes committed on the territory of a state party or pursuant to a UN Security Council referral; requests state cooperation for arrests and evidence collection; depends on Pre-Trial Chamber authorization and voluntary or coerced state compliance to give effect to jurisdictional claims.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, icc_office_of_prosecutor, agenda_setter,
    institutional, generational, constrained, global).

% Survivors and family members seeking accountability for genocide, crimes against humanity, and war crimes; the universalist reading promises them recourse to the ICC even when their home state has not joined the Statute or is unwilling to investigate; often remain in conflict zones or displacement without functioning domestic courts.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, victims_of_core_crimes, beneficiary,
    powerless, biographical, trapped, local).

% States that have not ratified the Rome Statute yet face ICC jurisdictional claims over their nationals and territory; experience the universalist reading as an override of sovereign consent; respond through non-cooperation, bilateral immunity agreements, rhetorical opposition, and withdrawal from optional protocols, but cannot unilaterally nullify territorial or UNSC-triggered jurisdiction.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, non_party_states, payer,
    institutional, generational, constrained, global).

% States that ratified the Rome Statute and fund the ICC; benefit from an international criminal justice architecture that extends to non-party conduct on their territory or referred by the Council; shielded from the sovereignty costs borne by non-parties while participating in the normative framework.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, state_parties, beneficiary,
    institutional, generational, constrained, global).

% Can activate ICC jurisdiction over non-party states through Chapter VII referrals without the target stateâs consent; simultaneously insulate themselves and allies from equivalent exposure via veto power; exercise highly selective triggering that concentrates universalist jurisdiction on geopolitically disadvantaged situations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__universalist_reading, unsc_permanent_members, beneficiary).

% Individual officials and military personnel from non-party states who become subject to ICC investigation and prosecution without their state having ratified the Statute; lack the diplomatic shield or complementarity bargaining power available to some state-party nationals; face arrest risks if they travel to cooperating jurisdictions.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, nationals_of_non_party_states, payer,
    powerless, biographical, trapped, national).

% Collectively contested the asymmetric application of universalist jurisdiction toward African states and leaders; advanced counter-narratives of neo-colonial selectivity and pushed for head-of-state immunity and mass withdrawal strategies; largely excluded from the authoritative interpretive discourse that sets the boundaries of universalist claims despite bearing the brunt of their exercise.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__universalist_reading, african_union_bloc, excluded,
    organized, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__universalist_reading, icc_office_of_prosecutor).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__universalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a permanent international judicial mechanism to prosecute genocide, crimes against humanity, war crimes, and aggression, eliminating safe havens for perpetrators when national courts are unwilling or unable to act, and extending this reach to non-party contexts through territoriality or Security Council referral.
% TRANSFER_FUNCTION: Moves prosecutorial authority and sovereign immunity protections from non-party states and their nationals to the ICC Office of the Prosecutor and Chambers, activated by territorial presence or UNSC decision rather than state consent.
% ABSENT_VOICES: Non-party states and the African Union bloc are structurally underrepresented in the interpretive community that consolidates the universalist reading; their sovereignty objections are treated as resistance to justice rather than as legitimate legal positions within the same framework.
% DISAPPEARANCE_RATIONALE: If the universalist mandate disappeared overnight, non-party states would regain exclusive jurisdiction over their nationals, outstanding arrest warrants against Sudanese, Libyan, and other non-party indictees would lose legal basis, the architecture of international criminal law would revert to ad hoc tribunals requiring explicit creation, and victims in non-party territories would lose their existing pathway to international accountability.
% FOUNDING_PROBLEM: Systematic impunity for core international crimes when national courts are unwilling or unable to prosecute, exemplified by the limitations of the Nuremberg and Tokyo ad hoc tribunals and the failure of domestic accountability during the Yugoslav wars and Rwandan genocide.
% FOUNDING_PROBLEM_CORROBORATION: State parties and human rights NGOs attest the problem remains live in Sudan, Myanmar, and Gaza. Non-party states (United States, Russia, China, India) and the African Union attest that the problem is addressed through alternative mechanisms or that the universalist solution itself produces new harms; the disagreement is corroborated by competing empirical claims about deterrence and selectivity from outside the beneficiary set.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__universalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__universalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__universalist_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__universalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__universalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the substantial sovereignty cost imposed on non-party states, though mitigated by procedural complementarity and the rarity of successful enforcement. Suppression (0.58) captures the diplomatic and institutional pressure on non-parties to cooperate, amplified by the normative framing that resistance equals impunity. Theater ratio (0.55) is elevated because the universalist reading generates more arrest warrants and jurisdictional assertions than actual trials or arrests for non-party nationals, producing a performance of authority that exceeds functional enforcement capacity. Accessibility collapse (0.45) is moderate because alternativesânational courts, ad hoc tribunals, hybrid mechanismsâpersist and are actively used. Resistance (0.72) is high and rising, driven by the African Union, U.S. bilateral immunity agreements, and Russian withdrawal. The claim is tangled_rope because the structure contains both a genuine coordination function (ending impunity) and asymmetric extraction (sovereignty override for non-parties), held together by active institutional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (ICC Prosecutor) experiences the constraint as necessary legal authority to close impunity gaps; the payer seat (non-party states) experiences it as an illegitimate sovereignty grab. The victim seat experiences hope for justice mixed with the reality of non-enforcement against powerful non-party indictees. The UNSC permanent member seat experiences the constraint as a selectively deployable tool. These divergences are structural: the same legal text produces subsidy (jurisdiction) for the Prosecutor, cost (sovereignty loss) for non-parties, selective leverage for the Council, and partial benefit (symbolic recourse) for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC Prosecutor and state parties are beneficiaries of expanded jurisdiction (low d), while non-party states and their nationals are the targets (high d). Victims sit closer to the beneficiary end despite their powerlessness because the constraint is structurally oriented toward delivering them justice. The UNSC permanent members occupy a unique position: they can trigger the constraint against others while shielding themselves via veto, giving them an arbitrage-grade exit that pulls their d toward the beneficiary end despite their institutional power. The African Union bloc is excluded from the interpretive apparatus, leaving its d underdetermined by the formal structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist reading prevents mislabeling by requiring both coordination and extraction. A pure coordination reading (rope) would ignore the sovereignty override and the resistance it generates; a pure extraction reading (snare) would ignore the genuine victim recourse and the legal framework's aspiration to end impunity. The tangled_rope classification captures that the mandate has not atrophied into pure theater (piton) because enforcement remains active and the founding problem (impunity) is still live, even if contested. It is not a scaffold because no sunset clause exists and the universalist claim is presented as permanent international law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_text_universality_ambiguity,
    'Does the Rome Statute text (Articles 12 and 13) inherently authorize jurisdiction over non-party nationals, or does the universalist reading project authority beyond the treaty''s conventional consent structure?',
    'Authoritative judicial interpretation tracing by the ICC Appeals Chamber or an ICJ advisory opinion explicitly addressing the textual basis; complemented by systematic state practice and opinio juris analysis.',
    'If the text does not support the reading, the universalist claim is an extraction of authority from ambiguous language rather than coordination grounded in treaty law; this would shift classification toward higher theater and potentially toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_text_universality_ambiguity, conceptual, 'Whether the universalist mandate is textually grounded or projected onto ambiguous treaty language.').

omega_variable(
    enforcement_without_arrest_capacity,
    'Does the universalist reading produce genuine coordination (justice for victims) when the ICC lacks enforcement capacity against non-party states, or does it primarily generate symbolic authority and diplomatic leverage?',
    'Empirical measurement of completed prosecutions, arrests, and victim participation rates in UNSC-referred versus state-party-referred situations.',
    'If justice outcomes are negligible, the coordination story is largely cover for institutional authority expansion, meaning extraction from non-party sovereignty produces few corresponding victim benefits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_without_arrest_capacity, empirical, 'Whether universalist jurisdiction delivers tangible justice outcomes or remains symbolic.').

omega_variable(
    kernel_reading_sibling_boundary,
    'Is the universalist reading structurally separable from the hybrid complementarity reading, or do they collapse into one another when the complementarity mechanism is applied to non-party situations?',
    'Comparative analysis of jurisdictional assertions in Kenya, Sudan, and Palestine situations to determine whether complementarity proceduralizes the universalist claim or genuinely limits non-party exposure.',
    'If complementarity merely proceduralizes universalism, the hybrid reading is theater and the universalist reading captures the real constraint structure; if complementarity genuinely limits exposure, the universalist reading overstates the extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_sibling_boundary, conceptual, 'Structural boundary between universalist and hybrid readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__universalist_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t0, rome_statute_jurisdiction__universalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(rome_tr_t4, rome_statute_jurisdiction__universalist_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(rome_tr_t8, rome_statute_jurisdiction__universalist_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(rome_tr_t12, rome_statute_jurisdiction__universalist_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(rome_tr_t16, rome_statute_jurisdiction__universalist_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(rome_tr_t20, rome_statute_jurisdiction__universalist_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(rome_tr_t24, rome_statute_jurisdiction__universalist_reading, theater_ratio, 24, 0.55).

% Extraction over time
narrative_ontology:measurement(rome_be_t0, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rome_be_t4, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(rome_be_t8, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(rome_be_t12, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(rome_be_t16, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(rome_be_t20, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(rome_be_t24, rome_statute_jurisdiction__universalist_reading, base_extractiveness, 24, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rome_statute_jurisdiction__universalist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__universalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__universalist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

% DUAL FORMULATION NOTE:
% The Rome Statute jurisdiction kernel decomposes into three structurally distinct readings: the universalist reading (this constraint), the sovereigntist reading, and the hybrid complementarity reading. Each reading carries a distinct epsilon, beneficiary/victim structure, and classification. They share the same treaty text as kernel but emit different constraints based on interpretive commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
