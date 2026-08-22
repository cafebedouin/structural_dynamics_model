% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 (Expansive Human Rights Reading) — Universal Humanitarian Standards Floor
 *   domain: legal/humanitarian
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions is a contested kernel: which
 *   armed situations trigger its protections depends on how one reads the
 *   scope of 'armed conflict.' This constraint instantiates the expansive
 *   human rights reading: CA3 applies to any organized armed violence —
 *   regardless of intensity threshold, state recognition, or formal
 *   classification — as a floor of minimum humanitarian standards. The
 *   reading treats organizational capacity and violence scale as sufficient
 *   triggers, independent of the state's characterization. This creates a
 *   tangled_rope: it coordinates a universal humanitarian minimum
 *   (beneficiaries: detained persons, civilians, monitoring bodies) while
 *   extracting compliance costs from both state security forces and non-state
 *   armed groups (victims). The state-centric reading and ICRC customary
 *   reading are separate constraints in the same kernel family; this reading
 *   does not describe them — it instantiates only the expansive reading's
 *   structural claim and beneficiary/victim profile. The constraint coexists
 *   with the state-centric reading (different parties hold both) and
 *   influences the customary reading (by establishing political pressure for
 *   opinio juris to shift toward broader application).
 *
 * KEY AGENTS:
 *   - detained_persons: powerless, trapped (depend entirely on external enforcement for protections)
 *   - affected_civilian_populations: powerless, constrained (protected by virtue of presence in organized armed situation, exit difficult)
 *   - state_security_forces: institutional, constrained (operational obligations and accountability exposure under expansive reading)
 *   - non_state_armed_groups: organized, constrained (directly bound by CA3 minimums without prior state recognition)
 *   - international_humanitarian_monitoring_bodies (ICRC, UN): institutional, analytical (gain mandate clarity and monitoring authority)
 *   - international_criminal_court_and_tribunals: institutional, analytical (prosecutorial authority over CA3 violations expands)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.62).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.71).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 (Expansive Human Rights Reading) — Universal Humanitarian Standards Floor").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "legal/humanitarian").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '7b6c32ce-8937-4865-81b0-98c71465e924').
narrative_ontology:cs_kernel_codification('7b6c32ce-8937-4865-81b0-98c71465e924', fixed_text).
narrative_ontology:cs_authority_grounding('7b6c32ce-8937-4865-81b0-98c71465e924', lineage).
narrative_ontology:cs_interpretation_layer_present('7b6c32ce-8937-4865-81b0-98c71465e924').
narrative_ontology:cs_reading_relation('7b6c32ce-8937-4865-81b0-98c71465e924', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('7b6c32ce-8937-4865-81b0-98c71465e924', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('7b6c32ce-8937-4865-81b0-98c71465e924', foundational, universal_humanitarian_floor_all_organized_violence).
narrative_ontology:cs_axiom_status(universal_humanitarian_floor_all_organized_violence, holdable).
narrative_ontology:cs_axiom_grounding('7b6c32ce-8937-4865-81b0-98c71465e924', universal_humanitarian_floor_all_organized_violence, deontological).
narrative_ontology:cs_axiom('7b6c32ce-8937-4865-81b0-98c71465e924', foundational, classification_independent_trigger).
narrative_ontology:cs_axiom_status(classification_independent_trigger, holdable).
narrative_ontology:cs_axiom_grounding('7b6c32ce-8937-4865-81b0-98c71465e924', classification_independent_trigger, deontological).
narrative_ontology:cs_reference_frame('7b6c32ce-8937-4865-81b0-98c71465e924', universal_humanitarian_minimum).
narrative_ontology:cs_drift_state('7b6c32ce-8937-4865-81b0-98c71465e924', contemporary_conflict_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7b6c32ce-8937-4865-81b0-98c71465e924', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detained_persons).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, affected_civilian_populations).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_humanitarian_monitoring_bodies).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All individuals held in custody by any organized armed actor — state or non-state — receive guaranteed minimum protections: humane treatment, medical care, fair trial rights, prohibitions on torture and summary execution. Under this reading, detention status itself triggers protections regardless of the conflict's formal classification. The person's capacity to secure these rights depends entirely on external monitoring and enforcement.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detained_persons, beneficiary,
    powerless, biographical, trapped, universal).

% Receive protection from deliberate attack, deprivation of essential resources, and forced displacement in all armed contexts covered by this reading. Their protected status is not conditional on the conflict's legal classification; they are protected by virtue of being civilians in an organized armed situation, regardless of whether it is termed an international armed conflict, internal armed conflict, or security operation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, affected_civilian_populations, beneficiary,
    powerless, biographical, constrained, universal).

% Bear operational constraints and accountability exposure under this reading: they must apply CA3 minimums to all detained persons, must refrain from attacks on civilian population centers, and face potential international prosecution and investigation regardless of whether their government formally acknowledges armed conflict status. The expansive reading eliminates the state's ability to recharacterize counterinsurgency or police operations as law enforcement to escape CA3 application.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_forces, payer,
    institutional, generational, constrained, universal).

% Bear the same obligations as state forces under this reading: CA3 minimums apply to their detainees, their attacks on civilians are constrained, and they are potentially subject to international prosecution. The expansive reading treats organizational capacity and violence scale as sufficient triggers, independent of state recognition or formal conflict classification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups, payer,
    organized, biographical, constrained, universal).

% The ICRC, UN fact-finding missions, and human rights bodies gain a clear mandate to monitor and investigate compliance with CA3 minimums in all organized armed situations, without requiring the conflict to be formally classified as armed conflict. They set standards for what counts as organized violence triggering CA3, translate the reading into operational guidance, and conduct monitoring that holds both state and non-state actors accountable.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_humanitarian_monitoring_bodies, beneficiary,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, international_humanitarian_monitoring_bodies, agenda_setter).

% Face constraints on their discretion to classify situations and thereby determine which protections apply. Under this reading, they cannot reclassify an organized armed group's actions as ordinary crime to avoid CA3 constraints on their own response. They also bear responsibility to prevent violations by non-state actors and may face international liability for failure to prevent or prosecute violations, even in conflicts they do not formally acknowledge.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_governments, payer,
    institutional, generational, constrained, universal).

% Gain expansive jurisdiction and a clear mandate under this reading: CA3 violations become prosecutable in any organized armed situation, and the court does not need to first determine whether a conflict meets some higher threshold of intensity or organization to investigate alleged breaches of minimum standards. The reading expands the universe of prosecutable conduct and situations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_criminal_court_and_tribunals, agenda_setter,
    institutional, generational, analytical, universal).

% Are kept outside the operational consensus under this reading: they argue that CA3 applies only to formally recognized armed conflicts meeting intensity and organization thresholds, and that lower-level violence and law enforcement remain outside humanitarian law. The expansive reading forecloses their ability to use classification as a regulatory escape hatch and directly contradicts their core premise that state-level recognition determines application.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_centric_reading_adherents, excluded,
    institutional, generational, constrained, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__expansive_human_rights_reading, international_humanitarian_monitoring_bodies).
narrative_ontology:fixing_cost_class(common_article_3_scope__expansive_human_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal floor of minimum humanitarian standards — prohibitions on torture, execution without trial, denial of medical care — that apply to ANY organized armed violence, regardless of its political status or the state's formal classification. This solves the coordination problem of preventing humanitarian catastrophe: absent the universal floor, actors could reclassify violence as law enforcement or internal security to escape constraints, leaving vulnerable populations unprotected.
% TRANSFER_FUNCTION: Moves authority over violence standards from individual states (who decide how to classify their own situations) to an international legal floor enforced by external bodies (ICRC, UN, ICC). States and non-state actors surrender discretion to avoid protections through reclassification; the constraint transfers their unilateral classification power into an internationally monitored, universally applied standard.
% ABSENT_VOICES: State security establishments and states facing internal insurgencies who argue for narrower CA3 application are substantially excluded from the consensus under this reading — their position (CA3 applies only to formally recognized conflicts) is explicitly contradicted by the expansive reading's core premise. Military and security experts who contend that broad CA3 application hampers counterinsurgency effectiveness are kept outside the operative reading, though they testify in domestic legal challenges and policy forums.
% DISAPPEARANCE_RATIONALE: If this reading (and its enforcement) disappeared, states would reclassify internal armed conflicts as law enforcement to escape CA3 constraints; humanitarian monitoring bodies would lose mandate clarity in ongoing conflicts; detainees in non-state custody would lose protected status; non-state actors could claim they are not yet bound by humanitarian law. The international system would fragment into state-by-state determinations of when humanitarian standards apply, with vulnerable populations unprotected in gray zones.
% FOUNDING_PROBLEM: Historical use of classification as an escape hatch: states avoided humanitarian law in internal conflicts by denying armed conflict status; non-state actors were not bound by humanitarian law because they were not state parties; detainees in gray-zone operations (counterinsurgency, police response to organized violence) fell outside humanitarian protections. The core problem: humanitarian law's application depended on states' own classification choices and non-state actors' formal status, leaving vulnerable populations unprotected when classification was disputed or denied.
% FOUNDING_PROBLEM_CORROBORATION: International humanitarian organizations (ICRC, Human Rights Watch, Médecins Sans Frontières) document ongoing classification disputes in Yemen, Syria, Myanmar, Colombia, and Ukraine where states deny armed conflict status to avoid humanitarian law application and detainees in disputed zones face torture and summary execution. UN fact-finding missions and ICC prosecution teams testify that classification gaps create accountability vacuums. Academic legal scholars outside government (Dapo Akande, Amelia Branczik) argue the founding problem persists; governments claiming CA3 does not apply to their situations provide countervailing testimony but do not dispute that classification disputes occur — they dispute their consequence.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62) because the reading imposes compliance obligations that constrain violence tactics and transparency requirements on the payers (state and non-state armed actors) while generating no direct rents — the benefits accrue to protected populations and monitoring bodies as rule-following incentives and accountability mechanisms, not as material transfers. Suppression is high (0.71) because the reading must actively overcome state-centric classification regimes and security force resistance; absent continuous international pressure and enforcement, states revert to reclassification. Theater is moderate (0.28): monitoring and public accountability mechanisms are partly performative (some investigations never prosecute, some monitoring is symbolic) but the constraint's core function — preventing the worst humanitarian abuses through binding protections — is substantive. Accessibility collapse is high (0.78): once a state or armed group is organized and violence begins, the organization cannot avoid CA3 application; the reading forecloses the classification escape. Resistance is high (0.72): states and some non-state actors actively contest the expansive reading, argue for narrower application, and litigate its scope in national and international forums. The trajectory shows base extractiveness rising early (t=0 to t=15) as the reading's enforcement machinery matured through ICC prosecutions and UN fact-finding mandates, then plateauing (t=15 to t=30) as the enforcement ceiling was reached and resistance stabilized at a high level.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats experience radically different types: state security forces compute the constraint as a binding enforcement mechanism (no discretion to reclassify) while detained persons compute it as a floor of protection (baseline guarantees regardless of political context). The engine should compute the security-force seat as closer to snare (extraction with suppression and enforcement) while the detainee seat computes as rope (coordination that benefits them without extraction). This divergence arises from the structural asymmetry: the reading's enforcement depends on external monitoring and state accountability, which payers experience as coercive while beneficiaries experience as enabling. Directionality for state_security_forces sits near 1.0 (full target: they lose classification discretion, face accountability exposure, bear operational costs). Directionality for detained_persons sits near 0.0 (full beneficiary: they gain protections without bearing compliance costs). Non-state armed actors sit intermediate to high (0.6-0.8): they are newly bound by humanitarian law without having negotiated the terms, making them targets, but they also face lower surveillance and prosecution pressure than states in many contexts, creating some arbitrage.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (detained_persons, affected_civilian_populations, international_humanitarian_monitoring_bodies) gain protections, monitoring authority, and prosecution leverage from the expansive reading. Their directionality is derived as d ≈ 0.1-0.2 (beneficiary end): they collect protections without bearing compliance costs, and their exit options are analytical (for ICRC) or none (for detainees). Victims (state_security_forces, non_state_armed_groups) lose the ability to reclassify and escape CA3, face accountability exposure, and bear operational constraints. Their directionality is d ≈ 0.75-0.85 (target end): they are constrained actors facing external enforcement. State governments occupy a complex position: they pay compliance costs but also gain legitimacy from enforcing CA3 and gain leverage over non-state actors, placing them at d ≈ 0.55 (near symmetric). The reading trades state classification discretion for institutional legitimacy and unified rules. No directionality overrides are needed; the derivation chain (beneficiary/victim + power + exit) produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: classification disputes continue in active conflicts (Yemen, Syria, Myanmar) and states continue to argue for narrower CA3 application. The disappearance verdict is world_rearranges: humanitarian protections would collapse if the expansive reading were abandoned. There is no mandatrophy — the founding problem persists and the constraint's function remains necessary. The constraint is claimed as tangled_rope and should compute as such: it has genuine coordination (universal humanitarian floor that benefits detainees and civilians) AND asymmetric extraction (states and armed groups lose classification discretion and face accountability). Mandatrophy would arise only if the founding problem died (classification disputes stopped) while the enforcement machinery persisted as pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_determinism_vs_law,
    'Is a state''s own classification of a situation (as armed conflict vs. law enforcement) determinative of CA3 scope, or is scope determined by structural facts (organization, violence scale) independent of classification?',
    'Case law from international courts and tribunals; state practice in treaty reservations and application declarations; ICRC guidance interpretation by major state parties.',
    'If classification is determinative, the expansive reading fails (scope is what states agree it is). If structural facts are determinative, the expansive reading succeeds (scope is universal and states cannot escape it). This resolves the kernel contest between expansive and state-centric readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classification_determinism_vs_law, empirical, 'Whether CA3 scope is controlled by state classification or by structural facts of violence organization.').

omega_variable(
    non_state_actor_subjectivity,
    'Can non-state armed groups be direct subjects of international humanitarian law (bound by CA3 minimums) without state recognition or formal status, or do they require some threshold of organization and territorial control to qualify as IHL subjects?',
    'ICC prosecution of non-state armed group leaders; ICRC position papers and operational guidance; state ratification of protocols extending IHL to non-state actors; case law from ICTY, ICTR, and hybrid tribunals.',
    'If non-state actors can be direct subjects without formal status, the expansive reading''s core claim holds and non-state armed groups are immediate payers. If formal status or organization thresholds apply, the reading''s universality is qualified and some non-state actors remain outside CA3 scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_subjectivity, empirical, 'Whether non-state armed groups are direct CA3 subjects or require formal status.').

omega_variable(
    enforcement_versus_legitimacy,
    'Is the expansive reading''s legitimacy dependent on enforcement capacity (international courts and monitoring bodies actually holding actors accountable), or is it grounded in the norm itself regardless of enforcement?',
    'Trajectory of ICC prosecutions and investigation resources; state cooperation with international courts; ICRC field presence and monitoring capacity in major conflicts.',
    'If legitimacy depends on enforcement capacity, weakening enforcement (declining state cooperation, ICC budget cuts) would undermine the reading''s claim to being a true floor. If grounded in the norm itself, the reading persists even under weak enforcement. This affects whether theater_ratio rises as enforcement atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_versus_legitimacy, conceptual, 'Whether the expansive reading''s authority is enforcement-dependent or norm-intrinsic.').

omega_variable(
    suppression_internalization,
    'Is state resistance to the expansive reading (efforts to reclassify situations as law enforcement) structural (economic incentives to avoid constraints) or internalized (states have accepted the norm but resist its application in specific cases)?',
    'Post-compliance trajectory: if states remove application barriers and cooperate after initial resistance, suppression is partly internalized. If resistance persists regardless of monitoring, suppression is structural.',
    'If internalized, the constraint''s suppression metric is overstated for states that have formally accepted the norm (suppression should be lower, reflecting norm acceptance with tactical resistance). If structural, measured suppression is accurate (states actively avoid the constraint''s application). This affects the theater_ratio interpretation: is enforcement machinery partly performative or mostly substantive?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether state resistance to CA3 scope is structural or internalized norm-conflict.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comm_tr_t5, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(comm_tr_t10, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement(comm_tr_t15, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(comm_tr_t20, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(comm_tr_t25, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(comm_tr_t30, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comm_be_t5, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(comm_be_t10, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(comm_be_t15, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(comm_be_t20, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(comm_be_t25, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(comm_be_t30, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(comm_su_t5, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(comm_su_t10, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(comm_su_t15, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(comm_su_t20, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(comm_su_t25, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(comm_su_t30, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 30, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__expansive_human_rights_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, icc_prosecutorial_jurisdiction).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, non_state_armed_group_accountability).

% DUAL FORMULATION NOTE:
% Common Article 3 scope is a contested kernel with three structurally distinct readings: (1) expansive_human_rights_reading (this story): CA3 applies to all organized armed violence as a universal floor; (2) state_centric_reading (sibling): CA3 applies only to formally recognized armed conflicts meeting intensity thresholds; (3) icrc_customary_reading (sibling): CA3 scope emerges from state practice and opinio juris evolution. Each reading instantiates a different constraint with different ε, different beneficiary/victim sets, and different classifications. The readings coexist and influence each other — the expansive reading creates political pressure for custom to shift; the state-centric reading preserves operational discretion for states; the customary reading tracks the middle ground. All three must be authored as separate constraint stories linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
