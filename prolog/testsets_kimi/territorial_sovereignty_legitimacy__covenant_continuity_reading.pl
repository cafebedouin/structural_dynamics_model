% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__covenant_continuity_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Territorial Sovereignty Legitimacy â Covenant Continuity Reading
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story instantiates the covenant_continuity_reading of the
 *   contested kernel territorial_sovereignty_legitimacy. The reading asserts
 *   that Israeli/Jewish sovereignty over the territory derives from an
 *   ancient divine covenant, continuous Jewish presence, and modern
 *   international recognition (Balfour, UN Partition, 1948). It treats
 *   partition as a compromise of a pre-existing right rather than the
 *   creation of a new one, and frames settlement as return rather than
 *   colonization. Sibling readings include self_determination_reading
 *   (demographic majority as legitimacy basis) and existential_matrix_reading
 *   (existential need as zero-sum driver). The structural delta is temporal
 *   scope to the biblical period, legitimacy surviving demographic absence,
 *   and the partition-as-compromise framing.
 *
 * KEY AGENTS:
 *   - israeli_government (agenda_setter/beneficiary, institutional/constrained exit) â administers and enforces the sovereignty claim
 *   - jewish_israeli_citizens (beneficiary, organized/identity_locked) â receive identity and security coordination
 *   - religious_zionist_settlers (beneficiary, organized/identity_locked) â enforce and settle under the covenant mandate
 *   - palestinian_communities (payer, powerless/trapped) â bear occupation and displacement costs
 *   - palestinian_refugee_descendants (payer, powerless/trapped) â excluded from return across generations
 *   - un_and_international_bodies (observer, institutional/analytical) â recognize but increasingly contest the claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.75).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Territorial Sovereignty Legitimacy â Covenant Continuity Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d').
narrative_ontology:cs_kernel_codification('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', fixed_text).
narrative_ontology:cs_authority_grounding('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', lineage).
narrative_ontology:cs_interpretation_layer_present('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d').
narrative_ontology:cs_reading_relation('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_reading_relation('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', foundational, divine_covenant_territorial_mandate).
narrative_ontology:cs_axiom_status(divine_covenant_territorial_mandate, holdable).
narrative_ontology:cs_axiom_grounding('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', divine_covenant_territorial_mandate, theological).
narrative_ontology:cs_axiom('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', foundational, continuous_presence_preserves_title).
narrative_ontology:cs_axiom_status(continuous_presence_preserves_title, holdable).
narrative_ontology:cs_axiom_grounding('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', continuous_presence_preserves_title, empirically_contingent).
narrative_ontology:cs_reference_frame('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', covenantal_territorial_mandate).
narrative_ontology:cs_drift_state('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', contemporary_post_1967, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('89c6f1f0-92bf-4d88-b4c7-e7f30d91cd9d', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_government).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_israeli_citizens).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlers).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_communities).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_descendants).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, biblical_territorial_promise).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__covenant_continuity_reading, un_partition_plan_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territorial sovereignty claim through legal, military, and diplomatic machinery; justifies settlement and state policy via the covenant-continuity narrative; collects tax revenue and security control from the territory; exit from the narrative would require abandoning the state's primary legitimating framework.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_government, beneficiary).

% Receive citizenship, security, and collective identity affirmation from the state whose legitimacy is framed as covenantal realization; political imagination and institutional loyalty are bound to the continuity narrative; exit means severing national identity from territorial theology.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_israeli_citizens, beneficiary,
    organized, biographical, identity_locked, national).

% Actively settle territory under the covenant-continuity mandate; receive state subsidies and ideological validation; identity is fused with the land-as-divine-promise framework; they are both beneficiaries of the constraint and enforcement agents on the ground.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, religious_zionist_settlers, beneficiary,
    organized, generational, identity_locked, national).

% Bear the costs of the sovereignty claim through military occupation, settlement expansion, movement restrictions, and denial of self-determination; their residence is rendered politically subordinate to the covenant-continuity narrative; exit is blocked by military and legal barriers.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_communities, payer,
    powerless, biographical, trapped, national).

% Excluded from return and citizenship by the demographic logic of the covenant-continuity claim; inherit displacement across generations; their ancestral territory is administered under the narrative that prioritizes Jewish return over Palestinian repatriation.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_refugee_descendants, payer,
    powerless, generational, trapped, regional).

% Recognized the state in 1948 via partition but increasingly contests settlement expansion and occupation as violations of international law; observes the tension between the covenant-continuity claim and the self-determination reading; its resolutions are resisted by the agenda setter.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, un_and_international_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_government).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__covenant_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national collective existence across time and space by linking diaspora identity to territorial statehood through a shared narrative that bridges biblical covenant, historical presence, and modern international recognition.
% TRANSFER_FUNCTION: Transfers territorial control, political authority, and demographic priority from Palestinian Arab residents and refugee descendants to the Jewish national collective and its state institutions.
% ABSENT_VOICES: Palestinian refugees and their descendants are structurally excluded from the covenant-continuity conversation; their counter-narrative of continuous modern residence and indigenous status is treated as external to the legitimacy framework. Secular Zionist voices that might prioritize civic over covenantal legitimacy are also marginalized within the current coalition.
% DISAPPEARANCE_RATIONALE: If the covenant-continuity legitimacy claim vanished, the territorial boundaries and settlement enterprise would lose their primary theological-historical justification. Israeli domestic politics would fragment along secular/religious lines, and the international legal strategy would shift to security-based or civic-national arguments. Palestinian claims would gain symmetrical juridical ground, and the ideological basis for preferential settlement would collapse.
% FOUNDING_PROBLEM: The problem of Jewish statelessness, dispersion, and systemic vulnerability in diaspora, particularly accelerated by European pogroms and the Holocaust, which generated the political demand for a territorially sovereign Jewish polity secured against majority hostility.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state institutions attest to the founding problem as urgent and unresolved before 1948. Palestinian historians and post-Zionist scholars contest that the covenant-continuity reading was the necessary or legitimate solution, arguing the problem could have been addressed without a sovereignty claim overriding indigenous presence. International legal scholars corroborate the need for a refuge but not the specific covenant-continuity basis; the UN partition plan is cited by outside parties as a pragmatic compromise rather than a validation of pre-existing covenantal title.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__covenant_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint transfers territorial authority and demographic priority from one national collective to another under a non-negotiable theological-historical title. Suppression (0.75) is high because the claim's persistence requires active military, legal, and settlement enforcement against Palestinian self-determination and return. Theater ratio (0.45) reflects significant genuine belief (especially in religious Zionist communities) alongside performative state rituals (archaeological policy, biblical citation in legal documents) that maintain the narrative. Accessibility collapse (0.60) is moderate-high within the Israeli polity but lower globally. Resistance (0.80) is high due to sustained Palestinian opposition and international legal challenge. The claim is tangled_rope because the SAME narrative structure genuinely coordinates Jewish national identity (beneficiaries) while asymmetrically extracting from Palestinians (victims).
 *
 * PERSPECTIVAL GAP:
 *   The Israeli government and religious Zionist settlers experience this constraint as genuine coordination of national survival and divine mandate; the engine computes their seat as tangled_rope or rope-leaning. Palestinian communities and refugees experience it as enforced extraction with near-zero exit; the engine computes their seat as snare-leaning. The divergence is structural, not perspectival illusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (israeli_government, jewish_israeli_citizens, religious_zionist_settlers) have low directionality toward extraction because the constraint subsidizes their collective identity and territorial control. Victims (palestinian_communities, palestinian_refugee_descendants) have high directionality because the constraint extracts their territorial claims and political autonomy. The religious_zionist_settler seat is identity_locked, pushing it toward full beneficiary fusion. Palestinian seats are trapped, pushing them toward full target. The UN observer seat is analytical, with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â Jewish statelessness and vulnerability â was substantially addressed by 1948 statehood. However, the constraint persists and expands beyond the 1948 armistice lines because the covenant-continuity reading treats the territory as an indivisible divine mandate. Mandatrophy is partially resolved in the sense that the state exists, but unresolved in that the narrative now legitimates territorial expansion that exceeds the founding problem's scope. This prevents mislabeling the coordination as pure extraction (the statelessness problem was real) while capturing that the current operation extracts beyond what the founding problem justified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    covenant_reading_kernel_position,
    'This constraint is the covenant_continuity_reading of kernel territorial_sovereignty_legitimacy; siblings are self_determination_reading and existential_matrix_reading. Does the covenant reading foreclose the self-determination reading or merely coexist with it?',
    'Analysis of whether a single legal framework can hold both an ancient covenantal title and a modern demographic self-determination claim for the same territory; review of constitutional and juridical attempts to reconcile them.',
    'If foreclosed, the kernel is structurally irreconcilable; if coexisting, hybrid legitimacy frameworks are possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covenant_reading_kernel_position, conceptual, 'Position of this reading within the contested kernel.').

omega_variable(
    historical_presence_empirical_basis,
    'Does the claim of continuous Jewish presence constituting an unbroken title hold against demographic and archaeological evidence, or is it a selectively constructed narrative?',
    'Interdisciplinary historical and demographic review establishing the scale, continuity, and political character of Jewish presence versus other communities across the last two millennia.',
    'If the empirical basis is weak, the constraint shifts toward pure ideological coordination; if strong, the extraction is partially naturalized within the reading''s own logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_presence_empirical_basis, empirical, 'Empirical foundation of continuous presence claim.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of Palestinian alternatives structural (military occupation, legal discrimination) or internalized (acceptance of Jewish historical priority as common sense)?',
    'Post-exit or post-liberation suppression trajectory: if Palestinian resistance and juridical claims persist after structural barriers ease, suppression was partially structural; if claims immediately resurge, suppression was structural rather than internalized.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates more deeply as identity coordination; if purely structural, it is closer to conventional enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_ccr_tr_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tsl_ccr_tr_t19, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 19, 0.3).
narrative_ontology:measurement(tsl_ccr_tr_t30, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(tsl_ccr_tr_t47, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 47, 0.42).
narrative_ontology:measurement(tsl_ccr_tr_t56, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 56, 0.48).
narrative_ontology:measurement(tsl_ccr_tr_t76, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 76, 0.55).

% Extraction over time
narrative_ontology:measurement(tsl_ccr_be_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(tsl_ccr_be_t19, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 19, 0.55).
narrative_ontology:measurement(tsl_ccr_be_t30, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(tsl_ccr_be_t47, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 47, 0.58).
narrative_ontology:measurement(tsl_ccr_be_t56, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 56, 0.65).
narrative_ontology:measurement(tsl_ccr_be_t76, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 76, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(tsl_ccr_su_t0, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(tsl_ccr_su_t19, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 19, 0.65).
narrative_ontology:measurement(tsl_ccr_su_t30, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(tsl_ccr_su_t47, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 47, 0.6).
narrative_ontology:measurement(tsl_ccr_su_t56, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 56, 0.72).
narrative_ontology:measurement(tsl_ccr_su_t76, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 76, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, existential_matrix_reading).

% DUAL FORMULATION NOTE:
% The kernel territorial_sovereignty_legitimacy decomposes into at least three structurally distinct constraints: covenant_continuity_reading (juridical-historical title), self_determination_reading (modern democratic-demographic title), and existential_matrix_reading (existential-territorial need). Each has a different epsilon, stakeholder set, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
