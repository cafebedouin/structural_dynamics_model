% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Territorial Sovereignty Legitimacy - Self-Determination Reading
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the self-determination reading of the
 *   territorial_sovereignty_legitimacy kernel. The standing arrangement under
 *   contest is the Israeli territorial sovereignty regime over historic
 *   Palestine, assessed by the reading's own lights. The reading holds that
 *   sovereignty legitimacy derives from the modern principle of
 *   self-determination applied to the Arab population, which held demographic
 *   majority and continuous residence during the 19th-20th centuries. From
 *   this reading's perspective, the current arrangement is an unjust
 *   imposition maintained by colonial partition and active enforcement,
 *   extracting self-determination, land, and political status from
 *   Palestinians while coordinating Jewish Israeli collective existence. The
 *   authored metrics are reading-indexed: they describe how extractive the
 *   arrangement appears from the self-determination seat, not from a neutral
 *   Archimedean point.
 *
 * KEY AGENTS:
 *   - Israeli state (agenda_setter/institutional): Enforces territorial control and sovereignty privileges
 *   - Jewish Israeli citizens (beneficiary/organized): Collect self-determination and state-coordination benefits
 *   - Palestinian Arabs (payer/powerless): Bear dispossession and occupation costs
 *   - Palestinian refugees (payer/powerless): Bear exclusion from return and status loss
 *   - International human rights observers (observer/institutional): Monitor and document from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.82).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.88).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Territorial Sovereignty Legitimacy - Self-Determination Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '0b5959f7-5163-45e0-9172-1becd613eeaa').
narrative_ontology:cs_kernel_codification('0b5959f7-5163-45e0-9172-1becd613eeaa', fixed_text).
narrative_ontology:cs_authority_grounding('0b5959f7-5163-45e0-9172-1becd613eeaa', lineage).
narrative_ontology:cs_interpretation_layer_present('0b5959f7-5163-45e0-9172-1becd613eeaa').
narrative_ontology:cs_reading_relation('0b5959f7-5163-45e0-9172-1becd613eeaa', territorial_sovereignty_legitimacy__covenant_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('0b5959f7-5163-45e0-9172-1becd613eeaa', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('0b5959f7-5163-45e0-9172-1becd613eeaa', foundational, self_determination_as_sovereignty_source).
narrative_ontology:cs_axiom_status(self_determination_as_sovereignty_source, holdable).
narrative_ontology:cs_axiom_grounding('0b5959f7-5163-45e0-9172-1becd613eeaa', self_determination_as_sovereignty_source, conventional).
narrative_ontology:cs_axiom('0b5959f7-5163-45e0-9172-1becd613eeaa', foundational, colonial_partition_illegitimacy).
narrative_ontology:cs_axiom_status(colonial_partition_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0b5959f7-5163-45e0-9172-1becd613eeaa', colonial_partition_illegitimacy, deontological).
narrative_ontology:cs_reference_frame('0b5959f7-5163-45e0-9172-1becd613eeaa', modern_self_determination_framework).
narrative_ontology:cs_drift_state('0b5959f7-5163-45e0-9172-1becd613eeaa', post_1948_statehood_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('0b5959f7-5163-45e0-9172-1becd613eeaa', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, jewish_israeli_citizens).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arabs).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, anti_colonial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, un_charter_self_determination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises territorial sovereignty and military control over the contested territory, enforcing borders, settlement expansion, and a legal framework that privileges Jewish national self-determination. Sets the security and demographic policies that maintain the current arrangement.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Collect the benefits of statehood including security, democratic self-governance, economic development, and national-cultural expression within a territory where they are the enfranchised population. Their collective existence is coordinated by the state framework, though its persistence depends on the exclusion of the Arab demographic majority from equivalent sovereignty.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, jewish_israeli_citizens, beneficiary,
    organized, generational, mobile, national).

% Constituted the demographic majority with continuous residence in the territory during the modern period, now living under military occupation, blockade, or as second-class citizens within the sovereign territory. Bear the costs of denied self-determination, land confiscation, resource deprivation, and political marginalization. Exit is blocked by physical barriers, permit regimes, and identity-locked claims to the land.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_arabs, payer,
    powerless, generational, trapped, national).

% Descendants of those displaced from continuous residence during the 1948 and 1967 wars, denied the right of return by the current territorial arrangement. Maintained in host countries or camps without citizenship integration, their identity is fused to the lost territory. The constraint extracts their homeland and political status; exit is blocked by both host-state refusal of naturalization and the identity-locked character of the refugee claim.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees, payer,
    powerless, generational, trapped, regional).

% Jewish voices within and outside the territory who reject the Zionist state framework and support Palestinian self-determination. Structurally marginalized in Israeli political discourse and in international diplomatic frameworks that assume Jewish-Israeli consensus on state legitimacy.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, anti_zionist_jewish_dissidents, excluded,
    moderate, biographical, constrained, national).

% United Nations bodies, international courts, and human rights organizations that document violations of self-determination and refugee rights, issue advisory opinions, and monitor compliance with international law. They neither collect from nor pay into the constraint, but their reports create discursive pressure on the agenda-setter.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_human_rights_observers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__self_determination_reading, israeli_state).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__self_determination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish collective security, governance, and national-cultural expression within a defined territorial state framework, providing a single locus of military defense, legal jurisdiction, and democratic participation for Jewish Israelis.
% TRANSFER_FUNCTION: Transfers territorial sovereignty, land control, water and resource rights, and demographic dominance from the Arab population and Palestinian refugees to the Israeli state and its Jewish citizenry, while externalizing the Palestinian population into statelessness, occupation, exile, or subordinate civic status.
% ABSENT_VOICES: Palestinian refugees are excluded from final-status negotiations; anti-Zionist Jewish dissidents who reject the state framework are marginalized in diplomatic discourse; Palestinian citizens of Israel whose equal sovereignty claims challenge the Jewish character of the state are sidelined in constitutional deliberations.
% DISAPPEARANCE_RATIONALE: The constraint's disappearance would trigger the restoration of Arab-majority self-determination and refugee repatriation, dissolving the current territorial and demographic order and reorganizing sovereignty around the population with continuous modern residence.
% FOUNDING_PROBLEM: The problem of Jewish statelessness, persecution in Europe, and the search for a secure national home in the late 19th and early 20th centuries.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state institutions attest the founding problem remains live due to ongoing security threats and antisemitism. Palestinian historians, anti-colonial scholars, and critical international legal experts attest the founding problem has been superseded by Israeli state power and cannot justify ongoing dispossession; they corroborate from outside the beneficiary set that the arrangement now functions as colonial extraction rather than protective coordination.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.82) is very high because the constraint systematically transfers sovereignty, territory, and resources from the Arab demographic majority to a Jewish minority state, assessed by the reading's criterion of self-determination. Suppression (0.88) is higher still: the arrangement persists only through active military occupation, legal discrimination, blockade, diplomatic shielding, and the suppression of Palestinian political organization and return. Theater ratio (0.45) reflects significant performative maintenanceâpeace processes, democratic institutions within pre-1967 lines, and two-state rhetoricâthat obscures the ongoing extraction. Accessibility collapse (0.75) is high because alternatives (one democratic state, full refugee return, or sovereign Palestinian control) are rendered politically inaccessible by the same enforcement apparatus. Resistance (0.72) reflects sustained Palestinian popular resistance, international solidarity, and legal challenges. The measurement series run on one shared time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (Israeli state, Jewish citizens) experience the constraint as legitimate national coordination providing security and collective self-expression. The payer seats (Palestinian Arabs, refugees) experience the same structure as colonial extraction denying their demographic majority claim to self-determination. The engine computes this divergence from the structural data: low directionality for beneficiaries with mobile and arbitrage-grade exit options, high directionality for trapped victims at national and regional scope, yielding massively asymmetric effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (israeli_state, jewish_israeli_citizens) are declared in base_properties.beneficiaries; victims (palestinian_arabs, palestinian_refugees) are declared in victims. The Israeli state sits at institutional power with arbitrage-grade diplomatic exit, driving directionality toward the beneficiary end. Jewish citizens are organized beneficiaries with mobile exit options. Palestinian Arabs and refugees are powerless, trapped, and identity-lockedâhigh directionality toward the target end. The spatial scope is national to regional, amplifying effective extraction for the trapped populations. No overrides are needed: the structural derivation chain produces the correct asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâJewish statelessness and persecutionâmay have been genuine, but the self-determination reading holds that its resolution through partition and dispossession of the Arab majority transformed the arrangement from protective coordination into asymmetric extraction. The R5 genealogy interview records founding_problem_status as contested, signaling that the mandate's justification is disputed and that the arrangement persists beyond its legitimate grounding. This prevents mislabeling the constraint as scaffold (transitional) or rope (pure coordination): even if the founding problem was real, the reading assesses the current arrangement as having atrophied into a coercive extraction structure sustained by enforcement rather than consent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the self-determination reading of sovereignty legitimacy structurally separable from the covenant continuity and existential matrix readings, or do they share underlying empirical claims that could be jointly falsified?',
    'Comparative analysis of the three readings'' foundational axioms and empirical premises; identification of shared versus divergent commitment bases through historical and legal review.',
    'If readings share falsifiable empirical premises, axiom-overriding drift in one may cascade to others; if fully separable, they persist as parallel normative frameworks with independent vulnerability profiles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between sibling readings in the territorial sovereignty kernel').

omega_variable(
    colonial_coordination_separability,
    'Does the Israeli state structure provide genuine coordination for Jewish Israelis that is separable from the extraction from Palestinians, or are the two functions structurally inseparable?',
    'Analysis of whether institutional reforms (equal citizenship, refugee return frameworks, power-sharing) could preserve Jewish collective coordination without the extractive apparatus of occupation and exclusion.',
    'If separable, the constraint remains Tangled Rope with a reformable coordination component; if inseparable, the constraint is more accurately classified as Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_coordination_separability, conceptual, 'Whether coordination and extraction components are structurally separable').

omega_variable(
    demographic_majority_continuity,
    'What was the precise demographic composition and continuity of Arab residence in the territory during the 19th-20th centuries, and does it support the self-determination reading''s empirical foundation?',
    'Historical demographic research and archival analysis of Ottoman and British census data, land registries, and village records.',
    'If Arab demographic majority and continuity are substantiated, the reading''s empirical foundation is strengthened; if significantly challenged, the reading may require revised normative grounding independent of demographic claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_majority_continuity, empirical, 'Empirical basis for demographic majority and continuity claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(terr_tr_t20, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(terr_tr_t40, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(terr_tr_t60, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(terr_tr_t80, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 80, 0.55).
narrative_ontology:measurement(terr_tr_t100, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(terr_be_t20, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(terr_be_t40, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(terr_be_t60, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(terr_be_t80, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement(terr_be_t100, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 100, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(terr_su_t20, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(terr_su_t40, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(terr_su_t60, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(terr_su_t80, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 80, 0.82).
narrative_ontology:measurement(terr_su_t100, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 100, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the territorial_sovereignty_legitimacy kernel. Each reading represents a structurally distinct claim about the basis of sovereignty legitimacy in Israel/Palestine, with different epsilon values, beneficiary and victim sets, and normative foundations. The self-determination reading is linked to its siblings as part of the constraint family decomposition mandated by the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
