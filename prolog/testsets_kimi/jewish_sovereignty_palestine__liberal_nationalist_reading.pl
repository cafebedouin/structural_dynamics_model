% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Sovereignty in Palestine â Liberal Nationalist Reading
 *   domain: political/nationalism/postcolonial
 *
 * SUMMARY:
 *   This constraint instantiates the liberal-nationalist reading of the
 *   contested kernel jewish_sovereignty_palestine. It holds that the Jewish
 *   people possess a collective right of national self-determination
 *   legitimately exercised through sovereign statehood in their ancestral
 *   homeland, while recognizing Palestinians as co-equal self-determination
 *   claimants requiring territorial partition or a binational framework. The
 *   constraint is structurally distinct from its siblings: the cultural
 *   Zionist reading (spiritual center without sovereignty), the religious
 *   Zionist reading (divine inalienable promise), the post-Zionist reading
 *   (state achieved but ethnic framework obstructs civic equality), and the
 *   settler-colonial reading (European displacement regime). In this reading,
 *   Palestinians enter as co-equal claimants rather than mere obstacles,
 *   which moderates extractiveness but does not eliminate asymmetric
 *   territorial cost.
 *
 * KEY AGENTS:
 *   - jewish_collective: Primary beneficiary (organized/global/identity_locked) â receives sovereignty and statehood legitimation
 *   - palestinian_collective: Primary target (moderate/national/constrained) â bears territorial partition and sovereignty loss
 *   - israeli_state: Agenda setter (institutional/national/constrained) â administers enforcement of the sovereign framework
 *   - one_state_advocates: Excluded voice (moderate/national/constrained) â rejected by partition framework
 *   - international_community: Analytical observer (institutional/global/analytical) â mediates claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.52).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.62).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Sovereignty in Palestine â Liberal Nationalist Reading").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political/nationalism/postcolonial").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '59b50286-d38a-422f-92d1-c53a86ac7fbd').
narrative_ontology:cs_kernel_codification('59b50286-d38a-422f-92d1-c53a86ac7fbd', formalized).
narrative_ontology:cs_authority_grounding('59b50286-d38a-422f-92d1-c53a86ac7fbd', lineage).
narrative_ontology:cs_interpretation_layer_present('59b50286-d38a-422f-92d1-c53a86ac7fbd').
narrative_ontology:cs_reading_relation('59b50286-d38a-422f-92d1-c53a86ac7fbd', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('59b50286-d38a-422f-92d1-c53a86ac7fbd', jewish_sovereignty_palestine__religious_zionist_reading, forecloses).
narrative_ontology:cs_reading_relation('59b50286-d38a-422f-92d1-c53a86ac7fbd', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_reading_relation('59b50286-d38a-422f-92d1-c53a86ac7fbd', jewish_sovereignty_palestine__settler_colonial_reading, influences).
narrative_ontology:cs_axiom('59b50286-d38a-422f-92d1-c53a86ac7fbd', foundational, national_self_determination_legitimizes_statehood).
narrative_ontology:cs_axiom_status(national_self_determination_legitimizes_statehood, holdable).
narrative_ontology:cs_axiom_grounding('59b50286-d38a-422f-92d1-c53a86ac7fbd', national_self_determination_legitimizes_statehood, deontological).
narrative_ontology:cs_axiom('59b50286-d38a-422f-92d1-c53a86ac7fbd', foundational, territorial_compromise_with_co_equal_claimants).
narrative_ontology:cs_axiom_status(territorial_compromise_with_co_equal_claimants, holdable).
narrative_ontology:cs_axiom_grounding('59b50286-d38a-422f-92d1-c53a86ac7fbd', territorial_compromise_with_co_equal_claimants, deontological).
narrative_ontology:cs_reference_frame('59b50286-d38a-422f-92d1-c53a86ac7fbd', liberal_nationalist_sovereignty).
narrative_ontology:cs_drift_state('59b50286-d38a-422f-92d1-c53a86ac7fbd', contemporary_two_state_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('59b50286-d38a-422f-92d1-c53a86ac7fbd', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, national_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises collective self-determination through sovereign statehood in the ancestral homeland; the constraint vindicates Jewish national identity by providing a territorial state framework and international legitimation. Exit would mean abandoning the nation-state form as the primary vehicle of Jewish collective existence.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective, beneficiary,
    organized, civilizational, identity_locked, global).

% Bears the territorial and political cost of partition; recognized as co-equal self-determination claimant in liberal nationalist theory but structurally disadvantaged in access to land, refugee return, and full sovereign parity. Exit options are constrained by occupation, refugee status, and fragmented governance.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective, payer,
    moderate, generational, constrained, national).

% Administers the sovereign framework, enforces borders and demographic policies, and manages the security apparatus that sustains Jewish-majority statehood. Constrained by its own foundational commitment to the liberal nationalist self-determination claim.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Advocate for a single democratic or binational state in all of historic Palestine; structurally excluded from the liberal nationalist consensus that presupposes Jewish sovereign statehood as the anchor of any settlement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, one_state_advocates, excluded,
    moderate, biographical, constrained, national).

% Mediates between claims through international law and diplomacy; formally endorses the two-state framework and the self-determination of both peoples but does not bear direct costs of the territorial arrangement.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of Jewish collective self-determination through sovereign statehood in the ancestral homeland while formally recognizing Palestinians as co-equal self-determination claimants requiring partition or binational arrangement.
% TRANSFER_FUNCTION: Moves territorial sovereignty and demographic-majority control from the pre-existing Palestinian Arab population to a Jewish-national state framework, with residual sovereignty allocated to a separate Palestinian entity or shared institutions.
% ABSENT_VOICES: Palestinian refugees who would claim return rights incompatible with Jewish demographic majority; one-state advocates who reject partition; religious Zionists who reject territorial compromise; anti-nationalist cosmopolitans who reject ethnic sovereignty frameworks altogether.
% DISAPPEARANCE_RATIONALE: If the Jewish self-determination claim to sovereign statehood in Palestine vanished, the State of Israel would lose its foundational legitimation, Palestinian political claims would reconfigure around unitary or fully sovereign frameworks, and the international two-state consensus would collapse â the regional political order would rearrange.
% FOUNDING_PROBLEM: Jewish statelessness and minority status in Europe and the Middle East during the rise of ethnic nationalism, leading to persecution, exclusion, and the 'Jewish Question' of where Jewish collective existence could be secured.
% FOUNDING_PROBLEM_CORROBORATION: UNSCOP (1947) and British Peel Commission records attest the Jewish national homelessness problem from a non-beneficiary investigative seat. Palestinian historiography and postcolonial scholars attest the problem was real but its solution via Jewish statehood in Palestine created a complementary Palestinian dispossession. No neutral consensus exists outside the contending parties.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate because the reading accepts territorial compromise and recognizes Palestinian co-equal status, preventing the maximal extraction of a pure ethno-state. However, persistent settlement expansion, occupation, and denial of refugee return maintain asymmetric extraction. Suppression (0.62) reflects the active enforcement required to maintain Jewish-majority sovereignty against Palestinian demographic and territorial claims. Theater (0.42) captures the growing gap between two-state rhetoric and the one-state reality of Israeli territorial control. Accessibility collapse (0.65) is moderate-high: once the national self-determination frame is accepted, alternatives (binationalism, full Palestinian return, secular civic state) are marginalized in mainstream international discourse but persist in subaltern and academic contexts. Resistance (0.60) reflects sustained Palestinian opposition, BDS, and diplomatic contestation.
 *
 * PERSPECTIVAL GAP:
 *   The Jewish collective seat experiences the constraint as vindication of historical rights and necessary protection against persecution. The Palestinian collective seat experiences the same structure as territorial dispossession and truncated sovereignty. The Israeli state seat experiences it as a security and administrative imperative. These divergences are structurally encoded by directionality: the beneficiary (Jewish collective) sits near d=0.0, the target (Palestinian collective) near d=1.0, and the agenda setter (Israeli state) occupies an intermediate enforcement position.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish collective is the declared beneficiary of the self-determination right â the constraint subsidizes their national existence through sovereign statehood. The Palestinian collective is the declared victim â they bear the cost of partition, occupation, and refugee exclusion. The Israeli state enforces the constraint and is partly captured by it (directionality mid-range). International institutions observe without bearing direct cost. One-state advocates are structurally excluded, receiving high directionalities as their preferred alternatives are suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the co-equal claimant recognition and territorial compromise axioms, this would compute as a snare (pure extraction under cover of rights discourse). Without the Palestinian victim structure, it would appear as a rope (pure coordination of Jewish self-determination). The tangled rope classification captures the genuine coordination function â statehood for a historically persecuted minority â alongside the asymmetric extraction â Palestinian territorial and political costs. The moderate theater ratio prevents piton misclassification: the constraint is not merely performative; the Israeli state exercises real sovereignty, though the two-state rhetoric increasingly outruns the territorial reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the Jewish claim to self-determination in Palestine better understood as a liberal-nationalist exercise of a universal right, or as a settler-colonial project masked by rights discourse?',
    'Comparative historical-sociological analysis of Zionist migration patterns alongside other settler-colonial formations; assessment of whether the self-determination claim structurally requires Palestinian dispossession.',
    'If the settler-colonial framing is accurate, the extractiveness is higher than modeled and the coordination function is cover; if the liberal-nationalist framing holds, the moderate extractiveness represents a tragic collision of two legitimate claims rather than structural asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the constraint is rights-based coordination or settler-colonial extraction.').

omega_variable(
    partition_viability,
    'Given current territorial and demographic realities, is a partition-based liberal nationalist solution still structurally possible, or has practice drift rendered the two-state reference frame obsolete?',
    'Spatial-demographic analysis of settlement patterns, security barrier routing, and Palestinian population distribution relative to 1967 lines.',
    'If partition is no longer viable, the liberal nationalist reading loses its practical coordination function and becomes either a piton (theatrical maintenance of an impossible framework) or a snare (ongoing extraction under cover of a defunct compromise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_viability, empirical, 'Whether two-state partition remains structurally viable.').

omega_variable(
    co_equal_claimant_status,
    'Does the liberal nationalist reading structurally treat Palestinians as co-equal claimants with whom to share sovereignty, or as obstacles to be managed within a Jewish-majority framework?',
    'Analysis of citizenship laws, resource allocation within mixed jurisdictions, and the legal architecture of land ownership.',
    'If co-equal, the constraint moves toward rope or scaffold; if managed-minority, it moves toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_equal_claimant_status, empirical, 'Whether Palestinian co-equal status is structural or rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jewi_tr_t19, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 19, 0.2).
narrative_ontology:measurement(jewi_tr_t38, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 38, 0.25).
narrative_ontology:measurement(jewi_tr_t57, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 57, 0.35).
narrative_ontology:measurement(jewi_tr_t76, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 76, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(jewi_be_t19, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 19, 0.42).
narrative_ontology:measurement(jewi_be_t38, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 38, 0.45).
narrative_ontology:measurement(jewi_be_t57, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 57, 0.48).
narrative_ontology:measurement(jewi_be_t76, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 76, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jewi_su_t19, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 19, 0.5).
narrative_ontology:measurement(jewi_su_t38, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 38, 0.55).
narrative_ontology:measurement(jewi_su_t57, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 57, 0.58).
narrative_ontology:measurement(jewi_su_t76, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 76, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_colonial_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_sovereignty_palestine kernel. It is decomposed from the colloquial label 'Zionism' because that label conflates cultural, religious, liberal-nationalist, post-Zionist, and settler-colonial framings with structurally distinct epsilon values and stakeholder asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
