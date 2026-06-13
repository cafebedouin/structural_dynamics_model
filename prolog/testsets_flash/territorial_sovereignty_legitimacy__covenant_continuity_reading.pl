% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__covenant_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_sovereignty_legitimacy__covenant_continuity_reading
 *   human_readable: Sovereignty Legitimacy: Covenant and Continuity Reading
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint models the 'covenant and continuity' reading of
 *   territorial sovereignty legitimacy in the Israeli-Palestinian conflict.
 *   It asserts that legitimacy derives from a combination of ancient divine
 *   promise, continuous Jewish presence in the land, and modern international
 *   recognition (Balfour Declaration, UN Partition Plan, 1948 establishment).
 *   This reading frames the temporal scope of legitimacy as extending to the
 *   biblical period, views the legitimacy claim as surviving periods of
 *   demographic absence, and interprets international acts as compromises of
 *   pre-existing rights rather than the creation of new ones. Settlements are
 *   framed as a return rather than colonization.
 *
 * KEY AGENTS:
 *   - israeli_state: Primary agenda_setter (institutional/constrained) — enforces and benefits from the narrative.
 *   - palestinian_population: Primary payer (powerless/trapped) — bears the costs of the narrative's enforcement.
 *   - jewish_diaspora: Primary beneficiary (organized/mobile) — supports and benefits from the narrative.
 *   - arab_states: Secondary payer (institutional/constrained) — bears regional costs and opposes the narrative.
 *   - international_community: Observer (institutional/analytical) — mediates and sometimes challenges the narrative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.65).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.75).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__covenant_continuity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__covenant_continuity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__covenant_continuity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__covenant_continuity_reading, "Sovereignty Legitimacy: Covenant and Continuity Reading").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__covenant_continuity_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__covenant_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__covenant_continuity_reading, '7b63dcd1-e27b-4c81-bb66-1872101fbc49').
narrative_ontology:cs_kernel_codification('7b63dcd1-e27b-4c81-bb66-1872101fbc49', formalized).
narrative_ontology:cs_authority_grounding('7b63dcd1-e27b-4c81-bb66-1872101fbc49', lineage).
narrative_ontology:cs_interpretation_layer_present('7b63dcd1-e27b-4c81-bb66-1872101fbc49').
narrative_ontology:cs_reading_relation('7b63dcd1-e27b-4c81-bb66-1872101fbc49', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b63dcd1-e27b-4c81-bb66-1872101fbc49', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('7b63dcd1-e27b-4c81-bb66-1872101fbc49', foundational, divine_covenant_grants_land).
narrative_ontology:cs_axiom_status(divine_covenant_grants_land, holdable).
narrative_ontology:cs_axiom_grounding('7b63dcd1-e27b-4c81-bb66-1872101fbc49', divine_covenant_grants_land, theological).
narrative_ontology:cs_axiom('7b63dcd1-e27b-4c81-bb66-1872101fbc49', foundational, continuous_jewish_presence_maintains_claim).
narrative_ontology:cs_axiom_status(continuous_jewish_presence_maintains_claim, holdable).
narrative_ontology:cs_axiom_grounding('7b63dcd1-e27b-4c81-bb66-1872101fbc49', continuous_jewish_presence_maintains_claim, conventional).
narrative_ontology:cs_reference_frame('7b63dcd1-e27b-4c81-bb66-1872101fbc49', ancient_covenant_and_historical_presence).
narrative_ontology:cs_drift_state('7b63dcd1-e27b-4c81-bb66-1872101fbc49', contemporary_international_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7b63dcd1-e27b-4c81-bb66-1872101fbc49', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and enforces sovereignty based on historical, religious, and international claims. Benefits from the continuity narrative to legitimize its existence and territorial claims, including settlements. Bears the cost of ongoing conflict and international diplomatic pressure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, israeli_state, agenda_setter,
    institutional, generational, constrained, national).

% Experiences the constraint as a denial of self-determination and territorial rights. Bears the costs of displacement, occupation, and limited sovereignty. Their claims are often framed as secondary to the covenant and continuity narrative.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, palestinian_population, payer,
    powerless, generational, trapped, local).

% Benefits from the existence of a Jewish state, often viewing it as a fulfillment of historical and religious promises. Provides political and financial support, reinforcing the covenant and continuity narrative. Does not directly bear the costs of territorial enforcement.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, jewish_diaspora, beneficiary,
    organized, generational, mobile, global).

% Bear diplomatic, economic, and security costs due to the ongoing conflict and the perceived injustice of the covenant-continuity narrative. Their support for Palestinian self-determination is often in direct opposition to this reading of legitimacy.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, arab_states, payer,
    institutional, generational, constrained, regional).

% Observes and attempts to mediate the conflict, often balancing historical claims with modern principles of self-determination. Its recognition (e.g., UN resolutions) is a component of the covenant-continuity claim, but its broader stance is often contested.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__covenant_continuity_reading, international_community, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a foundational narrative for the establishment and continued existence of the Israeli state, coordinating the identity and historical claims of the Jewish people with modern statehood and international recognition.
% TRANSFER_FUNCTION: Transfers legitimacy and territorial rights from a historical/divine claim to the modern Israeli state, at the expense of the Palestinian population's claims to self-determination and continuous residence.
% ABSENT_VOICES: The voices of indigenous Palestinian populations prior to the 20th century are largely absent from the 'continuous presence' aspect of this narrative, which often focuses on Jewish historical ties. Their perspective would challenge the framing of 'return' over 'colonization'.
% DISAPPEARANCE_RATIONALE: If this specific legitimacy claim vanished, the foundational narrative for the Israeli state would be severely undermined, leading to a profound re-evaluation of its territorial claims, particularly regarding settlements, and a significant shift in international diplomatic positions. The entire political and legal landscape of the region would rearrange.
% FOUNDING_PROBLEM: The historical problem of Jewish statelessness and persecution, combined with the desire for national self-determination and a return to an ancestral homeland.
% FOUNDING_PROBLEM_CORROBORATION: The Israeli state and much of the Jewish diaspora attest that the problem of Jewish security and self-determination remains live. External corroboration comes from historical accounts of antisemitism and the Holocaust, which underscore the need for a secure homeland. However, the specific territorial claims derived from this problem are contested by the Palestinian population and many international bodies.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__covenant_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__covenant_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__covenant_continuity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__covenant_continuity_reading, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates a national identity and historical claim (beneficiaries: Israeli state, Jewish diaspora) but does so through asymmetric extraction and active enforcement (victims: Palestinian population, Arab states). Extractiveness is high (0.65) due to the displacement and dispossession inherent in the territorial claims. Suppression is also high (0.75) as the narrative requires active military and political enforcement to maintain its territorial outcomes against resistance. Theater ratio is moderate (0.4) as the narrative's historical and religious claims are genuinely held, but their application to modern territorial disputes involves significant performative justification for ongoing enforcement actions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Israeli state and Jewish diaspora, this reading provides a coherent and just basis for sovereignty. From the Palestinian perspective, it is a narrative of dispossession and denial of rights, enforced through coercion. The international community often attempts to bridge this gap by acknowledging historical ties while emphasizing modern international law and human rights, leading to a contested status for the founding problem.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state and Jewish diaspora are clear beneficiaries, as the narrative directly legitimizes their claims and identity (low directionality). The Palestinian population and Arab states are targets, as the narrative's enforcement directly extracts land, resources, and self-determination from them (high directionality). The international community is an observer, attempting to analyze and mediate without being a direct beneficiary or victim of this specific constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The 'founding problem' of Jewish statelessness is still considered live by proponents of this reading, preventing a clear mandatrophy resolution. However, the 'contested' status of the founding problem, coupled with high extractiveness and suppression, suggests that while the original mandate may persist, its application has accumulated significant extractive layers. The classification as Tangled Rope, rather than a pure Rope, captures this hybrid nature, preventing mislabeling genuine coordination as pure extraction or vice versa.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_promise_empirical_status,
    'Is the ''divine promise'' component of this legitimacy claim empirically verifiable or purely theological/deontological?',
    'Analysis of the epistemic grounding of the claim within its own tradition and its interaction with secular legal frameworks. If it is treated as an empirical claim, what evidence would falsify it?',
    'If purely theological, its interaction with secular international law is a conceptual boundary problem. If treated as an empirical claim, its lack of falsifiability would expose a performative aspect, increasing theater_ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_promise_empirical_status, conceptual, 'Epistemic status of the divine promise in the legitimacy claim.').

omega_variable(
    continuous_presence_demographic_threshold,
    'What constitutes ''continuous Jewish presence'' in the land, and at what demographic threshold does it maintain a claim to sovereignty, especially during periods of significant demographic absence?',
    'Historical demographic studies and legal analysis of ''presence'' definitions in international law. Does ''presence'' imply a continuous majority, or merely a continuous community?',
    'A strict demographic definition would weaken the historical continuity claim for certain periods, potentially reducing its legitimacy weight. A looser definition would reinforce the claim but might be challenged by competing claims of continuous presence by other groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuous_presence_demographic_threshold, empirical, 'Definition and threshold for ''continuous presence'' in territorial claims.').

omega_variable(
    partition_plan_as_compromise_vs_creation,
    'Is the UN Partition Plan (1947) best understood as a compromise of a pre-existing, divinely/historically granted right, or as the international community''s creation of a new right to statehood?',
    'Legal and historical analysis of the intent and effect of the Partition Plan within international law, and its reception by the parties at the time. Examination of how ''pre-existing right'' is defined and recognized.',
    'If a compromise, it reinforces the covenant-continuity reading''s claim to an inherent right. If a creation, it shifts the grounding of legitimacy more towards international recognition and away from the historical/divine, potentially weakening the claim to territories beyond the partition lines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_plan_as_compromise_vs_creation, conceptual, 'Interpretation of the UN Partition Plan''s role in sovereignty legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__covenant_continuity_reading, 1917, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1917, 0.1).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2000, 0.42).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1917, 0.3).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1917, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1917, 0.4).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 1967, 0.8).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__covenant_continuity_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__covenant_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__self_determination_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__covenant_continuity_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'territorial_sovereignty_legitimacy' kernel. Its claims about historical and divine rights directly influence, and are influenced by, competing readings based on self-determination and existential imperatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
