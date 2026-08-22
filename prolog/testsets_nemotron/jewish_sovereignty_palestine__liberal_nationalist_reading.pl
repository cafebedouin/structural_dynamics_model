% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination and Statehood in Ancestral Homeland (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The liberal nationalist reading of Jewish sovereignty in Palestine
 *   asserts that the Jewish people, as a nation, possess a collective right
 *   to self-determination, and that exercising this right through statehood
 *   in their ancestral homeland is legitimate. This reading emerged from the
 *   convergence of European liberal nationalism and Jewish emancipation
 *   politics, crystallized in the Zionist movement's Basel Program (1897),
 *   and gained international legal recognition through the Balfour
 *   Declaration (1917), League of Nations Mandate (1922), UN Partition Plan
 *   (1947), and Israel's admission to the UN (1949). Crucially, this reading
 *   structurally acknowledges Palestinian self-determination as a co-equal
 *   claim requiring territorial compromise — partition (two states) or a
 *   binational framework. The constraint's historical operation, however, has
 *   realized Jewish statehood while deferring and diminishing Palestinian
 *   sovereignty, creating a tangled rope: genuine coordination (Jewish
 *   collective security, cultural revival, democratic institutions)
 *   intertwined with asymmetric extraction (Palestinian displacement,
 *   occupation, denied statehood). The engine will compute per-seat
 *   classifications from this structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.48).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.55).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Self-Determination and Statehood in Ancestral Homeland (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, '77e96ec6-0c4c-490a-8a39-3f5f58aebd60').
narrative_ontology:cs_kernel_codification('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', formalized).
narrative_ontology:cs_authority_grounding('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', lineage).
narrative_ontology:cs_interpretation_layer_present('77e96ec6-0c4c-490a-8a39-3f5f58aebd60').
narrative_ontology:cs_reading_relation('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', jewish_sovereignty_palestine__post_zionist_reading, coexists_with).
narrative_ontology:cs_axiom('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', foundational, jewish_national_self_determination_universalizable).
narrative_ontology:cs_axiom_status(jewish_national_self_determination_universalizable, holdable).
narrative_ontology:cs_axiom_grounding('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', jewish_national_self_determination_universalizable, deontological).
narrative_ontology:cs_axiom('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', foundational, mutual_recognition_required_for_legitimate_sovereignty).
narrative_ontology:cs_axiom_status(mutual_recognition_required_for_legitimate_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', mutual_recognition_required_for_legitimate_sovereignty, deontological).
narrative_ontology:cs_axiom('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', secondary, democratic_state_compatible_with_jewish_national_character).
narrative_ontology:cs_axiom_status(democratic_state_compatible_with_jewish_national_character, holdable).
narrative_ontology:cs_axiom_grounding('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', democratic_state_compatible_with_jewish_national_character, conventional).
narrative_ontology:cs_reference_frame('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', liberal_nationalist_zionist_original_commitment).
narrative_ontology:cs_drift_state('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', contemporary_post_oslo_failure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('77e96ec6-0c4c-490a-8a39-3f5f58aebd60', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_national_rights_bearers).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_self_determination_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_institutions).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority_and_factions).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, collective_self_determination_right).
narrative_ontology:constraint_vindicates(jewish_sovereignty_palestine__liberal_nationalist_reading, national_self_rule_as_legitimate_political_form).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim collective self-determination as a nation with historical connection to the land; seek sovereign statehood as the legitimate political expression of that right. Benefit from international recognition of this right (Balfour, League of Nations mandate, UN Partition Plan, Israel's admission). Face ongoing security threats and legitimacy challenges; exit from the national project would mean abandoning the collective right they hold as foundational.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_national_rights_bearers, beneficiary,
    organized, generational, constrained, national).

% Hold a co-equal claim to self-determination in the same territory. The liberal nationalist reading acknowledges this claim and structurally requires territorial compromise (partition or binational framework) to resolve the overlap. However, the constraint's historical operation has concentrated the costs of compromise on Palestinians — displacement, military occupation, denied sovereignty — while the Jewish collective's statehood was realized. Their exit from the land is identity-locked; their political exit from the conflict requires the other side's recognition.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_self_determination_claimants, payer,
    moderate, generational, identity_locked, national).

% Administer the sovereign state that embodies the Jewish self-determination right. Set policies on borders, settlement, citizenship, and security that determine how much territorial compromise is actually offered. Benefit from the state's existence as the institutional vehicle of the national right. Constrained exit: the state cannot dissolve itself without negating the right it was created to secure.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, israeli_state_institutions, beneficiary).

% Administer limited self-governance under occupation (PA) or reject the framework entirely (Hamas, etc.). Negotiate or resist the terms of compromise. Bear the costs of the constraint's asymmetric realization. Constrained exit: armed resistance is suppressed; diplomatic pathways require counterpart consent; governance collapse deepens the population's vulnerability.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority_and_factions, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_authority_and_factions, agenda_setter).

% Provides the legal architecture (mandate, partition resolution, UNSC 242/338, Oslo framework, ICJ opinions) that simultaneously recognizes Jewish national rights and Palestinian self-determination. Observes and adjudicates the constraint's operation but lacks enforcement power to impose a resolution. Analytical exit: not a party to the conflict.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_legal_and_diplomatic_order, observer,
    institutional, generational, analytical, global).

% Benefit from the existence of a Jewish state as a symbolic and practical center of collective security, identity, and refuge. Mobilize political and material support for the state. Mobile exit: can disengage politically or emigrate individually without identity dissolution, though collective attachment remains.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, mobile, global).

% Hold the right of return and self-determination claims but are structurally excluded from the negotiation table and the territory. Bear the longest-running displacement costs of the constraint. Trapped exit: statelessness, host-country restrictions, and the right of return's non-realizability under current frameworks lock them out of both exit and voice.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_diaspora_and_refugees, excluded,
    powerless, generational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the statelessness and persecution vulnerability of the Jewish people by establishing a recognized sovereign state in their ancestral homeland, providing collective security, cultural continuity, and political agency.
% TRANSFER_FUNCTION: Moves territorial sovereignty and demographic control over historic Palestine from Ottoman/British imperial administration to a Jewish national state, while simultaneously (in this reading) requiring transfer of portions of that territory to a Palestinian sovereign entity — a bidirectional territorial compromise that has been partially implemented (Oslo Areas A/B, Gaza withdrawal) but remains incomplete and contested.
% ABSENT_VOICES: Palestinian refugees and diaspora (right of return claimants) are excluded from final-status negotiations. Mizrahi Jewish communities (from Arab/Muslim lands) were historically marginalized in the Zionist narrative and state-building despite constituting a demographic majority in early Israel. Internal Palestinian political pluralism (beyond PLO/PA/Hamas binary) is excluded by the bilateral negotiation framework.
% DISAPPEARANCE_RATIONALE: If the Jewish self-determination right and its territorial realization disappeared overnight, the Jewish collective would lose its sovereign political vehicle, returning to a condition of diaspora vulnerability. The Palestinian claim would lose its primary counterpart but not its territorial basis — the land and population would remain, requiring a new political arrangement. The international legal framework built around two-state partition would collapse. Regional security architecture (Camp David, Abraham Accords, Iran axis) would fundamentally reorganize.
% FOUNDING_PROBLEM: The Jewish people's historical statelessness and vulnerability to persecution in diaspora (pogroms, expulsions, Holocaust) created a need for collective self-protection and political agency. The liberal nationalist solution: a democratic nation-state in the ancestral homeland, exercising self-determination on terms compatible with universal liberal principles and the rights of other peoples in the territory.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by pre-state Zionist leadership (Herzl, Ben-Gurion, Weizmann) and the historical record of antisemitic persecution. Its status as 'contested' is corroborated by: (1) Palestinian national movement and historians who argue the founding problem was solved at their expense (Rashid Khalidi, Edward Said, Palestinian oral history); (2) Post-Zionist Israeli historians (Benny Morris, Tom Segev) documenting the displacement entailed in the solution; (3) International legal scholars noting the tension between Jewish self-determination and Palestinian self-determination in the same territory (UNSCOP minority report, ICJ 2024 advisory opinion). No external corroboration treats the founding problem as fully resolved without remainder.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.48 at interval end) reflects the moderate but persistent gap between the reading's stated commitment to mutual self-determination and the asymmetric realization: Jewish sovereignty achieved, Palestinian sovereignty partial and contingent. Suppression (0.55) captures the active enforcement required to maintain the constraint's current boundaries — military occupation, settlement expansion, blockade, legal restrictions on Palestinian movement and political agency — while acknowledging periods of negotiated reduction (Oslo, disengagement). Theater ratio (0.22) is moderate-low: the coordination function (Jewish national self-rule, democratic governance, Hebrew cultural revival) is real and substantial, but a growing share of state activity (settlements, nation-state law, judicial overhaul) serves to entrench asymmetric control rather than serve the declared liberal democratic framework. Accessibility collapse (0.42) is moderate: alternatives (binational state, confederation, enhanced autonomy) remain conceptually available but politically marginalized. Resistance (0.68) is high: Palestinian national resistance (armed, diplomatic, civil), Israeli peace camp advocacy, international legal pressure, and BDS all contest the constraint's asymmetric realization.
 *
 * PERSPECTIVAL GAP:
 *   From the Jewish national seat, the constraint is a rope: genuine coordination achieving collective liberation. From the Palestinian seat, it operates as a snare: the coordination story (two states) is cover for extraction (one state with permanent control). The Israeli institutional seat experiences it as a tangled rope: must maintain the liberal democratic frame while managing the occupation it claims to be temporary. The engine computes these divergences from the structural data — the claimed type (tangled_rope) represents this author's structural assessment, not a reconciliation of perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish collective as rights-bearing nation is the primary beneficiary (d near beneficiary end): the constraint was built for them, delivers their sovereign state, and its institutions serve their collective security and cultural continuity. Palestinian claimants are the primary payers (d near target end): they bear the territorial, demographic, and political costs of the compromise the reading requires but has not fully delivered. Israeli state institutions are agenda-setters with secondary beneficiary position: they administer the constraint and benefit from its realization but are constrained by its liberal democratic self-definition. Palestinian Authority/factions are payers with secondary agenda-setter role: they negotiate the terms of compromise but from a position of structural weakness. Diaspora Jews are beneficiaries with mobile exit; Palestinian refugees are excluded and trapped. International order is analytical observer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness and persecution vulnerability) was substantially solved by 1948/1967 — the Jewish people have a sovereign state with collective security. However, the constraint persists and expands (settlements, nation-state law, annexation discourse) because the mandate has not been updated to reflect the solved condition. The liberal nationalist reading's own logic requires territorial compromise with Palestinian self-determination; the failure to complete this compromise transforms the constraint from a solved coordination problem into an extractive one. Mandatrophy is unresolved: the arrangement continues to operate on a founding problem that no longer fully obtains, while the unresolved remainder (Palestinian sovereignty) becomes the extraction engine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mutual_recognition_as_structural_precondition,
    'Is mutual recognition of self-determination rights a structural precondition for the liberal nationalist reading''s legitimacy, or a contingent political outcome?',
    'Trace the reading''s internal logic: if the right to self-determination is universal (as liberal nationalism claims), then its exercise by one nation cannot structurally negate the same right for another nation in the same territory without contradiction. The reading either contains its own negation (if Palestinian rights are contingent) or requires mutual realization (if rights are universal).',
    'If mutual recognition is structural, the constraint''s current asymmetric realization is a deformation of the reading itself — the reading generates its own critique. If contingent, the reading can be sustained while Palestinian rights remain unrealized, making the extraction a feature, not a bug.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_recognition_as_structural_precondition, conceptual, 'Whether the liberal nationalist reading''s universalist premise structurally entails Palestinian statehood or merely aspirationally prefers it.').

omega_variable(
    territorial_compromise_boundary,
    'What territorial compromise does the liberal nationalist reading require? 1967 lines? 1947 lines? Confederation? Binational state?',
    'Examine the reading''s historical variants: Liberal Zionism (1920s-30s) accepted partition; Labor Zionism (1947-77) accepted 1947 lines then 1967 lines; Peace Now/Oslo camp (1978-2000) accepted 1967 lines with swaps; contemporary liberal Zionists split between two-state, confederation, and ''liberal democracy in one state'' positions. The reading does not specify a fixed boundary.',
    'If the reading has no determinate territorial content, its extractiveness cannot be measured independently of the political configuration that instantiates it. The constraint story may need to decompose into multiple readings (liberal_two_state, liberal_confederal, liberal_binational).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(territorial_compromise_boundary, conceptual, 'Indeterminacy of the territorial compromise within the liberal nationalist reading itself.').

omega_variable(
    committer_frame_structural_delta,
    'How does this reading''s structural delta (Palestinians as co-equal claimants, moderate extractiveness from territorial compromise) differ from sibling readings?',
    'Compare the beneficiary/victim structure and extractiveness profile across all five readings of the jewish_sovereignty_palestine kernel. This reading uniquely positions Palestinians as co-equal rights-bearers with a structural requirement for compromise; settler_colonial_reading positions Palestinians as primary victims of a displacement regime; religious_zionist_reading excludes Palestinian rights theologically; cultural_zionist_reading minimizes territorial politics; post_zionist_reading treats the ethnic-national framework itself as the extraction mechanism.',
    'Documents the kernel-reading decomposition for the corpus. Enables cross-reading comparison of how the same kernel generates different constraint structures. The omega serves as the committer-structure record required by Rules 2-4.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_structural_delta, conceptual, 'Commiter-frame record: this reading''s structural delta within the jewish_sovereignty_palestine kernel family.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1897, 0.05).
narrative_ontology:measurement(jewi_tr_t1917, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1917, 0.08).
narrative_ontology:measurement(jewi_tr_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1947, 0.12).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1948, 0.18).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 1993, 0.25).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1897, 0.15).
narrative_ontology:measurement(jewi_be_t1917, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1917, 0.22).
narrative_ontology:measurement(jewi_be_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1947, 0.38).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1967, 0.62).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 1993, 0.45).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 2024, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1897, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1897, 0.1).
narrative_ontology:measurement(jewi_su_t1917, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1917, 0.2).
narrative_ontology:measurement(jewi_su_t1947, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1947, 0.45).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 1993, 0.5).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.08).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_sovereignty_palestine kernel. The kernel decomposes into five constraint stories (one per reading), each with distinct beneficiary/victim structures, extractiveness profiles, and claimed types. This reading (liberal_nationalist) claims tangled_rope with moderate extractiveness and mutual recognition structure. The settler_colonial_reading claims snare with high extractiveness and Palestinian victims as primary. The religious_zionist_reading claims mountain (divine decree) or snare (theological exclusion). The cultural_zionist_reading claims rope (coordination without sovereignty). The post_zionist_reading claims piton or tangled_rope (founding narrative atrophied, ethnic framework persists). All five are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__liberal_nationalist_reading, organized, 0.3).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__liberal_nationalist_reading, moderate, 0.75).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__liberal_nationalist_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
