% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__partition_reading, []).

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
 *   constraint_id: territorial_legitimacy__partition_reading
 *   human_readable: Territorial Legitimacy via International Legal Partition (UN Resolution 181, 1948 Borders)
 *   domain: political_theory/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   The partition reading of territorial legitimacy instantiates a specific
 *   normative framework: legitimate statehood derives from UN-authorized
 *   territorial partition and international legal recognition of borders.
 *   Under this reading, both Israeli and Palestinian states gain legitimacy
 *   from the 1948 partition plan and subsequent state declarations;
 *   settlements beyond the 1967 lines are delegitimized as violations of the
 *   partition principle; a two-state solution with defined borders is
 *   structurally possible. This reading coexists with two sibling readings
 *   grounded in different legitimacy principles: indigenous continuity (which
 *   treats 1948 as a Nakba dispossession, not a legitimate partition) and
 *   security necessity (which treats post-1967 territorial expansion as
 *   justified by defensive requirements). The partition reading is neither
 *   logically foreclosed by these siblings nor universally accepted — it
 *   remains a contested but institutionally embedded framework within
 *   international law. The constraint exhibits tangled-rope characteristics:
 *   the partition reading solves a coordination problem (how to recognize
 *   post-colonial states in contested territory) while simultaneously
 *   extracting from displaced populations who are locked out of the new state
 *   system and from territorial claims holders whose claims are
 *   delegitimized. Suppression has risen from 0.45 to 0.68 over 50 years as
 *   enforcement mechanisms (border controls, settlement policies, legal
 *   frameworks criminalizing return) have matured. Theater ratio has risen
 *   from 0.35 to 0.52 as the original functional legitimacy (clear partition
 *   boundaries enabling state formation) has become obscured by 75 years of
 *   accumulated boundary violations, informal settlements, and fragmented
 *   territorial control.
 *
 * KEY AGENTS:
 *   - International Legal Order (UN, International Law Regime): Institutional beneficiary — partition reading provides clarity and precedent for post-colonial state recognition; has arbitrage exit options.
 *   - Displaced Palestinian Populations (1948): Primary victims — trapped by border closure and legal exclusion from return; no exit options; bear maximum extraction.
 *   - Jewish Communities Outside Partition Boundaries: Secondary victims — trapped by partition logic assigning them to Palestinian state; extraction flows from enforcement of homogeneity.
 *   - Partition-Based State Institutions (Israeli and Palestinian): Moderate institutional power with constrained exit — benefit from coordination (statehood, property rights, governance) but extract through concentration of authority.
 *   - International Peace Process / Two-State Solution Framework: Organized actors — see partition reading as temporary and revisable; constrained but have exit options through diplomatic negotiation.
 *   - Regional Powers (Turkey, Egypt, Saudi Arabia, Iran): Powerful institutional actors with mobile exit — benefit from coordination (recognized state partners) but experience suppression of intervention capacity.
 *   - Historical Legitimacy Institutions (1948 declarations, armistice agreements): Institutional incumbents — maintain partition ritual through inertia (piton classification) despite functional mismatch with current reality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy__partition_reading, 0.68).
domain_priors:theater_ratio(territorial_legitimacy__partition_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__partition_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__partition_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__partition_reading, "Territorial Legitimacy via International Legal Partition (UN Resolution 181, 1948 Borders)").
narrative_ontology:topic_domain(territorial_legitimacy__partition_reading, "political_theory/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__partition_reading, '3c687a47-4202-43ae-9d70-32216f0aa47f').
narrative_ontology:cs_kernel_codification('3c687a47-4202-43ae-9d70-32216f0aa47f', formalized).
narrative_ontology:cs_authority_grounding('3c687a47-4202-43ae-9d70-32216f0aa47f', lineage).
narrative_ontology:cs_interpretation_layer_present('3c687a47-4202-43ae-9d70-32216f0aa47f').
narrative_ontology:cs_reading_relation('3c687a47-4202-43ae-9d70-32216f0aa47f', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3c687a47-4202-43ae-9d70-32216f0aa47f', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_axiom('3c687a47-4202-43ae-9d70-32216f0aa47f', foundational, partition_authority_supersedes_prior_claims).
narrative_ontology:cs_axiom_status(partition_authority_supersedes_prior_claims, holdable).
narrative_ontology:cs_axiom_grounding('3c687a47-4202-43ae-9d70-32216f0aa47f', partition_authority_supersedes_prior_claims, conventional).
narrative_ontology:cs_axiom('3c687a47-4202-43ae-9d70-32216f0aa47f', foundational, statehood_requires_bounded_territory).
narrative_ontology:cs_axiom_status(statehood_requires_bounded_territory, holdable).
narrative_ontology:cs_axiom_grounding('3c687a47-4202-43ae-9d70-32216f0aa47f', statehood_requires_bounded_territory, conventional).
narrative_ontology:cs_reference_frame('3c687a47-4202-43ae-9d70-32216f0aa47f', un_authorized_partition_with_recognized_boundaries).
narrative_ontology:cs_drift_state('3c687a47-4202-43ae-9d70-32216f0aa47f', contemporary_post_1967, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3c687a47-4202-43ae-9d70-32216f0aa47f', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__partition_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, international_legal_order).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__partition_reading, partition_state_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, displaced_populations).
narrative_ontology:constraint_victim(territorial_legitimacy__partition_reading, territorial_claims_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED PALESTINIAN POPULATION (SNARE) — Trapped by border closure and legal exclusion from return; partition reading delegitimizes their territorial claim while locking them out of the new state system. No exit: cannot reclaim pre-1948 property, cannot establish independent state under this reading, cannot cross new borders. Maximum extraction — the partition mechanism itself is the suppression.
constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: JEWISH COMMUNITIES OUTSIDE PARTITION BOUNDARIES (SNARE) — Trapped by partition logic that assigns them to Palestinian state; extraction flows from the reading's enforcement of population homogeneity. Cannot reorganize territory, cannot exit without displacement.
constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: PARTITION-BASED STATE BUILDERS (TANGLED ROPE) — Moderate institutional power; constrained by international legal frameworks and neighboring state recognition. Both coordination (state institutions, property rights, governance structure) and extraction (concentration of authority, exclusion of minorities, control of border). Suppression is structural: the partition reading enforces homogeneity and suppresses alternative legitimacy claims.
constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL ORDER (ROPE) — Institutional beneficiary with arbitrage options. The partition reading solves a coordination problem: how to recognize post-colonial states in contested territory. UN Resolution 181 and the state recognition framework enable international governance, treaties, and diplomatic relations. The reading benefits the international legal system by providing clarity and precedent. Experiences minimal extraction because the system has exit options (can revise readings) and generates net coordination value.
constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TWO-STATE SOLUTION FRAMEWORK (SCAFFOLD) — Organized actors (UN mediators, international peace processes, diplomatic community) see the partition reading as temporary and revisable. The scaffold logic: international legal partition establishes two legitimate states with defined borders; ongoing negotiation addresses displaced populations, settlements, and property claims. Sunset logic: two-state framework is a transitional structure toward final status agreements. Effective extraction is low because organized actors see agency and an exit path (negotiated resolution). Temporal expectation: 15-30 years for final status settlement.
constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: 1948 PARTITION RITUAL (PITON) — The partition reading's original institutional form (UN Resolution 181, 1948 declaration, armistice agreements) persists largely through inertia and historical reverence rather than current functional legitimacy. The ritual maintains borders drawn 75+ years ago despite massive demographic and political shifts. Theater ratio is moderate-high (0.52) because the reading's appeal to 'international legal order' is increasingly performative — the actual territorial reality (settlements, Palestinian state non-independence, fragmented territory) does not match the partition logic. Piton classification: the reading survives because of institutional path-dependence and the absence of a formally competing recognition framework, not because it solves current territorial problems.
constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: REGIONAL POWER ACTORS (TANGLED ROPE) — Powerful institutional actors with mobile exit options (can shift alliances, mediate, or escalate). The partition reading provides coordination benefit: it establishes recognized state entities they can negotiate with and form treaties with. But it also extracts: the reading constrains their ability to intervene in internal territorial disputes and locks them into recognizing borders they may dispute. Suppression is moderate: they can exit through non-recognition or military intervention, but the costs are high (sanctions, diplomatic isolation).
constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, the partition reading can appear as reflecting an immutable logic: post-colonial territories must be partitioned into recognized sovereign states; territorial claims require legal boundaries; the international system cannot function without state recognition. This perspective risks naturalizing what is actually a contingent institutional arrangement grounded in post-WWII liberal internationalism. The engine's false-summit detection will evaluate whether beneficiaries of this 'natural law' framing exist and whether the suppression is genuinely indispensable or instrumentally imposed.
constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__partition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(territorial_legitimacy__partition_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__partition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(territorial_legitimacy__partition_reading, TR),
    TR >= 0.70.

:- end_tests(territorial_legitimacy__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The partition reading generates both coordination and extraction. Coordination value: it enables state formation, property rights systems, and diplomatic relations — legitimate institutional functions. Extraction value: displaced populations are permanently excluded from return; territorial claims holders are delegitimized; minorities within the partition boundaries face assimilation pressure. The rising trajectory (0.42 → 0.58) reflects that over 75 years, extractive outcomes have accumulated — enforcement mechanisms (property laws, border controls, citizenship rules) have hardened; alternative legitimacy claims have been suppressed; the original 'clean partition' has given way to overlapping territorial claims and informal settlements. Suppression (0.68): Moderate-high and rising. Suppression is structural to partition enforcement: borders must be enforced, movement controlled, alternative claims delegitimized. Initial suppression (0.45) was the bare minimum required for partition; current suppression (0.68) includes all the mechanisms needed to maintain partition boundaries against displacement pressures and counter-claims. Theater ratio (0.52): Moderate and rising. The 1948 partition had clear functional legitimacy: it solved the immediate problem of recognizing post-colonial states. Current theater ratio reflects that the same partition reading is invoked to justify very different territorial realities — the reading functions as a legitimacy cover story for boundary management that has little to do with the original 1948 logic. Rising theater suggests the partition reading is becoming increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   The partition reading exhibits dramatic perspectival divergence across observer positions. The international legal order sees rope (clean coordination mechanism). Partition-based state builders see tangled rope (coordination benefits mixed with extraction from minorities and displaced populations). Displaced populations see snare (trapped, no exit, pure extraction). The two-state solution framework sees scaffold (temporary structure with negotiated sunset). The piton perspective shows institutional inertia: the 1948 partition ritual survives not because it functions but because no competing formal legitimacy framework has replaced it. The analytical observer risks seeing mountain (immutable logic of state partition), but structural data reveals false summit: beneficiaries (international legal order, partition-based institutions) exist; suppression is active and rising; extraction flows from specific choices about homogeneity and displacement, not from natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is computed from the agent's structural relationship to the extraction flow and their exit options. Displaced populations (trapped, no exit, pure victims) receive d = 0.95, producing maximum f(d) ≈ 1.42. Partition-based state builders (moderate power, constrained exit, both beneficiary and victim depending on position) receive d ≈ 0.55, producing f(d) ≈ 0.75. International legal order (institutional, arbitrage exit, beneficiary) receives d ≈ 0.10, producing f(d) ≈ -0.01. Two-state solution framework (organized, constrained exit, sees agency) receives d ≈ 0.45, producing f(d) ≈ 0.40. The piton perspective (institutional, arbitrage, sees own degradation) uses canonical d ≈ 0.15, producing f(d) ≈ -0.01. The analytical observer (analytical, analytical context) uses canonical d ≈ 0.73, producing f(d) ≈ 1.15. No directionality overrides needed: the structural data (beneficiaries, victims, exit options) produces coherent directionality derivation across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The partition reading resolves mandatrophy by identifying which type emerges from which structural position. This is not a question of which type is 'correct' but which type accurately captures the experienced constraint from different positions. The snare perspective (displaced populations) is correct about their structural reality: trapped, no exit, pure extraction. The rope perspective (international legal order) is correct about coordination benefits. The tangled rope perspective (partition-based state builders) is correct about mixed coordination and extraction. The scaffold perspective (two-state solution) is correct about agency and sunset logic. The piton perspective identifies institutional inertia as the maintenance mechanism. The mountain perspective risks naturalization. All six types are legitimate readings of the same constraint from different structural positions. The mandatrophy resolves through perspectival honesty: acknowledge that the partition reading legitimizes for some agents while extracting from others, and that suppression is structural rather than incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    partition_vs_indigenous_continuity,
    'Is the partition reading''s legitimacy claim foundationally incompatible with indigenous continuity claims, or do they coexist as competing frameworks held by different parties?',
    'Jurisprudential analysis: can a legal system simultaneously affirm both 1948 partition boundaries AND continuous indigenous Palestinian presence? Court rulings, UN committee positions, and treatises on the status of the Nakba in international law.',
    'If forecloses: partition reading logically rules out indigenous continuity reading in any unified legal framework. If coexists: both readings remain live — different parties (Israeli state institutions vs Palestinian civil society) hold different readings without internal logical contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_vs_indigenous_continuity, conceptual, 'Logical relationship between partition and indigenous continuity readings').

omega_variable(
    security_necessity_constraint_hierarchy,
    'Does security necessity reading subordinate partition reading, or are they independent frameworks that sometimes conflict?',
    'Historical legal analysis: cases where Israeli courts or government institutions invoked security necessity to override partition boundaries (settlements, walls, blockades). If security necessity is treated as a superior constraint that can override partition boundaries, security reading influences or forecloses partition reading.',
    'If influences: security reading creates downstream pressure on partition reading, weakening its authority. If independent: partition reading retains autonomous legitimacy despite security-driven boundary violations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_necessity_constraint_hierarchy, empirical, 'Hierarchical relationship between security necessity and partition readings').

omega_variable(
    international_law_binding_force,
    'Does UN Resolution 181''s non-binding advisory status undermine the partition reading''s legitimacy claim, or does subsequent state recognition (1948 Israeli Declaration, 1988 Palestinian Declaration) provide sufficient binding force?',
    'International law analysis: treatment of UN General Assembly resolutions as sources of law; ICJ rulings on state recognition and legitimacy; customary international law formation from repeated state practice.',
    'If non-binding: partition reading lacks hard legal foundation — legitimacy depends on state practice rather than UN text. If binding or sufficiently formalized: partition reading has stable legal grounding independent of shifting state interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_law_binding_force, conceptual, 'Legal binding force of UN Resolution 181 and state recognition').

omega_variable(
    settler_legitimacy_internal_contradiction,
    'Can the partition reading coherently justify the 1948 partition as legitimate while also delegitimizing post-1967 settlements as illegal, or is there a logical inconsistency in the partition framework itself?',
    'Legal theory analysis: if both 1948 partition and 1967 settlements are applications of the same partition principle (population transfer, border drawing), what distinguishes legitimate from illegitimate partition? If the distinction rests on international legal process (UN authorization vs unilateral action), does partition reading subordinate its own logic to procedural authority?',
    'If inconsistent: partition reading is unstable and vulnerable to deconstruction. If consistent: partition reading has internal coherence but may require additional procedural constraints (authorization requirement, international consensus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_legitimacy_internal_contradiction, conceptual, 'Internal consistency of partition reading across 1948 and post-1967 boundary claims').

omega_variable(
    displaced_population_return_impossibility,
    'If partition reading legitimacy requires accepting 1948 partition boundaries as closed and irreversible, is the right of return for displaced populations logically foreclosed, or can it coexist with partition legitimacy?',
    'Jurisprudential analysis: treatment of right of return in partition-based frameworks (e.g., India-Pakistan partition, Greek-Turkish population exchange). Can partition reading accommodate return rights through compensation, resettlement, or property restitution without dismantling the reading itself?',
    'If foreclosed: partition reading structurally prevents displaced population return — extraction is inherent to the reading. If coexist: return rights can be addressed through mechanisms outside the partition framework (third-country resettlement, compensation funds, special status). Impacts whether snare classification for displaced populations is permanent or temporary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displaced_population_return_impossibility, conceptual, 'Logical compatibility of right of return with partition reading legitimacy').

omega_variable(
    homogeneity_requirement_theoretical_foundation,
    'Does partition reading require ethnic/religious homogeneity as a precondition for state legitimacy, or can legitimate states be multinational under partition logic?',
    'Partition theory analysis: historical partition cases (India-Pakistan, Cyprus, Korea) and their outcomes; contemporary international law on minority rights and state legitimacy; theoretical debate on whether partition assumes or requires population homogeneity.',
    'If requires homogeneity: partition reading inherently drives suppression and population transfer. If permits multinationality: suppression is a contingent political choice, not structural to the reading. Affects whether suppression (0.68) is intrinsic or contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homogeneity_requirement_theoretical_foundation, conceptual, 'Whether partition reading requires ethnic/religious homogeneity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__partition_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_part_tr_t0, territorial_legitimacy__partition_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(terr_part_tr_t25, territorial_legitimacy__partition_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement(terr_part_tr_t50, territorial_legitimacy__partition_reading, theater_ratio, 50, 0.52).

% Extraction over time
narrative_ontology:measurement(terr_part_be_t0, territorial_legitimacy__partition_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(terr_part_be_t25, territorial_legitimacy__partition_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(terr_part_be_t50, territorial_legitimacy__partition_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_part_su_t0, territorial_legitimacy__partition_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(terr_part_su_t25, territorial_legitimacy__partition_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(terr_part_su_t50, territorial_legitimacy__partition_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__partition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__indigenous_continuity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, territorial_legitimacy__security_necessity_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, settlement_expansion_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, right_of_return_dispute).
narrative_ontology:affects_constraint(territorial_legitimacy__partition_reading, state_recognition_sovereignty).

% DUAL FORMULATION NOTE:
% Territorial legitimacy is a kernel with three distinct readings. Each reading is a separate constraint story with its own epsilon, beneficiary/victim structure, and classification. The partition reading (THIS STORY) has ε=0.58 and tan-gled_rope classification. Indigenous continuity reading has different ε and snare characteristics. Security necessity reading has different ε and tangled_rope or snare characteristics depending on framing. Network links connect these readings as siblings within the kernel and as upstream influences on downstream constraints like settlement expansion and right of return disputes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
