% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space_contraction, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: total_war_possibility_space__space_contraction_reading
 *   human_readable: Total War Categorical Removal from Strategic Possibility Space (Space Contraction Reading)
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint story instantiates the 'space contraction' reading of the
 *   contested kernel 'total war possibility space.' The reading asserts that
 *   nuclear weapons removed total war from the set of strategically thinkable
 *   actions for state planners — not because it became catastrophically
 *   costly (the deterrence-equilibrium reading) or normatively forbidden (the
 *   nuclear-taboo reading), but because the logical architecture of warfare
 *   itself contracted. Once mutual thermonuclear destruction became assured,
 *   the pre-nuclear strategic calculus that made total mobilization and
 *   victory coherent ceased to function. Total war is not deterred; it has
 *   exited the possibility space entirely. This reading predicts
 *   institutional atrophy: mobilization doctrine disappears, general staff
 *   war-gaming for great-power conflict ceases, strategic studies shifts to
 *   sub-nuclear domains. The constraint is categorically presented as a
 *   feature of reality (the logic of nuclear-armed strategy), not as an
 *   arrangement whose operation extracts from some and benefits others.
 *   However, the beneficiary declaration (humanity as biological substrate)
 *   triggers False Summit Mountain evaluation: the constraint benefits
 *   identifiable parties (nuclear-armed states, the global strategic
 *   establishment, humanity's survival) and requires continuous institutional
 *   teaching and reinforcement, raising the question of whether it is natural
 *   law or constructed constraint wearing the mask of nature.
 *
 * KEY AGENTS:
 *   - humanity_as_biological_substrate: Universal beneficiary (non-agent, analytical entry) — the constraint removes total war from thinkability, preventing species-level destruction.
 *   - military_general_staff_apparatus: Institutional payer — experiences total-war planning as institutionally impossible, not merely costly; war-gaming doctrine atrophies.
 *   - strategic_studies_discipline: Analytical observer — the discipline's shift from total-war to sub-nuclear modeling is evidence of the constraint's operation.
 *   - deterrence_equilibrium_partisans: Excluded analytical voices — they read the same kernel (total-war removal) but dispute the mechanism (cost-raising vs. space-contraction).
 *   - nuclear_taboo_partisans: Excluded analytical voices — they read the same kernel but locate the constraint in normative taboo rather than logical structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.0).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.0).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Total War Categorical Removal from Strategic Possibility Space (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, 'f275eb65-b449-47b2-8767-d6e9e7e94ec7').
narrative_ontology:cs_kernel_codification('f275eb65-b449-47b2-8767-d6e9e7e94ec7', implicit).
narrative_ontology:cs_authority_grounding('f275eb65-b449-47b2-8767-d6e9e7e94ec7', expertise).
narrative_ontology:cs_interpretation_layer_present('f275eb65-b449-47b2-8767-d6e9e7e94ec7').
narrative_ontology:cs_reading_relation('f275eb65-b449-47b2-8767-d6e9e7e94ec7', total_war_possibility_space__deterrence_equilibrium_reading, influences).
narrative_ontology:cs_reading_relation('f275eb65-b449-47b2-8767-d6e9e7e94ec7', total_war_possibility_space__nuclear_taboo_reading, influences).
narrative_ontology:cs_axiom('f275eb65-b449-47b2-8767-d6e9e7e94ec7', foundational, mutual_nuclear_destruction_erases_total_victory_calculus).
narrative_ontology:cs_axiom_status(mutual_nuclear_destruction_erases_total_victory_calculus, holdable).
narrative_ontology:cs_axiom_grounding('f275eb65-b449-47b2-8767-d6e9e7e94ec7', mutual_nuclear_destruction_erases_total_victory_calculus, empirically_contingent).
narrative_ontology:cs_axiom('f275eb65-b449-47b2-8767-d6e9e7e94ec7', foundational, logical_incoherence_erases_option_more_completely_than_cost).
narrative_ontology:cs_axiom_status(logical_incoherence_erases_option_more_completely_than_cost, holdable).
narrative_ontology:cs_axiom_grounding('f275eb65-b449-47b2-8767-d6e9e7e94ec7', logical_incoherence_erases_option_more_completely_than_cost, deontological).
narrative_ontology:cs_reference_frame('f275eb65-b449-47b2-8767-d6e9e7e94ec7', pre_nuclear_total_war_strategic_possibility).
narrative_ontology:cs_drift_state('f275eb65-b449-47b2-8767-d6e9e7e94ec7', post_assured_destruction_establishment, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('f275eb65-b449-47b2-8767-d6e9e7e94ec7', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, humanity_as_biological_substrate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, military_general_staff_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The human species as the ultimate target-set of total war. Under this reading, nuclear weapons remove total war from the set of strategically thinkable actions for state planners — not because it is costly or forbidden, but because the logical architecture of warfare itself has contracted: the option space has closed, not remained open with entry barriers. This is a non-agent entry (a category, not an actor) kept for analytical completeness.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, humanity_as_biological_substrate, beneficiary,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(total_war_possibility_space__space_contraction_reading, humanity_as_biological_substrate).

% The military planning establishments of nuclear-armed states. Under this reading, they experience total-war planning as institutionally impossible, not merely undesirable. Their doctrinal apparatus atrophies; war-gaming for great-power conflict shifts from total-war scenarios to sub-nuclear domains. The cognitive and institutional machinery that once generated general mobilization orders, total economic coordination, and civilian-target planning becomes vestigial.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, military_general_staff_apparatus, payer,
    institutional, generational, constrained, global).

% The academic and professional field that analyzes war, strategy, and conflict. Under this reading, strategic studies observes and articulates the constraint: the discipline's own shift away from great-power total-war modeling and toward sub-nuclear, asymmetric, and proxy conflict analysis is evidence of the possibility-space contraction, not merely changing research fashion.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_discipline, observer,
    analytical, generational, analytical, global).

% Scholars and strategists who hold the deterrence-equilibrium reading (sibling reading). They would argue that total war remains strategically thinkable but is deterred by mutual vulnerability — that the space remains open, with entry blocked by cost, not by categorical erasure. They are excluded from this reading's institutional frame because this reading's core premise is that the space itself has contracted.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, deterrence_equilibrium_partisans, excluded,
    analytical, generational, analytical, global).

% Scholars and strategists who hold the nuclear-taboo reading (sibling reading). They would argue that total war became normatively prohibited through constructed, evolving taboo, independent of material capability — that the constraint is normative/cultural, not categorical/logical. They are excluded from this reading's institutional frame because this reading locates the constraint in the logical structure of war itself, not in taboo.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_taboo_partisans, excluded,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does not identify a coordination problem the constraint solves. Total war's removal from possibility space is presented as a categorical logical fact, not as a solution to a coordination failure. The benefit is survival of the biological substrate; the mechanism is structural contraction of the option space, not institutional coordination around a shared problem.
% TRANSFER_FUNCTION: No transfer is modeled in this reading. There is no extraction because the constraint is understood as a feature of reality (the logical structure of nuclear-armed warfare), not as an arrangement whose operation moves resources from one agent to another. Any cost — atrophy of military institutions, shift in strategic doctrine — is incidental to the categorical change in possibility space, not the constraint's mechanism.
% ABSENT_VOICES: Practitioners of total-war doctrine from the pre-nuclear era cannot speak from this reading's frame, because the constraint is categorical — total war is not prohibited or deterred, it is logically removed from the option set. The voices excluded are those who would argue that the option remains thinkable, available, merely costly: deterrence-equilibrium partisans and nuclear-taboo scholars. They are structurally outside this reading because they dispute the core claim that possibility space has contracted.
% DISAPPEARANCE_RATIONALE: If this constraint — the categorical removal of total war from possibility space — disappeared overnight, it would not mean total war became preferable; it would mean the reading's core claim is false. The structure of nuclear-armed warfare would remain unchanged. No rearrangement of institutions or resources would be required because the constraint is not an institutional arrangement but a fact about the logical structure of war itself. A reading that proposes the constraint is categorical cannot coherently propose its disappearance — its disappearance would disprove the reading, not reorganize the world.
% FOUNDING_PROBLEM: The existential threat posed by strategic nuclear exchange: the capability to destroy modern civilization via rapid intercontinental delivery of thermonuclear warheads made the pre-nuclear doctrinal goal of total mobilization and total victory logically incoherent. Once mutual destruction is assured, the strategic calculus that once made total war a rational option (victory through superior mobilization and will) ceases to function.
% FOUNDING_PROBLEM_CORROBORATION: This reading is corroborated by the observed institutional atrophy of total-war planning apparatus in nuclear-armed states post-1960s (documented in military history scholarship: Gaddis, Brodie, Lawrence Freedman). General staff institutions that once organized mobilization doctrine have been repurposed or downsized; great-power war-gaming centers on limited scenarios. Strategic studies as a discipline has shifted research focus away from total-war modeling toward sub-nuclear conflict. However, deterrence-equilibrium partisans (Schelling, contemporary rational-choice strategists) contest this, arguing the space remains open and is merely deterred. The founding problem's status as 'contested' reflects the disagreement among practitioners and scholars about whether the constraint is categorical (space contracted) or merely cost-raising (space remains open with entry barred by mutual vulnerability).
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.0, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__space_contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(total_war_possibility_space__space_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This is a mountain constraint under its claimed type — extractiveness and suppression are authored at zero because the constraint is presented as a categorical logical fact, not as a constructed arrangement. Accessibility collapse is very high (0.92) because once nuclear weapons establish mutual destruction, total-war planning becomes not forbidden but logically incoherent — alternatives don't just collapse, the option set itself shrinks. Resistance is near-zero (0.05) not because the constraint faces no opposition (deterrence theorists and nuclear-taboo scholars actively contest it), but because the constraint as a logical fact does not depend on enforcement — it is self-sustaining. The temporal measurement series are flat across all metrics because a categorical logical constraint does not drift; it either obtains or it does not. The interval spans 1945-2024 to cover the period from nuclear weapons' emergence through their maturation and the present strategic environment.
 *
 * PERSPECTIVAL GAP:
 *   The military general staff apparatus experiences this constraint asymmetrically from strategic studies observers. For military institutions, the constraint operates as institutional impossibility: total-war planning cannot be conducted without immediate cognitive dissonance (you cannot rationally plan to mobilize your entire society for victory if victory means mutual annihilation). For strategic scholars, the constraint is an intellectual fact — a shift in the conceptual tools of strategy theory itself. Neither seat experiences the constraint as a coordination problem they collectively solved; both experience it as a feature of the post-1945 strategic environment that has become background reality.
 *
 * DIRECTIONALITY LOGIC:
 *   This constraint does not fit the standard directionality framework because it is presented as natural law rather than constructed. However, the beneficiary declaration (humanity as biological substrate) initiates False Summit evaluation: if identifiable parties benefit from the constraint and institutional maintenance is required, directionality emerges. Nuclear-armed states benefit from the removal of total-war-as-option because it preserves the possibility of limited nuclear exchange (deterrence) while precluding the pre-nuclear path to total victory through mobilization. Non-nuclear states are excluded from this benefit but also protected from becoming total-war targets of great powers. The constraint's beneficiaries are structural (the order-preserving great-power system) rather than individual agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophy in the technical sense (a mandate has not outlived its function; the mandate — prevent total war via removing it from possibility space — remains active). However, the constraint exhibits structure similar to mandatrophy's institutional signature: the apparatus that would plan and execute total war has atrophied, yet military establishments maintain the pretense of total-war capability for deterrence purposes. The theater is not in the constraint itself (extractiveness is zero) but in institutional maintenance of doctrine that no one believes in. This is 'piton-lite': not a pure piton (gain_flow is diffuse, no concentrated beneficiary capturing extraction) but an institutional arrangement whose original coordination function (deter total war via credible retaliation threat) persists, while the machinery that would execute the threat has become theoretical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    space_contraction_vs_cost_raising,
    'Is total war removed from the possibility space (logical/categorical constraint on what strategic planners can coherently think as an option), or merely removed from the preferable space (high-cost option that rational actors avoid but could in principle consider)?',
    'Examine military doctrine and strategic studies output post-1960s: if total-war scenarios disappear entirely from planning documents and scholarly literature, supporting space contraction; if they persist but are labeled unthinkable/irrational rather than impossible, suggesting cost-raising. Post-1990s strategic studies shift to asymmetric and sub-nuclear conflict modeling despite stable nuclear arsenals suggests contraction, not merely rising cost.',
    'If the space has genuinely contracted, the constraint is categorical (mountain per this reading) and institutional atrophy is predicted. If cost has merely risen, the space remains open and the constraint is deterrence-based (tangled rope per the deterrence-equilibrium reading), making total-war doctrine latent but recoverable. This omega documents the core disagreement between this reading and the sibling deterrence-equilibrium reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(space_contraction_vs_cost_raising, empirical, 'Whether the constraint is a categorical removal of total war from possibility space or merely a cost-raising barrier to thinkability.').

omega_variable(
    kernel_identity_under_reading,
    'Is ''total war possibility space'' the same kernel under all three readings (deterrence_equilibrium, nuclear_taboo, space_contraction), or do the readings instantiate different constraints altogether?',
    'Examine the referent each reading takes: this reading asserts the kernel is the logical structure of nuclear-armed warfare (what is strategically thinkable); the deterrence reading asserts the kernel is mutual vulnerability (what deters); the taboo reading asserts the kernel is normative prohibition (what is culturally forbidden). If the readings are reading the same standing arrangement (the post-1945 nuclear order), they are readings of one kernel instantiated three ways. If they are reading different aspects or mechanisms, they may be different constraints wearing the same label.',
    'If this reading and its siblings read a single kernel differently, the engine''s constraint-family network links them validly. If they read different kernels, decomposition into separate constraint families is required. This omega documents the under-determination of the kernel concept itself across the three readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_under_reading, conceptual, 'Whether the three readings of ''total war possibility space'' read a single kernel or decompose into separate constraint families.').

omega_variable(
    natural_law_vs_constructed_mountain,
    'Is the categorical removal of total war from strategic possibility space a natural law (a logical truth about nuclear-armed warfare regardless of human choice), or a constructed constraint that benefits specific parties (military establishments, nuclear-armed states, global north survival) and therefore falsely appears as natural?',
    'Test whether the constraint''s persistence requires active institutional defense (doctrine maintenance, nuclear-doctrine teaching, strategic-discourse framing) or operates as automatic fact. If strategic establishments must continuously teach and reinforce that total war is unthinkable, the constraint may be a beneficiary-sustained narrative dressed as natural law. If total-war plans would spontaneously disappear from the option set regardless of institutional maintenance, it is categorical.',
    'If the constraint is constructed and benefits nuclear-armed states'' survival and great-power stability, False Summit Mountain (FSM) detection fires: the constraint reclassifies from mountain to tangled_rope (coordination of great-power non-war with asymmetric extraction from non-nuclear states in proxy domains). If categorical, the mountain claim holds and FSM does not fire. This omega is triggered by the beneficiary declaration (humanity as biological substrate) on a mountain, per FSM rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_mountain, conceptual, 'Whether the constraint is a categorical logical fact or a constructed arrangement benefiting specific parties.').

omega_variable(
    institutional_atrophy_as_evidence,
    'Does the observed atrophy of total-war planning institutions provide evidence for space contraction, or is it consistent with cost-raising + rational de-prioritization?',
    'Distinguish between institutional atrophy (functions disappear, cannot be recovered without major reconstruction) and institutional repurposing (functions shift to other domains but machinery remains reversible). If total-war planning capacity would require decades to rebuild and is treated as obsolete doctrine, atrophy supports space contraction. If it could be rapidly reactivated and remains in reserves, cost-raising is consistent.',
    'Institutional irreversibility supports this reading''s space-contraction claim. Institutional reversibility is consistent with the sibling deterrence-equilibrium reading (space open, entry merely costly). Post-1990s evidence: no major military has rebuilding total-war doctrines on standby, suggesting atrophy and contraction. But this could also reflect rational resource allocation given the actual threat environment, not logical impossibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_atrophy_as_evidence, empirical, 'Whether institutional atrophy of total-war planning reflects categorical removal or rational de-prioritization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.0).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__space_contraction_reading, theater_ratio, 1962, 0.0).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__space_contraction_reading, theater_ratio, 1980, 0.0).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__space_contraction_reading, theater_ratio, 2000, 0.0).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__space_contraction_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.0).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1962, 0.0).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1980, 0.0).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2024, 0.0).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_possibility_space__space_contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.0).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).

% DUAL FORMULATION NOTE:
% The constraint 'total_war_possibility_space' decomposes into three structurally distinct readings. This story (space_contraction_reading) instantiates the reading asserting total war is categorically removed from strategic possibility space — a logical feature of nuclear-armed warfare, not a cost-raising or normative constraint. The sibling stories (deterrence_equilibrium_reading, nuclear_taboo_reading) instantiate readings that locate the constraint in mutual vulnerability and constructed taboo, respectively. The three readings read the same kernel (the post-1945 removal of total war from great-power strategic options) but produce different ε values, different beneficiary structures, and different institutional predictions. They are linked via network.affects_constraints because the space_contraction reading directly influences and constrains the possibility space the other two readings reason within: if the logical architecture of war has contracted, deterrence and taboo operate within that contraction, not as independent mechanisms. The network graph runs upstream (space-contraction is foundational), not symmetric.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
