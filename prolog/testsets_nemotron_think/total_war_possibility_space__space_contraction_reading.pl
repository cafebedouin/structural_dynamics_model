% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__space_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__space_contraction_reading, []).

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
 *   human_readable: Nuclear-Induced Total War Unthinkability (Space Contraction Reading)
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint story captures the space_contraction_reading of the
 *   total_war_possibility_space kernel. The reading asserts that nuclear
 *   weapons did not merely raise the cost of total war
 *   (deterrence_equilibrium_reading) or create a normative taboo
 *   (nuclear_taboo_reading), but categorically removed total war from the
 *   strategically thinkable — the possibility space itself contracted. The
 *   constraint is the cognitive-institutional boundary: great-power general
 *   staffs no longer war-game total war, mobilization doctrines have
 *   atrophied, civil defense has hollowed out, and strategic studies has
 *   migrated to sub-nuclear domains. The reading claims this is a Mountain —
 *   a structural feature of the nuclear age, not a policy choice. But
 *   identifiable beneficiaries exist (states freed from total war preparation
 *   costs), triggering false summit detection. The metrics reflect the
 *   reading's own lights: low extractiveness (the constraint prevents
 *   catastrophe rather than extracting), low suppression (unthinkability is
 *   accepted, not enforced), high accessibility collapse (alternatives
 *   genuinely vanish), low resistance (no serious actor advocates total war
 *   planning). The engine will compute per-seat classifications from the
 *   structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__space_contraction_reading, 0.15).
domain_priors:suppression_score(total_war_possibility_space__space_contraction_reading, 0.1).
domain_priors:theater_ratio(total_war_possibility_space__space_contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(total_war_possibility_space__space_contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__space_contraction_reading, mountain).
narrative_ontology:human_readable(total_war_possibility_space__space_contraction_reading, "Nuclear-Induced Total War Unthinkability (Space Contraction Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__space_contraction_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:emerges_naturally(total_war_possibility_space__space_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__space_contraction_reading, 'e9a91c76-14d3-4cde-b761-4df5a46cabac').
narrative_ontology:cs_kernel_codification('e9a91c76-14d3-4cde-b761-4df5a46cabac', fixed_text).
narrative_ontology:cs_authority_grounding('e9a91c76-14d3-4cde-b761-4df5a46cabac', lineage).
narrative_ontology:cs_interpretation_layer_present('e9a91c76-14d3-4cde-b761-4df5a46cabac').
narrative_ontology:cs_reading_relation('e9a91c76-14d3-4cde-b761-4df5a46cabac', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9a91c76-14d3-4cde-b761-4df5a46cabac', total_war_possibility_space__nuclear_taboo_reading, coexists_with).
narrative_ontology:cs_axiom('e9a91c76-14d3-4cde-b761-4df5a46cabac', foundational, total_war_categorically_unthinkable).
narrative_ontology:cs_axiom_status(total_war_categorically_unthinkable, holdable).
narrative_ontology:cs_axiom_grounding('e9a91c76-14d3-4cde-b761-4df5a46cabac', total_war_categorically_unthinkable, empirically_contingent).
narrative_ontology:cs_axiom('e9a91c76-14d3-4cde-b761-4df5a46cabac', secondary, mobilization_apparatus_atrophy_inevitable).
narrative_ontology:cs_axiom_status(mobilization_apparatus_atrophy_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('e9a91c76-14d3-4cde-b761-4df5a46cabac', mobilization_apparatus_atrophy_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('e9a91c76-14d3-4cde-b761-4df5a46cabac', classical_total_war_framework).
narrative_ontology:cs_drift_state('e9a91c76-14d3-4cde-b761-4df5a46cabac', nuclear_age, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e9a91c76-14d3-4cde-b761-4df5a46cabac', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__space_contraction_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_great_powers).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_armed_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, nuclear_umbrella_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__space_contraction_reading, conventional_military_establishments).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, conventional_military_establishments).
narrative_ontology:constraint_victim(total_war_possibility_space__space_contraction_reading, civil_defense_agencies).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, nuclear_deterrence_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, mutual_assured_destruction_doctrine).
narrative_ontology:constraint_vindicates(total_war_possibility_space__space_contraction_reading, strategic_stability_concept).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established and maintain the nuclear deterrence framework that defines the strategic possibility space. They authored the doctrines, built the arsenals, and created the institutional arrangements (NPT, arms control, crisis management) that make total war cognitively inaccessible. They do not experience this as a constraint they bear; they experience it as the strategic reality they constructed and now inhabit.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_great_powers, agenda_setter,
    institutional, generational, analytical, global).

% Possess nuclear weapons but lack the global reach of the great powers. They benefit enormously from the contraction of the total war possibility space: they need not maintain mass mobilization armies, civil defense, or total war economies. Their security is guaranteed by the unthinkability of great-power nuclear exchange. Exit would mean disarmament or proliferation crises — constrained but not trapped.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_armed_states, beneficiary,
    powerful, generational, constrained, regional).

% Non-nuclear states under extended deterrence guarantees (NATO allies, US allies in Asia, etc.). They gain the security benefits of nuclear deterrence without the costs or risks of possession. Total war is unthinkable for them because their patrons' arsenals make it so. They can and do debate burden-sharing, but the strategic possibility space is set by others.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, nuclear_umbrella_states, beneficiary,
    organized, biographical, mobile, regional).

% General staffs, mobilization agencies, and total-war planning bureaucracies that existed before 1945. The space contraction reading predicts their atrophy: war-gaming for great-power conflict ceases, mobilization doctrine disappears, resources shift to sub-nuclear domains. They pay through institutional obsolescence and identity crisis — their professional self-concept was constituted by total war planning. Yet they also benefit: nuclear deterrence frees them from the impossible burden of preparing for wars that would destroy their societies.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, conventional_military_establishments, payer,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__space_contraction_reading, conventional_military_establishments, beneficiary).

% Agencies tasked with population protection against total war (fallout shelters, evacuation plans, continuity of government). The unthinkability of total war renders their core mission empty. They persist in vestigial form (FEMA, civil protection agencies) but their original justification has collapsed. They cannot exit — their statutory mandate exists — but the constraint has made their purpose incoherent. They are trapped in institutional zombiehood.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, civil_defense_agencies, payer,
    organized, biographical, trapped, national).

% Academic and think-tank analysts who study the constraint from outside. They document the shift from total war planning to sub-nuclear domains (limited war, counterinsurgency, hybrid warfare, gray zone). They see the full structure: the cognitive contraction, the institutional atrophy, the beneficiaries and the hollowed-out agencies. Their exit is analytical — they can change frameworks, but the empirical reality they study is fixed.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, strategic_studies_community, observer,
    analytical, civilizational, analytical, global).

% Activists, physicians, and civil society groups who argue nuclear weapons make war MORE thinkable (by accident, miscalculation, or escalation), not less. They are excluded from the strategic consensus that treats unthinkability as fact. They would object that the constraint is a dangerous illusion — that the possibility space has not contracted but has been obscured. Their voice is structurally absent from the deterrence framework's internal logic.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__space_contraction_reading, anti_nuclear_movement, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of great-power total war: by making the outcome mutually suicidal, nuclear weapons create a coordination equilibrium where no rational actor initiates total war. The unthinkability is the coordination mechanism — it aligns expectations without continuous negotiation.
% TRANSFER_FUNCTION: Transfers the burden of total war preparation (mobilization costs, civil defense, economic regimentation) FROM all states TO the nuclear great powers (who bear arsenal costs) and TO the atrophy of conventional military establishments (who lose their defining mission). The gain is negative: the avoidance of a catastrophe that would extract everything from everyone.
% ABSENT_VOICES: The anti-nuclear movement and Global South non-aligned states are structurally excluded. The movement argues the constraint is a snare — that unthinkability is an illusion maintained by luck and that the risk of accidental nuclear war makes total war MORE present, not less. Non-aligned states (especially during Cold War) argued the deterrence framework imposed a bipolar order that extracted their sovereignty — they were the battlefield on which the unthinkability was maintained. Neither voice penetrates the deterrence framework's internal logic.
% DISAPPEARANCE_RATIONALE: If the unthinkability constraint vanished overnight — if great powers could once again conceive of total war as a rational option — mobilization doctrines would return, civil defense would be rebuilt, strategic studies would pivot back to total war planning, and the entire post-1945 institutional order (UN, NPT, arms control, alliance structures) would face existential stress. The world rearranges because the constraint IS the architecture of the current strategic order.
% FOUNDING_PROBLEM: How to prevent great-power total war after 1945, given that industrial warfare had twice destroyed Europe and threatened global civilization, and that the UN collective security system had failed to prevent the Cold War bifurcation.
% FOUNDING_PROBLEM_CORROBORATION: The nuclear great powers attest the problem is live: great-power war remains the supreme danger and nuclear deterrence remains the only solution. The anti-nuclear movement and many non-aligned states attest the problem is dead or transformed: the Cold War ended, the bipolar order collapsed, and the remaining danger is the weapons themselves, not the war they deter. Independent historians of strategy (e.g., Freedman, Gaddis, Mueller) corroborate that the founding problem (preventing intentional great-power total war) has been substantially solved, but dispute whether the solution created new, worse problems.
narrative_ontology:disappearance_verdict(total_war_possibility_space__space_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__space_contraction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__space_contraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__space_contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__space_contraction_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.15) because the constraint's primary effect is preventive — it averts a loss that would be total — not extractive. Suppression is low (0.1) because the constraint operates through cognitive closure and institutional inertia, not active coercion; the anti-nuclear movement is excluded from the deterrence framework but not suppressed by it. Theater ratio is low (0.1) because the atrophy of total-war institutions is real functional obsolescence, not performative maintenance. Accessibility collapse is very high (0.92) because the reading's core claim is that total war has become literally unthinkable — not just discouraged — for the relevant strategic actors. Resistance is near zero (0.05) because no institutional actor seriously attempts to rebuild total war planning capacity. The measurement series shows a slight rise in extractiveness and suppression after 1985, reflecting the reading's prediction that as the Cold War ends, the constraint's maintenance requires more active management (arsenal modernization, extended deterrence reassurance, counter-proliferation) — but the reading still classifies these as coordination costs, not extraction.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (nuclear great powers) experiences the constraint as a Mountain they built and maintain — genuine coordination with negligible extraction. The payer seats (conventional military establishments, civil defense agencies) experience it as institutional atrophy — their defining mission collapsed without their consent. The beneficiary seats (nuclear armed/umbrella states) experience it as a Rope — they coordinate on non-war and collect the security dividend. The excluded seat (anti-nuclear movement) experiences it as a Snare — the unthinkability is a cover for ongoing existential risk. The engine computes these divergences from the structural data; the authored claim (mountain) does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear great powers are agenda_setters with analytical exit — they constructed the framework and can revise it (arms control, posture changes). Nuclear armed states and umbrella states are beneficiaries with constrained/mobile exit — they gain security without bearing full costs. Conventional military establishments are dual-role payers/beneficiaries with identity_locked exit — their professional identity was constituted by total war planning, so exit means professional dissolution. Civil defense agencies are trapped payers — statutory mandate persists but mission has evaporated. Strategic studies community is the analytical observer. Anti-nuclear movement is excluded — their structural position is that the constraint is a dangerous illusion, but they cannot access the deterrence framework's internal logic. The engine derives directionality from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing intentional great-power total war) is substantially solved — no great power has fought another directly since 1945. But the arrangement persists and deepens (arsenal modernization, new nuclear states, extended deterrence expansion) while the original bipolar context has vanished. The mandatrophy question: does the constraint now serve a live coordination function (preventing great-power war in a multipolar world) or has it become a piton — maintained by institutional inertia and identity lock of the nuclear priesthood? The reading predicts atrophy of total-war institutions but not of the nuclear deterrence apparatus itself. The dual-role of conventional military establishments (payer/beneficiary) captures the mandatrophy tension: they lose their old mission but gain a new, sustainable one (sub-nuclear operations). The civil defense agencies are the purest mandatrophy victims — their mandate is dead but the organization persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_impossibility,
    'Is the unthinkability of total war a genuine cognitive/physical limit (Mountain) or a constructed institutional constraint that benefits identifiable agents (False Summit)?',
    'Counterfactual analysis: if a great power genuinely believed total war was winnable (e.g., through missile defense breakthrough, decapitation capability, or limited nuclear war doctrine), would the constraint hold? Historical test: did any nuclear-armed state ever seriously plan for total war victory after acquiring secure second strike? The 1950s US ''New Look'' and 1980s SDI suggest the boundary is contested.',
    'If constructed, the constraint is a false summit mountain (reclassifies to tangled_rope via FSM). The beneficiaries (nuclear great powers) would be revealed as extracting security dividends while externalizing risk. If genuine, the Mountain classification stands and the beneficiaries are incidental to a structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_impossibility, conceptual, 'Whether total war unthinkability is a natural law of the nuclear age or a constructed cognitive boundary').

omega_variable(
    space_contraction_reading_of_total_war_kernel,
    'How does this reading''s structural claim (categorical impossibility) differ from sibling readings, and what classification follows from each?',
    'Compare the three readings'' ε values and stakeholder structures. Deterrence_equilibrium_reading: ε moderate (deterrence fails occasionally), active enforcement (arsenal maintenance), victims (populations held hostage). Nuclear_taboo_reading: ε low (normative), suppression normative not material, victims (those who would use nukes if taboo broke). Space_contraction_reading: ε near-zero (impossibility), no enforcement needed, no victims (only atrophied institutions). The classification divergence is the measurement.',
    'If the readings produce different constraint types from the same kernel, the kernel is a genuine site of structural ambiguity. The engine''s per-seat computation on each reading will reveal which actors experience which type under which reading. This is the committer-frame data the corpus exists to capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(space_contraction_reading_of_total_war_kernel, conceptual, 'Structural delta between this reading and its siblings in the total_war_possibility_space kernel').

omega_variable(
    identity_lock_mechanism_military_establishments,
    'What specific identity-fusion mechanism binds conventional military establishments to the atrophied total-war mission?',
    'Institutional ethnography: do general staffs self-identify as ''total war planners'' even when they no longer plan total war? Survey doctrinal publications, war college curricula, and promotion criteria for persistence of total-war conceptual vocabulary. Compare across nuclear and non-nuclear states.',
    'If identity_locked is professional identity (career path dependence), the constraint is a piton for that seat — the institution could change but the people cannot. If identity_locked is institutional identity (the organization has ''become'' its function), the constraint may be a scaffold that failed to sunset. The classification of the payer seat depends on this mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_military_establishments, empirical, 'Whether military establishment identity lock is professional (individual) or institutional (organizational)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__space_contraction_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twps_scr_tr_t1945, total_war_possibility_space__space_contraction_reading, theater_ratio, 1945, 0.02).
narrative_ontology:measurement(twps_scr_tr_t1955, total_war_possibility_space__space_contraction_reading, theater_ratio, 1955, 0.05).
narrative_ontology:measurement(twps_scr_tr_t1965, total_war_possibility_space__space_contraction_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(twps_scr_tr_t1975, total_war_possibility_space__space_contraction_reading, theater_ratio, 1975, 0.08).
narrative_ontology:measurement(twps_scr_tr_t1985, total_war_possibility_space__space_contraction_reading, theater_ratio, 1985, 0.09).
narrative_ontology:measurement(twps_scr_tr_t1995, total_war_possibility_space__space_contraction_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(twps_scr_tr_t2005, total_war_possibility_space__space_contraction_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(twps_scr_tr_t2015, total_war_possibility_space__space_contraction_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(twps_scr_tr_t2025, total_war_possibility_space__space_contraction_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(twps_scr_be_t1945, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(twps_scr_be_t1955, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1955, 0.08).
narrative_ontology:measurement(twps_scr_be_t1965, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1965, 0.1).
narrative_ontology:measurement(twps_scr_be_t1975, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1975, 0.12).
narrative_ontology:measurement(twps_scr_be_t1985, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1985, 0.14).
narrative_ontology:measurement(twps_scr_be_t1995, total_war_possibility_space__space_contraction_reading, base_extractiveness, 1995, 0.13).
narrative_ontology:measurement(twps_scr_be_t2005, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2005, 0.14).
narrative_ontology:measurement(twps_scr_be_t2015, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2015, 0.15).
narrative_ontology:measurement(twps_scr_be_t2025, total_war_possibility_space__space_contraction_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(twps_scr_su_t1945, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1945, 0.02).
narrative_ontology:measurement(twps_scr_su_t1955, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1955, 0.05).
narrative_ontology:measurement(twps_scr_su_t1965, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1965, 0.08).
narrative_ontology:measurement(twps_scr_su_t1975, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1975, 0.1).
narrative_ontology:measurement(twps_scr_su_t1985, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1985, 0.1).
narrative_ontology:measurement(twps_scr_su_t1995, total_war_possibility_space__space_contraction_reading, suppression_requirement, 1995, 0.08).
narrative_ontology:measurement(twps_scr_su_t2005, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2005, 0.08).
narrative_ontology:measurement(twps_scr_su_t2015, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2015, 0.09).
narrative_ontology:measurement(twps_scr_su_t2025, total_war_possibility_space__space_contraction_reading, suppression_requirement, 2025, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__space_contraction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__space_contraction_reading, 0.12).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, total_war_possibility_space__nuclear_taboo_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, nuclear_deterrence_posture).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, arms_control_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__space_contraction_reading, extended_deterrence_architecture).

% DUAL FORMULATION NOTE:
% This story is the space_contraction_reading of the total_war_possibility_space kernel. It claims ε ≈ 0.15 (coordination cost) and Mountain classification. The deterrence_equilibrium_reading claims ε ≈ 0.4 (deterrence failure risk) and Tangled Rope. The nuclear_taboo_reading claims ε ≈ 0.25 (norm maintenance cost) and Scaffold. The three readings share the kernel but instantiate different constraints with different metrics, stakeholders, and classifications. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_possibility_space__space_contraction_reading, institutional, 0.15).
constraint_indexing:directionality_override(total_war_possibility_space__space_contraction_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
