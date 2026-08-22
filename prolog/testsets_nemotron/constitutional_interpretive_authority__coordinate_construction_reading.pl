% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Coordinate Construction of Constitutional Meaning
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This constraint story captures the coordinate construction reading of
 *   constitutional interpretive authority — the view that no single branch
 *   possesses final interpretive authority, but rather constitutional meaning
 *   emerges through ongoing inter-branch dialogue and political contestation.
 *   This is one of three structurally distinct readings of the contested
 *   kernel 'constitutional_interpretive_authority.' The coordinate
 *   construction reading treats the absence of a final arbiter as a
 *   coordination mechanism: it forces branches to negotiate, accommodates
 *   constitutional change without formal amendment, and distributes the
 *   burden of constitutional maintenance across democratic institutions. But
 *   it also extracts from those who depend on stable constitutional
 *   protections — minority rights claimants, marginalized communities, and
 *   unpopular speakers — whose entitlements become bargaining chips in
 *   inter-branch negotiation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.28).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.15).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Coordinate Construction of Constitutional Meaning").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "political/constitutional").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, '3f3e1235-c7c6-4d9c-81af-f8f2c8db4862').
narrative_ontology:cs_kernel_codification('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', fixed_text).
narrative_ontology:cs_authority_grounding('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', lineage).
narrative_ontology:cs_interpretation_layer_present('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862').
narrative_ontology:cs_reading_relation('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', constitutional_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', foundational, no_single_final_arbitrator).
narrative_ontology:cs_axiom_status(no_single_final_arbitrator, holdable).
narrative_ontology:cs_axiom_grounding('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', no_single_final_arbitrator, conventional).
narrative_ontology:cs_axiom('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', foundational, constitutional_meaning_constructed_through_dialogue).
narrative_ontology:cs_axiom_status(constitutional_meaning_constructed_through_dialogue, holdable).
narrative_ontology:cs_axiom_grounding('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', constitutional_meaning_constructed_through_dialogue, conventional).
narrative_ontology:cs_axiom('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', secondary, political_branches_legitimately_participate_in_interpretation).
narrative_ontology:cs_axiom_status(political_branches_legitimately_participate_in_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', political_branches_legitimately_participate_in_interpretation, conventional).
narrative_ontology:cs_reference_frame('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', founding_era_ambiguous_authority).
narrative_ontology:cs_drift_state('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', contemporary_polarized_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3f3e1235-c7c6-4d9c-81af-f8f2c8db4862', '2026-07-25T14:30:00Z').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, political_parties).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_claimants).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, marginalized_communities).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, unpopular_speech_actors).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_dialogue_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts constitutional amendments and ordinary legislation; controls judicial appointments and budget; exercises oversight through hearings and confirmation processes. Benefits from interpretive latitude in ambiguous constitutional provisions and the ability to shape constitutional meaning through ordinary legislation when judicial review is deferred.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, legislative_branch, beneficiary).

% Implements constitutional provisions through enforcement discretion; appoints judges; issues executive orders interpreting constitutional authority; controls administrative state. Benefits from flexible constitutional construction during emergencies and policy innovation periods where judicial deference is highest.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, executive_branch, beneficiary).

% Provides authoritative but revisable interpretations through judicial review; resolves concrete disputes; develops doctrinal frameworks. Benefits from institutional prestige and the power to set the terms of constitutional debate, but cannot finally settle meaning without inter-branch acquiescence.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, judicial_branch, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, judicial_branch, beneficiary).

% Mobilize constitutional interpretation as electoral strategy; coordinate appointments and legislative agendas; use constitutional rhetoric to energize bases. Benefit from the contestability of meaning — interpretive stability would reduce constitutional politics as a mobilization resource.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, political_parties, beneficiary,
    organized, biographical, mobile, national).

% Depend on stable constitutional protections against majority will; bear the cost of interpretive instability when rights are subject to political contestation rather than judicial entrenchment. Exit is blocked by the very structure — they cannot leave the constitutional order that fails to protect them.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, minority_rights_claimants, payer,
    moderate, biographical, trapped, national).

% Experience constitutional meaning as it is constructed by dominant political coalitions; bear disproportionate costs when political branches negotiate away protections in inter-branch bargains. Structurally excluded from the dialogue that constructs their constitutional status.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, marginalized_communities, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__coordinate_construction_reading, marginalized_communities, excluded).

% Face shifting constitutional protection depending on political winds; when no branch has final authority, speech protections become bargaining chips in inter-branch negotiation. Cannot exit the constitutional regime but can sometimes find temporary refuge in judicial decisions.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, unpopular_speech_actors, payer,
    powerless, immediate, constrained, national).

% Analyze and theorize the dynamics of coordinate construction; provide intellectual frameworks that branches selectively adopt; document the pattern of interpretive settlement and unsettlement. Do not bear extraction costs nor collect coordination benefits directly.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of constitutional governance without a final arbiter — enables a constitutional order to function despite persistent disagreement about fundamental meaning by distributing interpretive authority across branches that must negotiate, accommodate, and sometimes confront each other.
% TRANSFER_FUNCTION: Moves interpretive authority and constitutional entitlements from courts (which would hold them under judicial supremacy) to the political branches and the electoral coalitions that control them; moves the costs of interpretive instability onto rights-holders who lack political power to secure stable protections.
% ABSENT_VOICES: Future generations who will inherit the constitutional settlements made today; non-citizens subject to constitutional authority (detainees, territorial populations); the structurally silenced whose interests are never represented in inter-branch dialogue because they lack organizational capacity.
% DISAPPEARANCE_RATIONALE: If coordinate construction vanished and one branch gained final interpretive authority (judicial supremacy, parliamentary supremacy, or executive supremacy), the constitutional order would fundamentally reorganize — the distribution of power, the stability of rights, and the nature of constitutional politics would all shift.
% FOUNDING_PROBLEM: How to constitute a government of limited powers that can adapt to unforeseen circumstances without collapsing into either judicial tyranny (unelected judges imposing policy preferences) or legislative tyranny (majorities overriding minority rights) — the founding generation's solution was to make interpretation a shared, ongoing project rather than a settled delegation.
% FOUNDING_PROBLEM_CORROBORATION: The founding generation's own writings (Federalist Papers, ratification debates) attest to deliberate ambiguity about final interpretive authority. Modern political scientists (Whittington, Kramer, Tushnet) corroborate from outside the benefiting branches that coordinate construction was a pragmatic compromise, not a principled commitment — though contemporary scholars (Balkin, Levinson) contest whether it remains viable under polarized conditions.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__coordinate_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__coordinate_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(constitutional_interpretive_authority__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__coordinate_construction_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).
:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.28) reflects that the coordinate construction arrangement does transfer interpretive power and its benefits to political branches while imposing instability costs on rights-holders — but the transfer is moderate because courts still exercise significant (if revisable) review. Suppression (0.15) is low because alternatives (judicial supremacy, parliamentary supremacy) remain live in public discourse and have been partially instantiated at different historical moments — the constraint does not actively suppress rival interpretations. Theater ratio (0.35) has risen over time as coordinate construction has become more performative: branches increasingly invoke 'dialogue' while entrenching partisan positions, and the coordination function degrades into ritualized confrontation. Accessibility collapse (0.45) is moderate — the coordinate construction framework is contestable and alternatives are imaginable, but the structural incentives of the constitutional system make exit from this interpretive regime difficult. Resistance (0.55) is significant — the arrangement faces persistent challenge from judicial supremacy advocates, originalists, and rights-protective movements.
 *
 * PERSPECTIVAL GAP:
 *   From the legislative/executive seats, coordinate construction appears as genuine coordination — it solves the problem of governing without judicial veto. From the judicial seat, it appears as a constrained but legitimate role — the Court speaks authoritatively but revisably. From the minority rights seat, it appears as extraction — their protections are hostage to political bargaining. The engine computes these divergent classifications from the structural data; the claim (tangled_rope) captures the hybrid coordination/extraction structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The three branches are co-agenda-setters and co-beneficiaries: each gains interpretive latitude and political leverage from the absence of a final arbiter. Political parties are pure beneficiaries — they mobilize constitutional ambiguity as electoral resource. Minority rights claimants and marginalized communities are payers: they bear the costs of interpretive instability without the political power to stabilize their protections. Unpopular speech actors are payers with slightly better exit (judicial decisions sometimes provide temporary refuge). Constitutional scholars are analytical observers. The derivation chain correctly places institutional actors at the beneficiary end (d ≈ 0.2-0.3) and rights-holders at the target end (d ≈ 0.7-0.85), with scholars at the analytical end (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (balancing governance against tyranny) remains live but the coordinate construction solution shows mandatrophy symptoms: the arrangement was built for a political culture of inter-branch negotiation and compromise that has degraded into partisan warfare. Theater ratio rising from 0.1 to 0.35 tracks this degradation. The coordination function (forcing negotiation) persists but the extraction function (instability costs on the powerless) has grown. This is not a snare — the coordination function is real and branches genuinely depend on it — but it is a tangled rope where the coordination benefit accrues to powerful institutions and the extraction cost falls on the politically weak.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine instantiation of the coordinate construction reading, or does it conflate coordinate construction with weak judicial review?',
    'Compare the structural features of this constraint against the definitional commitments of coordinate construction theory (Whittington, Tushnet, Balkin) — specifically whether inter-branch dialogue is structurally required or merely permitted.',
    'If conflation is confirmed, the constraint_id should be decomposed: one story for coordinate construction proper, one for weak judicial review as a distinct arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Whether coordinate construction is a distinct structural type or a variant of judicial deference.').

omega_variable(
    extraction_measurement_referent,
    'Does the extractiveness metric (ε=0.28) correctly capture the standing arrangement under contest as this reading sees it, or does it inadvertently measure the reading''s preferred alternative?',
    'Audit the extraction assessment against the ε-invariance principle: the referent must be the actual coordinate construction regime (with its instability costs on rights-holders), not the stable protections the reading''s proponents might prefer.',
    'If the referent is misidentified, ε is invalid and the classification is ungrounded — the story must be re-authored with the correct referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_measurement_referent, conceptual, 'Whether ε correctly refers to the standing arrangement (coordinate construction as practiced) rather than the idealized version.').

omega_variable(
    tangled_rope_boundary,
    'Is the coordinate construction arrangement genuinely a tangled rope (coordination + extraction), or does the coordination function collapse under polarized conditions leaving only extraction?',
    'Track whether inter-branch constitutional dialogue produces stable settlements or merely ritualized conflict — measure the rate of durable interpretive accommodations versus performative confrontations over time.',
    'If coordination function has collapsed, the constraint reclassifies toward snare; if coordination persists despite polarization, tangled_rope stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tangled_rope_boundary, empirical, 'Whether the coordination component of coordinate construction survives contemporary polarization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_tr_t1789, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_tr_t1865, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1865, 0.15).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_tr_t1937, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1937, 0.2).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_tr_t1954, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1954, 0.22).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_tr_t1973, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 1973, 0.28).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_tr_t2000, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_tr_t2024, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_be_t1789, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1789, 0.15).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_be_t1865, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1865, 0.22).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_be_t1937, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1937, 0.25).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_be_t1954, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1954, 0.23).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_be_t1973, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 1973, 0.27).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_be_t2000, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 2000, 0.26).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_be_t2024, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 2024, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_su_t1789, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1789, 0.05).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_su_t1865, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1865, 0.12).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_su_t1937, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1937, 0.1).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_su_t1954, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1954, 0.08).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_su_t1973, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 1973, 0.1).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_su_t2000, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(constitutional_interpretive_authority__coordinate_construction_reading_su_t2024, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_interpretive_authority__coordinate_construction_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, judicial_review_scope).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_amendment_difficulty).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, federalism_structure).

% DUAL FORMULATION NOTE:
% This constraint is one member of the constitutional_interpretive_authority kernel family. The three readings (coordinate_construction, judicial_supremacy, parliamentary_supremacy) are structurally distinct constraints with different ε values, different beneficiary/victim structures, and different type classifications. They are linked by network.affects_constraints because each reading's institutional dynamics create structural pressure on the others — coordinate construction's tolerance for instability creates legitimacy pressure on judicial supremacy; judicial supremacy's entrenchment creates reactive pressure toward parliamentary supremacy; parliamentary supremacy's majoritarianism creates rights-protection pressure toward coordinate construction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_interpretive_authority__coordinate_construction_reading, institutional, 0.25).
constraint_indexing:directionality_override(constitutional_interpretive_authority__coordinate_construction_reading, organized, 0.15).
constraint_indexing:directionality_override(constitutional_interpretive_authority__coordinate_construction_reading, moderate, 0.75).
constraint_indexing:directionality_override(constitutional_interpretive_authority__coordinate_construction_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
