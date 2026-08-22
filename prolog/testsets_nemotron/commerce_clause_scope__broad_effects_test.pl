% ============================================================================
% CONSTRAINT STORY: commerce_clause_scope__broad_effects_test
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_scope__broad_effects_test, []).

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
 *   constraint_id: commerce_clause_scope__broad_effects_test
 *   human_readable: Commerce Clause — Substantial Effects / Aggregation Doctrine (Broad Reading)
 *   domain: constitutional/federalism
 *
 * SUMMARY:
 *   The broad-effects reading of the Commerce Clause holds that Congress may
 *   regulate any economic activity that, in the aggregate, substantially
 *   affects interstate commerce — including purely intrastate, non-commercial
 *   activity (Wickard v. Filburn, Gonzales v. Raich). This reading has been
 *   the operational foundation of the modern federal regulatory state since
 *   1937. It is claimed as a tangled_rope: it solves genuine coordination
 *   problems (race-to-the-bottom, externalities, national markets) but
 *   extracts jurisdictional authority from states and local autonomy through
 *   the same structure. The constraint is actively enforced by courts
 *   upholding federal statutes and striking down state laws under dormant
 *   commerce or preemption doctrines.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, 0.72).
domain_priors:suppression_score(commerce_clause_scope__broad_effects_test, 0.68).
domain_priors:theater_ratio(commerce_clause_scope__broad_effects_test, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, extractiveness, 0.72).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(commerce_clause_scope__broad_effects_test, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_scope__broad_effects_test, tangled_rope).
narrative_ontology:human_readable(commerce_clause_scope__broad_effects_test, "Commerce Clause — Substantial Effects / Aggregation Doctrine (Broad Reading)").
narrative_ontology:topic_domain(commerce_clause_scope__broad_effects_test, "constitutional/federalism").

domain_priors:requires_active_enforcement(commerce_clause_scope__broad_effects_test).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_scope__broad_effects_test, 'd634c481-25cd-4166-a86e-7faa0bf93179').
narrative_ontology:cs_kernel_codification('d634c481-25cd-4166-a86e-7faa0bf93179', fixed_text).
narrative_ontology:cs_authority_grounding('d634c481-25cd-4166-a86e-7faa0bf93179', lineage).
narrative_ontology:cs_interpretation_layer_present('d634c481-25cd-4166-a86e-7faa0bf93179').
narrative_ontology:cs_reading_relation('d634c481-25cd-4166-a86e-7faa0bf93179', commerce_clause_scope__narrow_originalist, forecloses).
narrative_ontology:cs_reading_relation('d634c481-25cd-4166-a86e-7faa0bf93179', commerce_clause_scope__intermediate_channels, influences).
narrative_ontology:cs_axiom('d634c481-25cd-4166-a86e-7faa0bf93179', foundational, aggregation_principle_is_constitutive).
narrative_ontology:cs_axiom_status(aggregation_principle_is_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('d634c481-25cd-4166-a86e-7faa0bf93179', aggregation_principle_is_constitutive, conventional).
narrative_ontology:cs_axiom('d634c481-25cd-4166-a86e-7faa0bf93179', foundational, national_economic_unity_justifies_plenary_reach).
narrative_ontology:cs_axiom_status(national_economic_unity_justifies_plenary_reach, holdable).
narrative_ontology:cs_axiom_grounding('d634c481-25cd-4166-a86e-7faa0bf93179', national_economic_unity_justifies_plenary_reach, instrumental).
narrative_ontology:cs_reference_frame('d634c481-25cd-4166-a86e-7faa0bf93179', new_deal_settlement).
narrative_ontology:cs_drift_state('d634c481-25cd-4166-a86e-7faa0bf93179', contemporary_federalism_revival, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d634c481-25cd-4166-a86e-7faa0bf93179', '').
narrative_ontology:cs_kernel_id(commerce_clause_scope__broad_effects_test, commerce_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, federal_regulators).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, national_interest_groups).
narrative_ontology:constraint_beneficiary(commerce_clause_scope__broad_effects_test, civil_rights_enforcement).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, state_experimentation).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, local_economic_autonomy).
narrative_ontology:constraint_victim(commerce_clause_scope__broad_effects_test, federalism_as_constraint).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, aggregation_principle).
narrative_ontology:constraint_vindicates(commerce_clause_scope__broad_effects_test, national_economic_unity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce regulatory programs that reach intrastate activity by asserting aggregate effects on interstate commerce. They gain expansive jurisdictional authority, career stability through permanent enforcement infrastructure, and the ability to set national policy without state consent. Exit from this role means accepting narrower jurisdiction — structurally disfavored by institutional incentives.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federal_regulators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_scope__broad_effects_test, federal_regulators, beneficiary).

% Lobby for federal preemption of state variation to achieve uniform national standards (environmental, labor, consumer protection, etc.). They benefit from a single compliance regime and avoid the cost of fifty-state campaigns. Their exit is shifting to state-level advocacy if federal scope narrows — feasible but costlier.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, national_interest_groups, beneficiary,
    organized, biographical, mobile, national).

% Relies on commerce power to reach private discrimination (Heart of Atlanta, Katzenbach) where Fourteenth Amendment state-action doctrine falls short. Gains enforcement reach into local economic actors. Exit would mean depending on state enforcement or narrower constitutional hooks — a significant capability loss.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, civil_rights_enforcement, beneficiary,
    institutional, generational, constrained, national).

% States lose ability to serve as laboratories of democracy when federal regulation occupies the field via aggregation doctrine. They bear compliance costs, preemption of local innovation, and political accountability without policy control. Exit means constitutional challenge or interstate compacts — legally uncertain and politically difficult.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, state_experimentation, payer,
    organized, biographical, constrained, regional).

% Small businesses, municipalities, and local economic actors subject to federal rules justified by attenuated aggregate effects. They bear compliance costs designed for national markets, with no meaningful voice in federal rulemaking. Exit is geographic relocation or cessation of activity — costly and often impossible.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, local_economic_autonomy, payer,
    moderate, biographical, constrained, local).

% The structural principle that state sovereignty limits federal power. Under broad aggregation, this constraint is eroded — federal authority expands until it encounters only political, not jurisdictional, limits. It cannot 'exit' the constitutional order; its degradation is the victim's experience of the constraint.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, federalism_as_constraint, payer,
    institutional, generational, trapped, national).

% Advocates for narrow_originalist or intermediate_channels readings. They argue the broad reading exceeds textual and historical limits. They are excluded from the operational consensus that treats Wickard/Raich as settled law; their objections are treated as dissent, not a live contest over the constraint's scope.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, constitutional_originalists, excluded,
    organized, generational, identity_locked, national).

% Analyze the doctrine's evolution, empirical effects, and theoretical coherence. They do not bear costs or collect benefits from the constraint's operation; they map the structure for the profession and the courts.
narrative_ontology:constraint_stakeholder(commerce_clause_scope__broad_effects_test, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single national regulatory floor for economic problems that spill across state lines — pollution, labor standards, financial stability, civil rights in commercial settings — avoiding a race to the bottom and collective-action failures among states.
% TRANSFER_FUNCTION: Moves regulatory authority from states to the federal government across virtually all economic activity. The transfer is jurisdictional: states lose the power to set their own rules; the federal government gains plenary reach; compliance costs shift from fifty regimes to one (but that one is often more demanding).
% ABSENT_VOICES: State governments as sovereign co-equals (not merely as regulated parties), local communities facing federal mandates without representation in the rulemaking, and the narrow_originalist/intermediate_channels reading holders who are treated as having lost the argument rather than holding a live institutional position.
% DISAPPEARANCE_RATIONALE: If the substantial-effects/aggregation doctrine vanished overnight, the federal regulatory state would lose its jurisdictional foundation for most domestic programs (environment, labor, health, civil rights in commerce). States would reclaim primary authority; the national regulatory floor would disappear; interstate competition and coordination problems would resurface. The legal, economic, and political landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The Articles of Confederation failed because states could free-ride on national public goods and impose externalities on each other. The Commerce Clause was meant to empower Congress to solve collective-action problems among states — but the founding generation did not anticipate the aggregation doctrine that turns every intrastate activity into a federal hook.
% FOUNDING_PROBLEM_CORROBORATION: Federalist defenders (Hamilton, Madison in Federalist 42) attest the Clause was aimed at state-imposed barriers to trade, not plenary economic regulation. The New Deal Court (Wickard) and Warren Court (Heart of Atlanta) attest the aggregation principle is necessary for modern governance. No disinterested historical consensus exists — the founding problem is claimed by both sides.
narrative_ontology:disappearance_verdict(commerce_clause_scope__broad_effects_test, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_scope__broad_effects_test, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_scope__broad_effects_test, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(commerce_clause_scope__broad_effects_test, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_scope__broad_effects_test, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_scope__broad_effects_test_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(commerce_clause_scope__broad_effects_test, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(commerce_clause_scope__broad_effects_test_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the aggregation doctrine effectively removes jurisdictional limits — virtually all human economic activity falls within federal reach. Suppression (0.68) is substantial because states and local actors cannot exit the federal regime; preemption and supremacy make resistance costly. Theater (0.22) is low-moderate: the coordination function (national solutions to spillovers) is real, but a growing share of federal regulation addresses problems with weak or attenuated interstate effects, suggesting mission creep. The temporal series shows extraction rising from the New Deal through the Great Society, a slight dip during the Rehnquist federalism revival (Lopez, Morrison), then resuming upward through Raich and the ACA litigation era.
 *
 * PERSPECTIVAL GAP:
 *   From the federal regulator seat, the constraint is a rope (genuine coordination, net benefit). From state_experimentation and local_economic_autonomy seats, it is a snare (extraction without consent, no exit). From federalism_as_constraint, it is a mountain being eroded — a structural limit that has become permeable. The engine computes these per-seat types from the structural data; the claimed_type (tangled_rope) reflects the system-level hybrid character.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal regulators are the structural beneficiaries (d ~ 0.1): they gain authority, resources, and permanence. National interest groups and civil rights enforcement are beneficiaries with mobile/constrained exit (d ~ 0.2-0.35). State experimentation, local autonomy, and federalism-as-constraint are payers with constrained/trapped exit (d ~ 0.7-0.95). Constitutional originalists are excluded but identity-locked — their professional identity is constituted by opposition to this reading. Legal scholars are analytical (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interstate collective action) is live but has been stretched beyond recognition. The aggregation doctrine now reaches activity the founding generation would not have recognized as commerce. The constraint persists because the beneficiaries (federal state, national interest groups, civil rights infrastructure) are powerful and organized, while the victims (states, localities, structural federalism) are fragmented, trapped, or abstract. Mandatrophy is unresolved: the coordination function survives but the extraction has become the dominant feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregation_doctrine_boundary,
    'Is there a principled stopping point to the aggregation doctrine, or does it logically extend to all human activity?',
    'A future Supreme Court decision identifying a category of activity that cannot be aggregated (e.g., non-economic, traditionally local, or de minimis), or a constitutional amendment.',
    'If no stopping point exists, the constraint is a snare from the state-autonomy seat — extraction without structural limit. If a stopping point exists, the constraint remains a tangled_rope with a genuine coordination core and a bounded extraction fringe.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregation_doctrine_boundary, conceptual, 'Whether the aggregation principle has an internal limit or is logically unbounded.').

omega_variable(
    coordination_necessity_of_plenary_reach,
    'Are the coordination problems the broad reading solves (externalities, race-to-bottom, national markets) actually solvable only through plenary federal authority, or could interstate compacts, model acts, or conditional preemption achieve similar outcomes?',
    'Empirical study of policy diffusion and interstate cooperation in domains where federal regulation is absent or minimal (e.g., family law, land use, some professional licensing).',
    'If coordination is achievable without plenary aggregation, the extraction is unnecessary — the constraint is a snare wearing a rope''s clothes. If plenary authority is necessary, the extraction is the price of coordination — a genuine tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_of_plenary_reach, empirical, 'Whether the coordination function requires the broad reading''s extraction or if less extractive alternatives exist.').

omega_variable(
    committer_structure_kernel_reading,
    'This constraint is one reading (broad_effects_test) of the contested kernel commerce_clause_scope. How does the existence of sibling readings (narrow_originalist, intermediate_channels) affect the structural classification of this reading?',
    'The engine computes per-seat types for each reading independently. The kernel structure is documented here; the classification divergence across readings is the measurement target.',
    'If the narrow_originalist reading computes as mountain (low extraction, high naturalness) while broad_effects_test computes as tangled_rope/snare, the kernel itself is a false summit — the ''Commerce Clause'' label conflates a natural limit with an extractive doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committer-frame omega: this reading''s structural relationship to its kernel and siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_scope__broad_effects_test, 1937, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_scope__broad_effects_test, theater_ratio, 1937, 0.1).
narrative_ontology:measurement(comm_tr_t1942, commerce_clause_scope__broad_effects_test, theater_ratio, 1942, 0.12).
narrative_ontology:measurement(comm_tr_t1964, commerce_clause_scope__broad_effects_test, theater_ratio, 1964, 0.15).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_scope__broad_effects_test, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(comm_tr_t2005, commerce_clause_scope__broad_effects_test, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_scope__broad_effects_test, theater_ratio, 2012, 0.21).
narrative_ontology:measurement(comm_tr_t2024, commerce_clause_scope__broad_effects_test, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(comm_be_t1937, commerce_clause_scope__broad_effects_test, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(comm_be_t1942, commerce_clause_scope__broad_effects_test, base_extractiveness, 1942, 0.55).
narrative_ontology:measurement(comm_be_t1964, commerce_clause_scope__broad_effects_test, base_extractiveness, 1964, 0.62).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_scope__broad_effects_test, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(comm_be_t2005, commerce_clause_scope__broad_effects_test, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_scope__broad_effects_test, base_extractiveness, 2012, 0.65).
narrative_ontology:measurement(comm_be_t2024, commerce_clause_scope__broad_effects_test, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1937, commerce_clause_scope__broad_effects_test, suppression_requirement, 1937, 0.4).
narrative_ontology:measurement(comm_su_t1942, commerce_clause_scope__broad_effects_test, suppression_requirement, 1942, 0.55).
narrative_ontology:measurement(comm_su_t1964, commerce_clause_scope__broad_effects_test, suppression_requirement, 1964, 0.6).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_scope__broad_effects_test, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(comm_su_t2005, commerce_clause_scope__broad_effects_test, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_scope__broad_effects_test, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(comm_su_t2024, commerce_clause_scope__broad_effects_test, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_scope__broad_effects_test, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_scope__broad_effects_test, 0.12).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__intermediate_channels).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, commerce_clause_scope__narrow_originalist).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, dormant_commerce_clause).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, federal_preemption_doctrine).
narrative_ontology:affects_constraint(commerce_clause_scope__broad_effects_test, necessary_and_proper_clause_scope).

% DUAL FORMULATION NOTE:
% The commerce_clause_scope kernel decomposes into three constraint stories: broad_effects_test (this file, tangled_rope), intermediate_channels (rope/tangled_rope boundary), and narrow_originalist (mountain/rope boundary). Each has distinct ε, beneficiaries, victims, and claimed_type. They are linked via affects_constraints. The broad reading structurally depends on the necessary_and_proper_clause for its enforcement reach.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commerce_clause_scope__broad_effects_test, institutional, 0.15).
constraint_indexing:directionality_override(commerce_clause_scope__broad_effects_test, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
