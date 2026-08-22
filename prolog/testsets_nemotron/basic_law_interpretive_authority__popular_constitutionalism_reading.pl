% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__popular_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__popular_constitutionalism_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: basic_law_interpretive_authority__popular_constitutionalism_reading
 *   human_readable: Popular Constitutionalism — Ongoing Democratic Contestation as Constitutional Authority
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the popular constitutionalism reading
 *   of the contested kernel 'basic_law_interpretive_authority.' Under this
 *   reading, constitutional meaning is not settled by any terminal
 *   institutional adjudicator — neither courts (judicial supremacy) nor
 *   legislature (parliamentary sovereignty) — but emerges from the ongoing,
 *   diffuse, and often conflictual contestation of the citizenry and their
 *   organized formations. The arrangement functions as a scaffold: it
 *   coordinates constitutional adaptation without formal amendment, but its
 *   justification is transitional — it presupposes a polity capable of
 *   sustaining productive contestation, and its legitimacy depends on that
 *   capacity not atrophying. The constraint extracts modestly (ε=0.18)
 *   primarily through the costs of constitutional uncertainty and gridlock
 *   distributed across institutional sites; it suppresses alternatives weakly
 *   (0.22) because the very structure invites contestation; theater ratio is
 *   moderate (0.35) because institutional actors perform constitutional
 *   fidelity while advancing partisan readings.
 *
 * KEY AGENTS:
 *   - citizenry_as_constituent_power: Primary beneficiary (organized/identity_locked) — holds ultimate interpretive authority but bears gridlock costs
 *   - social_movements: Beneficiary (moderate/constrained) — organized contestation agents
 *   - marginalized_communities: Beneficiary/payer (powerless/trapped) — gain from contestation, bear disproportionate backlash
 *   - judiciary: Agenda-setter/payer (institutional/analytical) — interprets without terminal authority
 *   - legislature: Agenda-setter/payer (institutional/analytical) — legislates without terminal authority
 *   - executive_branch: Payer (institutional/constrained) — implements under uncertainty
 *   - legal_scholars_and_public_intellectuals: Observer (analytical/analytical) — shapes interpretive ecology
 *   - future_generations: Excluded (powerless/trapped) — inherit without voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.18).
domain_priors:suppression_score(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.22).
domain_priors:theater_ratio(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__popular_constitutionalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__popular_constitutionalism_reading, scaffold).
narrative_ontology:human_readable(basic_law_interpretive_authority__popular_constitutionalism_reading, "Popular Constitutionalism — Ongoing Democratic Contestation as Constitutional Authority").
narrative_ontology:topic_domain(basic_law_interpretive_authority__popular_constitutionalism_reading, "constitutional_law/political_theory/institutional_design").

narrative_ontology:has_sunset_clause(basic_law_interpretive_authority__popular_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__popular_constitutionalism_reading, 'c8ca80d1-0a9a-4d66-985f-be3fab974896').
narrative_ontology:cs_kernel_codification('c8ca80d1-0a9a-4d66-985f-be3fab974896', formalized).
narrative_ontology:cs_authority_grounding('c8ca80d1-0a9a-4d66-985f-be3fab974896', distributed).
narrative_ontology:cs_reading_relation('c8ca80d1-0a9a-4d66-985f-be3fab974896', basic_law_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8ca80d1-0a9a-4d66-985f-be3fab974896', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('c8ca80d1-0a9a-4d66-985f-be3fab974896', foundational, constituent_power_resides_in_people).
narrative_ontology:cs_axiom_status(constituent_power_resides_in_people, holdable).
narrative_ontology:cs_axiom_grounding('c8ca80d1-0a9a-4d66-985f-be3fab974896', constituent_power_resides_in_people, deontological).
narrative_ontology:cs_axiom('c8ca80d1-0a9a-4d66-985f-be3fab974896', foundational, no_institution_has_terminal_interpretive_authority).
narrative_ontology:cs_axiom_status(no_institution_has_terminal_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('c8ca80d1-0a9a-4d66-985f-be3fab974896', no_institution_has_terminal_interpretive_authority, deontological).
narrative_ontology:cs_axiom('c8ca80d1-0a9a-4d66-985f-be3fab974896', secondary, constitutional_meaning_emerges_from_contestation).
narrative_ontology:cs_axiom_status(constitutional_meaning_emerges_from_contestation, holdable).
narrative_ontology:cs_axiom_grounding('c8ca80d1-0a9a-4d66-985f-be3fab974896', constitutional_meaning_emerges_from_contestation, conventional).
narrative_ontology:cs_reference_frame('c8ca80d1-0a9a-4d66-985f-be3fab974896', popular_sovereignty_as_ongoing_practice).
narrative_ontology:cs_drift_state('c8ca80d1-0a9a-4d66-985f-be3fab974896', contemporary_polarized_constitutionalism, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c8ca80d1-0a9a-4d66-985f-be3fab974896', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, citizenry_as_constituent_power).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, social_movements).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__popular_constitutionalism_reading, marginalized_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, marginalized_communities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__popular_constitutionalism_reading, executive_branch).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, constitutional_meaning_as_ongoing_practice).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, democratic_legitimacy_requires_contestation).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__popular_constitutionalism_reading, no_institution_has_terminal_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The people collectively constitute the ultimate source of constitutional meaning through ongoing political engagement, protest, voting, and public discourse. They bear the costs of gridlock and instability but retain the capacity to reshape constitutional understanding. Exit from this role is identity-locked — one cannot exit being a member of the polity without leaving the polity itself.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, citizenry_as_constituent_power, beneficiary,
    organized, generational, identity_locked, national).

% Organized collective actors (civil rights movement, labor movement, feminist movement, etc.) that contest constitutional meaning through extra-institutional pressure. They benefit from the openness of constitutional meaning but face repression, co-optation, and the exhaustion of sustained mobilization. Exit is constrained — disbanding surrenders the contestation space.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, social_movements, beneficiary,
    moderate, biographical, constrained, national).

% Groups historically excluded from formal constitutional interpretation (racial minorities, women, LGBTQ+ people, indigenous peoples, undocumented persons). They gain when contestation expands rights, but bear disproportionate costs of instability and backlash. Exit is trapped — they cannot opt out of the constitutional order that governs them.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, marginalized_communities, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, marginalized_communities, payer).

% Courts issue authoritative constitutional interpretations but lack terminal authority under this reading. They set agendas through doctrine but face pushback from other branches and the public. They pay institutional legitimacy costs when perceived as anti-democratic. Their role is analytical — they interpret but do not finally decide.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, judiciary, payer).

% Elected representatives enact constitutional understandings through legislation and constitutional amendments. They set agendas but face electoral accountability and judicial review. They pay political costs when constitutional contests stall governance. Their role is analytical — they respond to and shape contestation but do not finally decide.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature, agenda_setter,
    institutional, biographical, analytical, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__popular_constitutionalism_reading, legislature, payer).

% Implements constitutional interpretations under pressure from courts, Congress, and public opinion. Bears administrative costs of constitutional uncertainty and gridlock. Cannot exit the obligation to govern within contested constitutional space.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% Produce interpretive frameworks, historical narratives, and normative arguments that feed public contestation. They do not decide but shape the terms of debate. Exit is analytical — they can shift focus but remain part of the interpretive ecosystem.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, legal_scholars_and_public_intellectuals, observer,
    analytical, generational, analytical, global).

% Will inherit the constitutional order shaped by current contestation but have no voice in present contests. Their interests are represented only through present actors' claims about posterity. Exit is trapped — they cannot opt out of inheriting the polity.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__popular_constitutionalism_reading, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for constitutional meaning to adapt to changing social conditions without requiring formal amendment or terminal institutional decree — coordinates collective understanding of fundamental law through distributed, ongoing contestation rather than centralized settlement.
% TRANSFER_FUNCTION: Transfers interpretive authority from terminal adjudicators (courts or legislature) to the diffuse citizenry and their organized formations; moves the costs of constitutional uncertainty and gridlock from marginalized groups (who bear them under terminal adjudication) to institutional actors who must continuously justify their readings.
% ABSENT_VOICES: Future generations cannot participate in present contestation; non-citizen residents subject to constitutional authority lack formal voice; the dead (framers, past movements) are invoked but cannot speak for themselves; institutional actors (courts, legislatures) often claim to speak for the people while structurally filtering popular input.
% DISAPPEARANCE_RATIONALE: If popular constitutionalism vanished overnight — if constitutional meaning became fixed by terminal judicial or legislative decree — the adaptive capacity of the constitutional order would collapse; social movements would lose their primary leverage for rights expansion; marginalized communities would lose the contestation space that has historically produced their constitutional gains; the polity would lose its mechanism for legitimate constitutional change between formal amendments.
% FOUNDING_PROBLEM: The founding problem was how to constitute a legitimate fundamental law in a polity that rejected both monarchical sovereignty and pure legislative supremacy — how to make a constitution that could bind government while remaining accountable to the people who ordained it, without creating an unaccountable interpreter (judicial supremacy) or an omnipotent legislature (parliamentary sovereignty).
% FOUNDING_PROBLEM_CORROBORATION: The founding generation itself disagreed: Jefferson and Madison (in different moments) endorsed popular constitutionalism; Marshall and the Federalists built toward judicial supremacy; Anti-Federalists feared both. Contemporary corroboration: Ackerman (We the People), Kramer (The People Themselves), Tushnet (Taking the Constitution Away from the Courts) argue the founding problem remains live; Vermeule, Baude, Sachs argue it was resolved toward judicial supremacy or originalism. No consensus exists outside the benefiting parties (scholars and movements who favor popular constitutionalism).
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__popular_constitutionalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__popular_constitutionalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(basic_law_interpretive_authority__popular_constitutionalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).
:- end_tests(basic_law_interpretive_authority__popular_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is scaffold because this reading treats ongoing contestation as a transitional coordination mechanism — legitimate only while the polity sustains the capacity for productive democratic engagement. If contestation degrades into polarization, capture, or exhaustion, the scaffold's justification fails. Extractiveness (0.18) is low but non-zero: the constraint imposes real costs (gridlock, uncertainty, instability) distributed across institutions rather than concentrated on the powerless. Suppression (0.22) is low because the structure does not block exit from particular constitutional readings — it invites them. Theater ratio (0.35) reflects institutional performance of popular constitutionalism (citations to 'the people' in opinions, legislative hearings) that often masks partisan agendas. Accessibility collapse (0.42) is moderate: alternatives (judicial supremacy, parliamentary sovereignty) remain intellectually and institutionally available. Resistance (0.58) is significant: institutional actors resist ceding terminal authority.
 *
 * PERSPECTIVAL GAP:
 *   From the citizenry's seat, the constraint is rope-like — a genuine coordination mechanism for collective self-governance. From institutional seats (judiciary, legislature), it appears as a snare — a constraint that denies them the final authority they structurally crave. From marginalized communities' seat, it is a tangled rope — coordination that enables rights claims but extracts disproportionate mobilization costs. The engine computes these per-seat classifications from the structural data; the authored claim (scaffold) reflects the reading's own self-understanding as a transitional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizenry and social movements are structural beneficiaries (d ≈ 0.2) — the constraint subsidizes their interpretive agency. Marginalized communities are dual-positioned: beneficiaries of contestation's openness (d ≈ 0.3) but payers of its asymmetric costs (d ≈ 0.7 for backlash exposure). Institutional actors (judiciary, legislature, executive) are payers (d ≈ 0.6–0.7) — they bear legitimacy and governance costs without terminal authority. Future generations are trapped excluded (d ≈ 0.9) — they inherit the consequences without voice. The directionality derivation from beneficiary/victim declarations + exit options captures this gradient; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling this as pure coordination (rope) by recognizing its transitional justification — if the polity loses the capacity for productive contestation, the arrangement loses its legitimacy. It prevents mislabeling as extraction (snare) by recognizing that the primary beneficiaries are the citizenry broadly, not a narrow elite, and that the constraint's operation *is* the coordination function. The mandate (constitutional adaptability through popular engagement) remains live but contested — the founding problem (legitimate fundamental law without terminal adjudicator) persists, but whether this reading solves it is disputed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contestation_capacity_threshold,
    'At what threshold of polarization, institutional capture, or civic exhaustion does the scaffold''s transitional justification collapse — when does popular constitutionalism cease to coordinate and become a cover for dominance?',
    'Longitudinal study of constitutional regimes measuring: (a) correlation between contestation intensity and rights expansion vs. contraction, (b) institutional responsiveness to popular mobilization over time, (c) public trust in constitutional process across partisan lines.',
    'If a threshold exists and is crossed, the constraint reclassifies from scaffold to snare (extraction by dominant factions under cover of ''the people'') or piton (theatrical contestation masking institutional capture). The founding problem would be dead but the arrangement would persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contestation_capacity_threshold, empirical, 'Whether popular constitutionalism has a viability boundary beyond which it becomes extractive or theatrical.').

omega_variable(
    kernel_reading_foreclosure_boundary,
    'Does the popular constitutionalism reading logically foreclose judicial supremacy or parliamentary sovereignty within a single constitutional framework, or do they coexist as competing but non-exclusive positions?',
    'Conceptual analysis of whether a polity can simultaneously hold that (a) the people are the ultimate constitutive authority AND (b) courts/legislature have final say on specific constitutional questions — i.e., whether ''finality'' and ''ultimacy'' are compatible categories.',
    'If forecloses: the kernel admits no stable coexistence; regimes must choose one reading as constitutionally fundamental. If coexists_with: the kernel supports a pluralist constitutional order where multiple authority claims operate at different levels. If influences: popular constitutionalism creates pressure on terminal adjudicators without displacing them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_boundary, conceptual, 'Structural relationship between this reading and its siblings — foreclosure, coexistence, or influence.').

omega_variable(
    gridlock_cost_distribution,
    'Are the gridlock costs of distributed interpretive authority actually distributed across multiple institutional sites, or do they concentrate on the most vulnerable (as critics of popular constitutionalism argue)?',
    'Comparative analysis of constitutional crises under popular constitutionalist vs. judicial supremacy regimes: measure which actors bear the costs of delayed rights recognition, government shutdowns, policy paralysis, and democratic backsliding.',
    'If costs concentrate on the powerless, the constraint''s extractiveness is higher than measured and its beneficiary structure is misidentified — it would be a tangled rope or snare. If genuinely distributed, the scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gridlock_cost_distribution, empirical, 'Whether the constraint''s claimed cost distribution matches its actual operation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__popular_constitutionalism_reading, 1789, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pop_const_tr_t1789, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1789, 0.15).
narrative_ontology:measurement(pop_const_tr_t1865, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1865, 0.25).
narrative_ontology:measurement(pop_const_tr_t1937, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1937, 0.3).
narrative_ontology:measurement(pop_const_tr_t1954, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1954, 0.35).
narrative_ontology:measurement(pop_const_tr_t1973, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 1973, 0.42).
narrative_ontology:measurement(pop_const_tr_t2026, basic_law_interpretive_authority__popular_constitutionalism_reading, theater_ratio, 2026, 0.35).

% Extraction over time
narrative_ontology:measurement(pop_const_be_t1789, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1789, 0.08).
narrative_ontology:measurement(pop_const_be_t1865, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1865, 0.12).
narrative_ontology:measurement(pop_const_be_t1937, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1937, 0.15).
narrative_ontology:measurement(pop_const_be_t1954, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1954, 0.18).
narrative_ontology:measurement(pop_const_be_t1973, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 1973, 0.22).
narrative_ontology:measurement(pop_const_be_t2026, basic_law_interpretive_authority__popular_constitutionalism_reading, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(pop_const_su_t1789, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1789, 0.1).
narrative_ontology:measurement(pop_const_su_t1865, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1865, 0.18).
narrative_ontology:measurement(pop_const_su_t1937, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1937, 0.25).
narrative_ontology:measurement(pop_const_su_t1954, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(pop_const_su_t1973, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 1973, 0.3).
narrative_ontology:measurement(pop_const_su_t2026, basic_law_interpretive_authority__popular_constitutionalism_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__popular_constitutionalism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__popular_constitutionalism_reading, 0.08).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__popular_constitutionalism_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the basic_law_interpretive_authority constraint family (kernel). The three readings share the same referent (the question of who holds final constitutional interpretive authority) but instantiate different constraints with different ε values, different beneficiary/victim structures, and different types. Judicial supremacy reading: higher extractiveness (court-centered), lower suppression (institutional closure), claimed as rope/tangled_rope. Parliamentary sovereignty reading: moderate extractiveness (legislature-centered), moderate suppression (majoritarian closure), claimed as rope. Popular constitutionalism reading (this): lowest extractiveness (diffuse), lowest suppression (openness), claimed as scaffold. The epsilon-invariance principle requires separate stories because the structural relationships differ fundamentally — not merely the measurement basis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
