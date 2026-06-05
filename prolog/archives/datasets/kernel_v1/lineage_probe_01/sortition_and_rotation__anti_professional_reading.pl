% ============================================================================
% CONSTRAINT STORY: sortition_and_rotation__anti_professional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sortition_and_rotation__anti_professional_reading, []).

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
 *   constraint_id: sortition_and_rotation__anti_professional_reading
 *   human_readable: Sortition as Anti-Professionalism: Amateur Government by Lot
 *   domain: legal/doctrinal/political_theory
 *
 * SUMMARY:
 *   Sortition — selection of office by lot — operationalizes
 *   anti-professionalism as a structural constraint against the formation of
 *   a political class. This reading emphasizes the suppression of accumulated
 *   skill, expertise, and careerism as the intentional mechanism preventing
 *   oligarchy. Where the equal_chance_reading focuses on citizen equality
 *   (every person has identical probability of office), this reading focuses
 *   on the elimination of professional politics (no person can build a career
 *   from office, no expertise can be hoarded). The constraint works by making
 *   office brief (one year), non-renewable or rarely renewable, subject to
 *   audit and recall, and filled by lottery rather than election. These
 *   structural features combine to prevent the formation of a permanent
 *   political class: even if an officeholder is talented, they cannot
 *   consolidate power because their term ends; even if they seek reelection,
 *   they have no institutional advantage (unlike elected officials with name
 *   recognition and donor networks); even if they could build a network, the
 *   lot might not draw them again. The constraint's beneficiary is the
 *   citizen body as equals (amateur government guarantees no subset can
 *   accumulate durable power); the victim set is political skill itself
 *   (expertise cannot be developed, transmitted, or deployed for career
 *   advancement). This reading treats the suppression of skill as a
 *   structural feature, not a bug — the point is to make office incompatible
 *   with professionalism.
 *
 * KEY AGENTS:
 *   - Citizen Body (Equals): Primary beneficiary (powerful/mobile) — sortition ensures no political class can form; all citizens have equal and brief access to power
 *   - Accumulated Political Skill: Victim (powerless/trapped) — expertise in governance cannot be deployed or rewarded; skilled administrators, generals, treasurers are suppressed in their capacity to use skill professionally
 *   - Polis Institutional Structure: Secondary beneficiary (institutional/arbitrage) — oligarchy prevention ensures institutional survival and legitimacy
 *   - Aristocratic Remnant: Organized agent (organized/constrained) — experiences sortition as suppression of their natural leadership but accepts strategic exceptions (generals, treasurers) where cardinally necessary
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees sortition as pure coordination mechanism preventing power concentration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sortition_and_rotation__anti_professional_reading, 0.22).
domain_priors:suppression_score(sortition_and_rotation__anti_professional_reading, 0.68).
domain_priors:theater_ratio(sortition_and_rotation__anti_professional_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sortition_and_rotation__anti_professional_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(sortition_and_rotation__anti_professional_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sortition_and_rotation__anti_professional_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sortition_and_rotation__anti_professional_reading, rope).
narrative_ontology:human_readable(sortition_and_rotation__anti_professional_reading, "Sortition as Anti-Professionalism: Amateur Government by Lot").
narrative_ontology:topic_domain(sortition_and_rotation__anti_professional_reading, "legal/doctrinal/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sortition_and_rotation__anti_professional_reading, '72634d43-6cc8-4588-869b-1a33fe19b684').
narrative_ontology:cs_kernel_codification('72634d43-6cc8-4588-869b-1a33fe19b684', formalized).
narrative_ontology:cs_authority_grounding('72634d43-6cc8-4588-869b-1a33fe19b684', lineage).
narrative_ontology:cs_interpretation_layer_present('72634d43-6cc8-4588-869b-1a33fe19b684').
narrative_ontology:cs_reading_relation('72634d43-6cc8-4588-869b-1a33fe19b684', sortition_and_rotation__equal_chance_reading, coexists_with).
narrative_ontology:cs_reading_relation('72634d43-6cc8-4588-869b-1a33fe19b684', sortition_and_rotation__strategic_exception_reading, influences).
narrative_ontology:cs_axiom('72634d43-6cc8-4588-869b-1a33fe19b684', foundational, professionalism_incompatible_with_equality).
narrative_ontology:cs_axiom_status(professionalism_incompatible_with_equality, holdable).
narrative_ontology:cs_axiom_grounding('72634d43-6cc8-4588-869b-1a33fe19b684', professionalism_incompatible_with_equality, deontological).
narrative_ontology:cs_axiom('72634d43-6cc8-4588-869b-1a33fe19b684', foundational, accumulated_skill_enables_oligarchy).
narrative_ontology:cs_axiom_status(accumulated_skill_enables_oligarchy, holdable).
narrative_ontology:cs_axiom_grounding('72634d43-6cc8-4588-869b-1a33fe19b684', accumulated_skill_enables_oligarchy, empirically_contingent).
narrative_ontology:cs_reference_frame('72634d43-6cc8-4588-869b-1a33fe19b684', egalitarian_amateurism_framework).
narrative_ontology:cs_drift_state('72634d43-6cc8-4588-869b-1a33fe19b684', contemporary_liberal_democracy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('72634d43-6cc8-4588-869b-1a33fe19b684', '').
narrative_ontology:cs_kernel_id(sortition_and_rotation__anti_professional_reading, sortition_and_rotation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sortition_and_rotation__anti_professional_reading, citizen_body_as_equals).
narrative_ontology:constraint_beneficiary(sortition_and_rotation__anti_professional_reading, rotational_office_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CITIZEN-JUROR IN SORTITION (ROPE) — Drawn by lot to brief office with audit afterward. Experiences the constraint as coordination mechanism: the rotation prevents career-building from office, ensuring no political class can consolidate power. Exit is structurally guaranteed (the term ends; the lot may not recall). The constraint solves the collective action problem of preventing incumbency capture — all citizens benefit from the rotation's protection.
constraint_indexing:constraint_classification(sortition_and_rotation__anti_professional_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: ACCUMULATED POLITICAL SKILL (SNARE) — Expertise, institutional memory, and craft knowledge of governance cannot be deployed or rewarded when office is brief and random. The talented administrator, the experienced treasurer, the skilled general cannot build a career or accumulate authority. The constraint actively suppresses the development and transmission of political expertise. Those with skill cannot exit — they are trapped in amateur incompetence as the price of equality.
constraint_indexing:constraint_classification(sortition_and_rotation__anti_professional_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE POLIS (INSTITUTIONAL/GENERATIONAL) (ROPE) — Sortition is a coordination mechanism that solves the polis-level problem of preventing oligarchy. The constraint benefits the institutional survival of the polity by blocking the formation of a political class that could capture the state. The polis experiences this as coordination overhead (some inefficiency from amateur rule is the price of equality), not as extraction. The institutional perspective sees the mechanism as functional to the whole.
constraint_indexing:constraint_classification(sortition_and_rotation__anti_professional_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: ARISTOCRATIC REMNANT / STRATEGIC EXCEPTION (TANGLED ROPE) — Organized elites recognize that sortition solves the oligarchy problem (coordination function) but constraints on their own exercise of power emerge: the lot may draw incompetents for high-stakes offices. The constraint is enforced — sortition is the rule. But exit exists through strategic exception (generals and treasurers are elected, not drawn) where life-and-death consequences justify aristocratic selection. The organized group experiences both coordination benefit (oligarchy prevention) and extraction cost (their skill is suppressed except where cardinally necessary).
constraint_indexing:constraint_classification(sortition_and_rotation__anti_professional_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (ROPE) — From a universal perspective, sortition is a pure coordination mechanism solving the fundamental collective action problem: how to prevent the concentration of political power. The constraint is low-extraction (beneficiaries are the citizen body as a whole; no identifiable agent captures disproportionate benefit). The suppression of accumulated skill is accepted as the structural cost of equality. No agent is exploited — all citizens incur the same rotation and bear the same amateurism burden.
constraint_indexing:constraint_classification(sortition_and_rotation__anti_professional_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sortition_and_rotation__anti_professional_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sortition_and_rotation__anti_professional_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sortition_and_rotation__anti_professional_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(sortition_and_rotation__anti_professional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low. This is fundamentally a coordination mechanism with minimal extraction. The constraint prevents one agent from capturing disproportionate benefit — no political class forms, no oligarch accumulates power. All citizens rotate through office equally (in expectation). The extractiveness value reflects that there IS a real cost: amateurism sometimes produces inferior decisions, and this burden falls collectively. But it is not asymmetric extraction — the cost is borne equally by all. Suppression (0.68): High. The constraint actively suppresses the development, deployment, and reward of political skill. Experts cannot use their expertise for career advancement; talented administrators cannot consolidate authority; knowledge cannot be transmitted through an apprenticeship hierarchy (each new office-holder starts fresh, drawing from oral tradition and written precedent but not from institutional mentorship). The suppression is structural and intentional — it is the mechanism that prevents professionalism. Theater ratio (0.35): Low-moderate. Sortition is not primarily performative. The lottery mechanism is real; office holders are genuinely amateurish; terms are genuinely brief; audit is genuinely conducted. But there is some theater: the pretense that a random citizen is as qualified as a trained administrator, the ritual of accepting incompetence as the price of equality, the strategic exception (generals and treasurers are elected, not drawn) that admits the rule's practical limits while maintaining the rhetoric. Theater rises slightly over time (from 0.32 to 0.38) as strategic exceptions expand and the contradiction between egalitarian ideology and aristocratic exceptions becomes more pronounced. Claimed type: Rope. This is pure coordination. No agent is victimized in the economic sense — the citizen body benefits from oligarchy prevention, and all citizens share the amateurism burden. The suppression of skill is not extraction (which would require concentrated benefit flowing to identifiable agents); it is coordination cost.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is between professional and egalitarian frames. The skilled administrator sees Snare (their expertise is actively suppressed; they are trapped by term limits and non-renewability). The citizen-juror sees Rope (rotation prevents any one class from dominating; the mechanism is transparent and benefits the whole). The aristocratic remnant sees Tangled Rope (they accept oligarchy prevention as a genuine coordination need, but they also see their natural role suppressed except where strategic necessity overrides egalitarian ideology). The analytical observer sees pure Rope (the constraint solves a fundamental problem — how to prevent power concentration — without creating asymmetric extraction). The gap between Snare and Rope perspectives reveals the reading's core tension: is skill suppression a cost (Rope) or a damage to skilled agents (Snare)? This reading's answer is 'cost' — the skilled are burdened but not victimized, because the burden is collective and necessary.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for sortition's anti-professional reading runs from the structural position of each agent. The citizen body as equals are beneficiaries — they benefit from oligarchy prevention and equal rotation — but they experience amateurism as a cost, not as extraction. The directionality is low (d ≈ 0.35–0.45) because the benefit is collective and the cost is distributed. Accumulated political skill is a 'victim' in the structural sense but not in the extraction sense — the suppression is not asymmetric extraction that enriches identifiable agents; it is a coordination cost borne collectively. The polis itself is a secondary beneficiary (d ≈ 0.20) — institutional stability and legitimacy flow to the whole. The aristocratic remnant is organized but constrained (d ≈ 0.55) — they bear the suppression of their natural leadership but retain strategic exceptions for high-stakes offices where cardinally necessary. The analytical observer (d ≈ 0.70) stands outside the constraint system and sees the full structure: pure coordination, no extraction. The constraint does not produce negative χ for beneficiaries because the chi formula measures effective extraction flow — there is no extraction flow here, only coordination overhead.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does NOT exhibit mandatrophy. The constraint's classification as Rope is stable across perspectives because the fundamental structure is coordination, not extraction. The variation across perspectives (Snare for skill, Rope for citizens, Tangled Rope for aristocrats, Rope for the polis) reflects different experienced burden and benefit, not a contradiction in the constraint's classification. The tension is real — skilled agents genuinely experience suppression, and the equal-access rhetoric papers over real inequality in the value of political participation (a randomly selected farmer has less impact than a randomly selected philosopher). But this is not mandatrophy. Mandatrophy occurs when the same constraint's type varies between classification gates (e.g., one perspective says it is Rope by extractiveness and Snare by suppression). Here, all perspectives agree on the low-extraction-high-suppression profile; they differ in their experienced burden because they occupy different structural positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_suppression_vs_coordination_cost,
    'Is the suppression of accumulated political skill a necessary coordination cost or an extractive mechanism benefiting the citizen body at the expense of the talented?',
    'Comparative institutional analysis: Does democratic stability with sortition exceed that of elected professional governance? Do citizen outcomes improve when amateur office replaces expertise?',
    'If cost: the constraint is pure coordination (Rope from all perspectives). If extraction: talented agents are structurally victimized, reclassifying the constraint as Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_suppression_vs_coordination_cost, conceptual, 'Whether skill suppression is coordination cost or extraction').

omega_variable(
    strategic_exception_cascade,
    'Once strategic exceptions are made (generals and treasurers elected, not drawn), what prevents the exception set from expanding until sortition is replaced by selective meritocracy?',
    'Historical trajectory: Did Athenian strategic exceptions expand over time? Did the exception set absorb most high-stakes offices, degrading the constraint to theater (Piton)?',
    'If exceptions cascade: the constraint is unstable and risks degradation to Piton. If exceptions remain bounded: sortition''s structure resists expansion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_exception_cascade, empirical, 'Whether strategic exceptions remain bounded or expand to replace sortition').

omega_variable(
    reading_identity_vs_equal_chance,
    'Is this reading''s core claim (anti-professionalism) logically distinct from the equal_chance_reading''s core claim (literal identical access to power), or do they describe the same mechanism from different narrative frames?',
    'Logical analysis: Can one defend sortition as anti-professionalism while rejecting it as equal-access? Can one accept equal-access while rejecting anti-professionalism?',
    'If distinct: the readings coexist; a party can adopt one frame while another adopts the other. If identical: the readings are the same constraint described in different language.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_vs_equal_chance, conceptual, 'Whether anti-professionalism and equal-chance readings are logically distinct').

omega_variable(
    amateur_competence_floor,
    'What is the minimum competence threshold required for sortition to produce functional governance, and does random selection reliably clear it?',
    'Empirical: comparative outcomes (Athenian decisions vs. elected-oligarchy states on war, treasury, infrastructure). Did sortition-drawn magistrates perform below competence floor?',
    'If floor is exceeded: amateurism is theater, not structural suppression (Rope holds). If floor is missed: incompetence causes real harm, and the constraint extracts from those harmed (Tangled Rope or Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amateur_competence_floor, empirical, 'Whether random selection clears minimum competence threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sortition_and_rotation__anti_professional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sortprof_tr_t0, sortition_and_rotation__anti_professional_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sortprof_tr_t50, sortition_and_rotation__anti_professional_reading, theater_ratio, 50, 0.35).
narrative_ontology:measurement(sortprof_tr_t100, sortition_and_rotation__anti_professional_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(sortprof_be_t0, sortition_and_rotation__anti_professional_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(sortprof_be_t50, sortition_and_rotation__anti_professional_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(sortprof_be_t100, sortition_and_rotation__anti_professional_reading, base_extractiveness, 100, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sortition_and_rotation__anti_professional_reading, identity_coordination).
narrative_ontology:affects_constraint(sortition_and_rotation__anti_professional_reading, sortition_and_rotation__equal_chance_reading).
narrative_ontology:affects_constraint(sortition_and_rotation__anti_professional_reading, sortition_and_rotation__strategic_exception_reading).

% DUAL FORMULATION NOTE:
% Sortition_and_rotation is a contested kernel with three distinct readings: anti_professional (this story), equal_chance, and strategic_exception. Each reading has a distinct beneficiary set, victim set, and extractiveness profile. They are linked as sibling readings of the same historical/doctrinal commitment, not as separate constraints. The three readings coexist in Athenian political theory and practice. Each story captures one reading's structural logic independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
