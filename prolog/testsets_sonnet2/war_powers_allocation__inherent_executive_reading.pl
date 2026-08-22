% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive War Powers Reading (Commander-in-Chief Unilateralism)
 *   domain: constitutional_law/separation_of_powers
 *
 * SUMMARY:
 *   This story instantiates the inherent-executive reading of the war powers
 *   allocation kernel: the President's Article II commander-in-chief
 *   authority is read as an independent, self-executing grant of power to
 *   deploy force in defense of national interests, with congressional
 *   authorization treated as politically prudent but not constitutionally
 *   required. Under this reading, the War Powers Resolution's reporting
 *   requirements and 60-day withdrawal clock function as norms observed at
 *   executive discretion, and post-deployment appropriations function as
 *   ratification rather than the deliberative authorization the
 *   coordination-primacy reading treats as necessary. This is a distinct
 *   constraint from the congressional_primacy_reading and
 *   functional_accommodation_reading siblings — each has a different
 *   beneficiary/victim structure and a different ε, per the ε-invariance
 *   principle; they are not the same constraint viewed from different angles.
 *
 * KEY AGENTS:
 *   - executive_branch: agenda_setter/beneficiary (institutional/arbitrage) — orders deployments, controls the legal justification apparatus
 *   - congress_war_powers_committees: payer (organized/constrained) — nominal Article I authority reduced to after-the-fact ratification
 *   - deployed_service_members: payer (powerless/trapped) — bear the physical risk of unilaterally ordered deployments
 *   - civilian_populations_in_theater: payer (powerless/trapped) — bear the costs of force with no representation in the decision
 *   - national_security_apparatus: beneficiary (institutional/arbitrage) — gains operational speed and reduced political friction
 *   - defense_contractors: beneficiary (organized/mobile) — benefits from deployment frequency correlated with lower authorization friction
 *   - federal_courts: observer (institutional/analytical) — declines to adjudicate via political question doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.32).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers Reading (Commander-in-Chief Unilateralism)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional_law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, 'edb6b089-6064-4df4-a87c-01269fadcc62').
narrative_ontology:cs_kernel_codification('edb6b089-6064-4df4-a87c-01269fadcc62', distributed).
narrative_ontology:cs_authority_grounding('edb6b089-6064-4df4-a87c-01269fadcc62', practice).
narrative_ontology:cs_interpretation_layer_present('edb6b089-6064-4df4-a87c-01269fadcc62').
narrative_ontology:cs_reading_relation('edb6b089-6064-4df4-a87c-01269fadcc62', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('edb6b089-6064-4df4-a87c-01269fadcc62', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('edb6b089-6064-4df4-a87c-01269fadcc62', foundational, commander_in_chief_power_self_executing).
narrative_ontology:cs_axiom_status(commander_in_chief_power_self_executing, holdable).
narrative_ontology:cs_axiom_grounding('edb6b089-6064-4df4-a87c-01269fadcc62', commander_in_chief_power_self_executing, conventional).
narrative_ontology:cs_axiom('edb6b089-6064-4df4-a87c-01269fadcc62', secondary, appropriations_constitute_authorization).
narrative_ontology:cs_axiom_status(appropriations_constitute_authorization, holdable).
narrative_ontology:cs_axiom_grounding('edb6b089-6064-4df4-a87c-01269fadcc62', appropriations_constitute_authorization, instrumental).
narrative_ontology:cs_reference_frame('edb6b089-6064-4df4-a87c-01269fadcc62', founding_era_barbary_emergency_practice).
narrative_ontology:cs_drift_state('edb6b089-6064-4df4-a87c-01269fadcc62', post_war_on_terror_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('edb6b089-6064-4df4-a87c-01269fadcc62', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, national_security_apparatus).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, defense_contractors).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress_war_powers_committees).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, deployed_service_members).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, civilian_populations_in_theater).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_doctrine).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, commander_in_chief_plenary_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Orders force deployments citing Article II commander-in-chief authority, treats congressional authorization requests as optional political cover rather than legal prerequisite, and relies on Justice Department Office of Legal Counsel opinions it commissions itself to validate each deployment after the fact.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, executive_branch, beneficiary).

% Holds nominal Article I war-declaration power but has not formally declared war since 1942; the War Powers Resolution's reporting and 60-day clock is treated by every administration as advisory. Members can hold hearings, cut funding, or pass resolutions, but withholding appropriations mid-deployment is politically costly once troops are committed, so authorization is sought (if at all) only after the fact and functions as ratification rather than a check.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress_war_powers_committees, payer,
    organized, biographical, constrained, national).

% Bear the direct physical risk of deployments ordered without congressional debate or formal authorization. Have no institutional voice in the decision to deploy and cannot exit the chain of command; their exposure is set entirely by executive determinations of national interest.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, deployed_service_members, payer,
    powerless, immediate, trapped, global).

% Live in the areas where force is deployed under this reading's authority. Bear the costs of military action decided in a foreign capital's executive branch with no representation in, or even public notice of, the decision process.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, civilian_populations_in_theater, payer,
    powerless, immediate, trapped, regional).

% Gains operational speed and reduced political friction when deployment authority does not require prior legislative debate; classified operations and rapid-response postures are justified as requiring executive unilateralism.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, national_security_apparatus, beneficiary,
    institutional, civilizational, arbitrage, global).

% Benefit from sustained and rapidly initiated deployments that generate procurement and sustainment contracts; frequency and duration of engagements correlate with revenue, and the low authorization friction of this reading increases the rate of deployments relative to a congressional-primacy regime.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, defense_contractors, beneficiary,
    organized, generational, mobile, global).

% Are repeatedly asked to adjudicate war powers disputes but routinely decline via the political question doctrine, leaving the executive's claimed authority effectively unreviewed in practice even where the constitutional text is contested.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine problem of needing rapid, decisive military response to imminent threats when convening Congress would introduce fatal delay — a single decisive commander is more effective than a deliberative body for time-critical defensive action.
% TRANSFER_FUNCTION: Moves the practical power to initiate and sustain military force from the collective legislature (and the constituencies it represents) to the executive; moves risk of engagement from decision-makers to deployed personnel and foreign civilian populations who have no voice in the decision.
% ABSENT_VOICES: Foreign civilian populations in theaters of operation have no representation whatsoever in the U.S. constitutional process that authorizes force against or near them. Rank-and-file service members ordered into deployments have no institutional channel to contest the legal basis of an order before executing it.
% DISAPPEARANCE_RATIONALE: If the inherent-authority reading were repudiated and enforced as such, every future deployment beyond genuine emergency defense would require prior congressional authorization, materially slowing initiation of force, shifting political accountability onto named legislators, and likely reducing deployment frequency and duration — defense procurement cycles tied to sustained engagements would also contract.
% FOUNDING_PROBLEM: The constitutional design left an ambiguous seam between Congress's Article I power to declare war and the President's Article II role as commander-in-chief; early practice (naval actions against Barbary pirates, use of force in emergencies) needed some doctrine for who decides when speed matters and legislative process cannot keep pace.
% FOUNDING_PROBLEM_CORROBORATION: The executive branch and OLC opinions it commissions attest that the founding problem (need for rapid unilateral defensive response) remains fully live and justifies broad standing authority. Outside the executive branch, congressional war powers scholars, the Congressional Research Service, and retired flag officers testifying before Congress attest that the doctrine has expanded far past emergency defense into discretionary force projection with no genuine time constraint, and that appropriations-based ratification is a post-hoc legal cover rather than the deliberative authorization the framers intended.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) is substantial and rising over the interval because the reading has expanded from emergency-defense use (Barbary-era, immediate threat response) to discretionary force projection covering prolonged and geographically dispersed engagements, with appropriations serving as the primary post-hoc legitimation mechanism. Suppression is comparatively low (0.32) because the reading does not require actively silencing dissent — it operates through structural default: courts decline review via political question doctrine, and Congress's own institutional incentives (avoiding blame for a failed intervention, or for underfunding troops already in the field) do the suppressive work rather than direct coercion. Theater ratio (0.55) is elevated because a substantial share of activity — War Powers Resolution reports filed and functionally ignored, OLC opinions commissioned to bless decisions already made — is performative compliance with a check that carries no binding consequence.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch is the structural beneficiary: it sets the deployment agenda, controls the legal interpretive apparatus (OLC), and captures the political and operational benefits of speed. National security apparatus and defense contractors are secondary beneficiaries whose institutional interests are served by higher-frequency, lower-friction deployment. Congress is a victim in this reading specifically because its Article I role is reduced from prior constraint to subsequent ratification — the coordination function it was built to provide (deliberative check before commitment) is bypassed. Deployed service members and foreign civilian populations are the most acute victims: they bear irreversible physical costs of a decision process in which they have zero voice, and their exit options are trapped by design (military command structure, geographic circumstance).
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is deliberate: there IS a genuine coordination function here (rapid response capability when deliberative process would be fatally slow — the founding problem is real and partially still live) coexisting with asymmetric extraction (the same unilateral-authority structure that solves genuine emergencies is used to bypass authorization for discretionary, non-emergency, prolonged engagements). Classifying this as pure snare would erase the genuine coordination case for some unilateral authority (imminent attack response); classifying it as pure rope would erase the demonstrated pattern of authority expanding well past the emergency case into routine discretionary force projection benefiting identifiable institutional actors. The tangled_rope reading holds both facts without collapsing the distinction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_authority_scope_ambiguity,
    'Does the commander-in-chief clause grant an independently sufficient, self-executing authority to initiate force in defense of national interests, or is it a purely operational-command power that presupposes a prior, separately-secured authorization to engage in hostilities?',
    'This is the central textual and historical dispute among the three kernel readings; it would require either a definitive Supreme Court ruling abandoning the political question doctrine in this domain, or a durable multi-administration convergence of practice that one side of Congress and the executive both treat as settled.',
    'If the inherent reading is correct, current practice is coordination properly functioning as designed and the extraction framing overstates the case. If incorrect, decades of deployments under this reading constitute a sustained constitutional violation with congress and downstream populations as its victims — exactly the reading this story authors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(inherent_authority_scope_ambiguity, conceptual, 'Whether Article II commander-in-chief power is self-executing or presupposes prior authorization — the foundational dispute this reading takes a side on.').

omega_variable(
    appropriations_as_ratification_validity,
    'Does Congress''s decision to appropriate funds for an already-commenced deployment constitute genuine constitutional ratification of the decision to use force, or is it merely evidence of the political impossibility of defunding troops already at risk?',
    'Compare congressional voting patterns and floor debate content across deployments where authorization was sought in advance versus deployments funded only after commencement; examine whether members who voted for appropriations explicitly disclaimed endorsing the initial deployment decision.',
    'If appropriations function as genuine ratification, congress''s victim status in this reading is overstated — actual consent exists, just delayed. If appropriations are coerced by sunk-cost political dynamics rather than genuine assent, the ratification mechanism is closer to extraction than coordination, supporting the high extractiveness score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_as_ratification_validity, empirical, 'Whether post-hoc appropriations represent genuine congressional consent or coerced sunk-cost funding.').

omega_variable(
    sibling_reading_divergence_locus,
    'Where exactly does the disagreement between this reading and the functional_accommodation_reading sit — is it a disagreement about the correct legal test (imminence-based accommodation vs. blanket inherent authority), or merely a disagreement about how the same test applies to disputed facts on the ground?',
    'Compare the two readings'' treatment of specific historical cases (e.g., Libya 2011, Syria strikes) — if both readings would classify the same case identically, the disagreement is rhetorical rather than structural; if they diverge on classification, the disagreement is a genuine test-level split.',
    'If the disagreement is merely factual application, the two readings could in principle converge on outcomes despite differing rhetoric, weakening the case for treating them as fully distinct constraints with different victim sets. If the disagreement is test-level, the constraints remain properly distinct per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_divergence_locus, conceptual, 'Whether this reading and the functional_accommodation sibling diverge at the level of legal test or only at the level of factual application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1950, war_powers_allocation__inherent_executive_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(war__tr_t1965, war_powers_allocation__inherent_executive_reading, theater_ratio, 1965, 0.35).
narrative_ontology:measurement(war__tr_t1980, war_powers_allocation__inherent_executive_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(war__tr_t1995, war_powers_allocation__inherent_executive_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(war__tr_t2010, war_powers_allocation__inherent_executive_reading, theater_ratio, 2010, 0.5).
narrative_ontology:measurement(war__tr_t2024, war_powers_allocation__inherent_executive_reading, theater_ratio, 2024, 0.55).

% Extraction over time
narrative_ontology:measurement(war__be_t1950, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(war__be_t1965, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(war__be_t1980, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(war__be_t1995, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(war__be_t2010, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(war__be_t2024, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1950, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(war__su_t1965, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1965, 0.22).
narrative_ontology:measurement(war__su_t1980, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1980, 0.24).
narrative_ontology:measurement(war__su_t1995, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1995, 0.27).
narrative_ontology:measurement(war__su_t2010, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement(war__su_t2024, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2024, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the war_powers_allocation kernel, decomposed per the ε-invariance principle rather than represented as a single constraint with a measurement parameter. congressional_primacy_reading treats explicit authorization as constitutionally necessary (low ε for the executive-benefit structure, higher suppression of executive unilateralism); functional_accommodation_reading splits the difference by operational context (moderate ε, context-dependent victim set). This story (inherent_executive_reading) carries the highest extractiveness of the three because it authorizes the broadest unilateral scope and names congress and downstream populations as victims of the resulting authority concentration. All three share the same underlying constitutional text and historical practice but diverge on which legal test governs — they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__inherent_executive_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
