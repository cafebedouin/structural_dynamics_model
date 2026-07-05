% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity-Boundary Marker
 *   domain: systematic_theology/ecclesiology/history_of_doctrine
 *
 * SUMMARY:
 *   This story instantiates one specific reading of the contested Nicene
 *   Creed kernel: the creed operates not primarily as a metaphysical
 *   assent-test (the strict_orthodox_reading) nor as a historically
 *   contingent witness authorized by community discernment (the
 *   symbolic_confessional_reading), but as a liturgical performance that
 *   constitutes identity-boundary and belonging through repeated communal
 *   recitation, independent of what any individual participant cognitively
 *   holds about the propositions recited. Under this reading the creed is
 *   structurally closer to a coordination device — a shared rite that lets
 *   dispersed congregations and communions recognize one another — than to a
 *   coercive metaphysical test. Extraction is very low because almost nothing
 *   material is transferred and almost no one is compelled by force to
 *   recite; theater_ratio rises over the long interval because as doctrinal
 *   controversy has cooled in most communions, an increasing share of the
 *   recitation's function is performative continuity rather than active
 *   boundary-policing against live heresy threats.
 *
 * KEY AGENTS:
 *   - worshiping_congregations: primary beneficiaries of the coordination function (organized/mobile)
 *   - liturgical_clergy: administer and sustain the practice (institutional/constrained by vocational identity)
 *   - ecumenical_communions: use the shared rite as cross-jurisdictional recognition marker (institutional/arbitrage)
 *   - doctrinally_dissenting_members: perform without full assent, rarely consulted (powerless/constrained)
 *   - doctrinal_enforcement_bodies: analytical observers who draw on this substrate for the stricter sibling reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.12).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity-Boundary Marker").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "systematic_theology/ecclesiology/history_of_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '8c576194-9d9f-488e-b1cd-504450ec9905').
narrative_ontology:cs_kernel_codification('8c576194-9d9f-488e-b1cd-504450ec9905', fixed_text).
narrative_ontology:cs_authority_grounding('8c576194-9d9f-488e-b1cd-504450ec9905', practice).
narrative_ontology:cs_interpretation_layer_present('8c576194-9d9f-488e-b1cd-504450ec9905').
narrative_ontology:cs_reading_relation('8c576194-9d9f-488e-b1cd-504450ec9905', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('8c576194-9d9f-488e-b1cd-504450ec9905', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('8c576194-9d9f-488e-b1cd-504450ec9905', foundational, performance_constitutes_belonging_independent_of_assent).
narrative_ontology:cs_axiom_status(performance_constitutes_belonging_independent_of_assent, holdable).
narrative_ontology:cs_axiom_grounding('8c576194-9d9f-488e-b1cd-504450ec9905', performance_constitutes_belonging_independent_of_assent, conventional).
narrative_ontology:cs_axiom('8c576194-9d9f-488e-b1cd-504450ec9905', secondary, identity_boundary_function_separable_from_propositional_truth_claim).
narrative_ontology:cs_axiom_status(identity_boundary_function_separable_from_propositional_truth_claim, holdable).
narrative_ontology:cs_axiom_grounding('8c576194-9d9f-488e-b1cd-504450ec9905', identity_boundary_function_separable_from_propositional_truth_claim, instrumental).
narrative_ontology:cs_reference_frame('8c576194-9d9f-488e-b1cd-504450ec9905', fourth_century_conciliar_recitation_norm).
narrative_ontology:cs_drift_state('8c576194-9d9f-488e-b1cd-504450ec9905', contemporary_pluralist_liturgy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c576194-9d9f-488e-b1cd-504450ec9905', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, worshiping_congregations).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, liturgical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, ecumenical_communions).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, liturgical_practice_constitutes_belonging).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Recite the creed communally in weekly or regular liturgy. The recitation binds them into a shared identity and rhythm of belonging regardless of whether each member holds a settled cognitive position on homoousios or the precise metaphysics of the Trinity. Members who privately doubt or reinterpret clauses still participate fully; exit from any single congregation to another liturgical tradition (or to non-liturgical worship) is generally available without severe penalty.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, worshiping_congregations, beneficiary,
    organized, generational, mobile, global).

% Administer the liturgy in which the creed is recited, choosing cadence, translation, and occasion. They benefit from the coordination function the creed provides (a stable, recognizable rite that unifies otherwise dispersed congregations) but bear career and vocational costs if they depart from customary liturgical form, since their professional identity is embedded in the practice.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, liturgical_clergy, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(nicene_creed_authority__liturgical_habituation_reading, liturgical_clergy, beneficiary).

% Use shared recitation of the creed across otherwise doctrinally and jurisdictionally distinct bodies (Catholic, Orthodox, many Protestant traditions) as a low-cost marker of mutual recognizability. This lets them coordinate ecumenical dialogue and mutual identification without resolving deeper metaphysical or authority disputes.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, ecumenical_communions, beneficiary,
    institutional, civilizational, arbitrage, global).

% Hold private metaphysical reservations about specific clauses (e.g., the exact sense of 'begotten, not made') but continue reciting the creed as a condition of full liturgical participation. Under this reading their situation is mild: the recitation asks performance, not sworn cognitive assent, so their dissent costs them little beyond the discomfort of communal speech-acts they do not fully endorse. Their voice on whether performance-without-assent is honest practice is rarely solicited.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, doctrinally_dissenting_members, excluded,
    powerless, biographical, constrained, local).

% Watch the same liturgical practice as raw material for a stricter metaphysical enforcement reading (the sibling strict_orthodox_reading) — they do not administer this constraint, but they draw on the social substrate of habituated performance to argue for binding metaphysical assent.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, doctrinal_enforcement_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, memorized, communally-performed text that lets dispersed congregations, clergy, and communions recognize one another as part of a shared tradition without requiring settled agreement on contested metaphysical technicalities.
% TRANSFER_FUNCTION: Moves very little materially; it transfers social recognition and a felt sense of belonging from the act of shared recitation to participants, and transfers a modest reputational/coordination benefit to clergy and communions who administer or invoke it.
% ABSENT_VOICES: Doctrinally dissenting members who recite the creed as performance without full assent are rarely asked whether performance-without-belief is coherent or costly to them; their acquiescence is assumed rather than solicited.
% DISAPPEARANCE_RATIONALE: If liturgical recitation of the creed vanished overnight, congregations would lose a recognizable shared rite and ecumenical bodies would lose a low-cost marker of mutual identification — some rearrangement would follow. But because this reading holds the function is performative rather than metaphysically load-bearing, many participants would report little change to their actual faith or communal life, hence the dispute over how much would truly rearrange.
% FOUNDING_PROBLEM: Early Christian communities, dispersed across the Mediterranean and beyond, needed a portable, memorizable, communally-recitable marker that let strangers recognize each other as co-religionists and worship together despite geographic and linguistic distance.
% FOUNDING_PROBLEM_CORROBORATION: Comparative liturgical scholarship (outside any single confessional body) documents continuous cross-traditional use of the creed as a recognition device in ecumenical contexts; sociologists of religion studying congregational practice independently report that habituated communal recitation functions as identity-formation regardless of members' articulated theology, corroborating the reading from outside the benefiting clergy and communions.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, contested).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.08 (ceiling for this reading per the expected structural delta) because liturgical recitation moves no significant material transfer and imposes no meaningful coercive cost on non-believing participants under this reading's own premises. Suppression is low (0.12) since exit from any single liturgical community is generally available and no sanction regime is invoked by this reading (that belongs to the sibling strict_orthodox_reading). Theater_ratio is authored higher and rising (0.20→0.40) because the performative, identity-marking function this reading emphasizes is, almost by definition, substantially theatrical in the neutral sense — repeated communal speech-act whose importance is social-constitutive rather than propositional-verificatory; as doctrinal controversy has become less immediately existential across most communions, this performative share has grown. Accessibility_collapse is moderate (0.30): alternative forms of communal identity marking exist and are used by non-liturgical traditions, so the creed's performative function has not collapsed all alternatives. Resistance is low (0.15) since this reading generates little friction — it is the least contested account among clergy administering ordinary worship.
 *
 * DIRECTIONALITY LOGIC:
 *   Worshiping congregations and ecumenical communions sit near the beneficiary end: they receive belonging, recognizability, and coordination benefit from a practice that costs them almost nothing structurally. Liturgical clergy are dual-positioned — they administer the practice (agenda_setter) and also draw identity and vocational meaning from it (beneficiary), but their exit is constrained by career and relational identity investment in the liturgical role. Doctrinally dissenting members are the closest thing to a target under this reading, though the cost imposed on them is mild (discomfort of performing words they do not fully hold) rather than coercive, which is precisely what distinguishes this reading from strict_orthodox_reading, where the same dissent would carry heresy sanction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in the opposite direction of the sibling readings: because performance is decoupled from metaphysical assent, there is no live 'problem' this reading solves in the sense of adjudicating truth — its founding problem (community recognition across distance) remains live precisely because it never depended on resolving the metaphysical contest. It would be a mandatrophy error to read the rising theater_ratio as evidence the practice has become empty; under this reading, theatrical performance IS the constitutive mechanism, not a corruption of an earlier substantive function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_assent_decoupling_stability,
    'Can liturgical performance of the creed durably sustain identity-boundary function if a critical mass of participants become aware their recitation is decoupled from metaphysical assent, or does the decoupling erode the practice''s coordination value over time?',
    'Longitudinal sociological study of congregations tracking self-reported assent levels alongside continued liturgical participation and communal cohesion measures over multiple decades.',
    'If decoupling erodes coordination value, this reading''s low-extraction rope classification would drift toward scaffold (transitional coordination whose justification depends on the transition, i.e. gradual loss of function) or toward piton (performance persisting after the coordination function has substantially atrophied).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_assent_decoupling_stability, empirical, 'Whether performance-without-assent is a stable equilibrium or a transitional/degrading state.').

omega_variable(
    reading_selection_grounds,
    'Is the liturgical_habituation_reading the correct primary lens for the Nicene Creed''s operative authority, or is it better understood as a secondary social effect riding on top of a metaphysical claim that remains primary even when individual assent is absent?',
    'No single empirical test resolves this; it depends on whether one takes the creed''s own self-understanding (a propositional confession of faith) or its observed sociological function (a performed boundary marker) as authoritative for classification purposes.',
    'Adopting the strict_orthodox framing instead would relocate this same recitation practice into a substantially more extractive, enforcement-dependent classification (tangled_rope or snare for dissenting members), since the metaphysical-assent reading treats non-belief during recitation as a violation rather than a tolerated performance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_grounds, conceptual, 'Alternative framing: liturgical function as primary vs. as downstream social effect of a metaphysical claim.').

omega_variable(
    clergy_identity_fusion,
    'To what extent is clergy''s constrained exit from liturgical administration a matter of genuine vocational identity fusion versus institutional/economic dependency dressed as vocation?',
    'Interview studies with clergy who have left liturgical traditions, distinguishing stated reasons for difficulty of exit (identity, economic, relational, institutional).',
    'If primarily economic/institutional rather than identity-fused, clergy''s exit_options may be better modeled as constrained-by-dependency rather than identity_locked, which would not change this reading''s low extractiveness but would refine the seat-level directionality analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(clergy_identity_fusion, empirical, 'Nature of clergy''s constrained exit from liturgical administration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 325, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t325, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 325, 0.2).
narrative_ontology:measurement_basis(nice_tr_t325, observed).
narrative_ontology:measurement(nice_tr_t600, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 600, 0.28).
narrative_ontology:measurement_basis(nice_tr_t600, observed).
narrative_ontology:measurement(nice_tr_t1000, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1000, 0.33).
narrative_ontology:measurement_basis(nice_tr_t1000, observed).
narrative_ontology:measurement(nice_tr_t1500, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1500, 0.36).
narrative_ontology:measurement_basis(nice_tr_t1500, observed).
narrative_ontology:measurement(nice_tr_t1900, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1900, 0.38).
narrative_ontology:measurement_basis(nice_tr_t1900, observed).
narrative_ontology:measurement(nice_tr_t2025, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 2025, 0.4).
narrative_ontology:measurement_basis(nice_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(nice_be_t325, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 325, 0.05).
narrative_ontology:measurement_basis(nice_be_t325, observed).
narrative_ontology:measurement(nice_be_t600, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 600, 0.06).
narrative_ontology:measurement_basis(nice_be_t600, observed).
narrative_ontology:measurement(nice_be_t1000, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1000, 0.07).
narrative_ontology:measurement_basis(nice_be_t1000, observed).
narrative_ontology:measurement(nice_be_t1500, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1500, 0.07).
narrative_ontology:measurement_basis(nice_be_t1500, observed).
narrative_ontology:measurement(nice_be_t1900, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1900, 0.08).
narrative_ontology:measurement_basis(nice_be_t1900, observed).
narrative_ontology:measurement(nice_be_t2025, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 2025, 0.08).
narrative_ontology:measurement_basis(nice_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(nicene_creed_authority__liturgical_habituation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(nicene_creed_authority__liturgical_habituation_reading, 0.08).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% This constraint is the liturgical_habituation_reading member of a three-story kernel family for nicene_creed_authority. It supplies the low-extraction social substrate (habituated communal performance) that the strict_orthodox_reading draws on to argue for binding metaphysical enforcement, and that the symbolic_confessional_reading draws on to argue for pluralist, discernment-based reinterpretation. Each sibling has its own ε: this reading is authored near the coordination floor (ε≈0.08); the strict_orthodox_reading is expected to be substantially more extractive due to enforcement/sanction machinery; the symbolic_confessional_reading is expected to sit closer to a pure rope with even less structural boundary-policing. Do not average these ε values — they are three distinct constraints sharing one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
