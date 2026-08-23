% ============================================================================
% CONSTRAINT STORY: human_transcendence_pathway__jerusalem_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_human_transcendence_pathway__jerusalem_reading, []).

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
 *   constraint_id: human_transcendence_pathway__jerusalem_reading
 *   human_readable: Jerusalem Pathway: Participatory Rebuilding of Communion under Divine Blessing
 *   domain: theological/political/social
 *
 * SUMMARY:
 *   The Jerusalem reading of the human transcendence pathway presents the
 *   post-exilic reconstruction (Ezra-Nehemiah, prophetic literature) as the
 *   paradigmatic form of authentic human community: a people constituted not
 *   by conquest or technological mastery but by covenantal reception of
 *   divine gift, expressed in patient participatory labor that integrates
 *   plurality into communion. This is a scaffold constraint — it carries a
 *   sunset clause (the messianic/eschatological fulfillment where 'God will
 *   be all in all') and its justification is the transitional rebuilding, not
 *   a steady-state regime. The constraint claims low extractiveness
 *   (persuasion, formation, liturgical formation) and low suppression (no
 *   coercion of conscience, alternatives like Hellenization or imperial
 *   collaboration remain live options). The beneficiaries are the community
 *   as a whole, especially the marginalized (exiles, poor, resident aliens)
 *   who are structurally centered in the Torah's social vision. There are no
 *   structural victims, though participatory builders sacrifice efficiency
 *   for solidarity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(human_transcendence_pathway__jerusalem_reading, 0.25).
domain_priors:suppression_score(human_transcendence_pathway__jerusalem_reading, 0.15).
domain_priors:theater_ratio(human_transcendence_pathway__jerusalem_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(human_transcendence_pathway__jerusalem_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(human_transcendence_pathway__jerusalem_reading, scaffold).
narrative_ontology:human_readable(human_transcendence_pathway__jerusalem_reading, "Jerusalem Pathway: Participatory Rebuilding of Communion under Divine Blessing").
narrative_ontology:topic_domain(human_transcendence_pathway__jerusalem_reading, "theological/political/social").

narrative_ontology:has_sunset_clause(human_transcendence_pathway__jerusalem_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(human_transcendence_pathway__jerusalem_reading, 'db748806-73f0-424c-b422-cca5d27c6560').
narrative_ontology:cs_kernel_codification('db748806-73f0-424c-b422-cca5d27c6560', fixed_text).
narrative_ontology:cs_authority_grounding('db748806-73f0-424c-b422-cca5d27c6560', lineage).
narrative_ontology:cs_interpretation_layer_present('db748806-73f0-424c-b422-cca5d27c6560').
narrative_ontology:cs_reading_relation('db748806-73f0-424c-b422-cca5d27c6560', human_transcendence_pathway__babel_reading, coexists_with).
narrative_ontology:cs_reading_relation('db748806-73f0-424c-b422-cca5d27c6560', human_transcendence_pathway__technocratic_vs_incarnational_reading, influences).
narrative_ontology:cs_axiom('db748806-73f0-424c-b422-cca5d27c6560', foundational, plurality_integrated_into_communion_not_uniformity).
narrative_ontology:cs_axiom_status(plurality_integrated_into_communion_not_uniformity, holdable).
narrative_ontology:cs_axiom_grounding('db748806-73f0-424c-b422-cca5d27c6560', plurality_integrated_into_communion_not_uniformity, theological).
narrative_ontology:cs_axiom('db748806-73f0-424c-b422-cca5d27c6560', foundational, participatory_labor_under_divine_blessing_not_technocratic_optimization).
narrative_ontology:cs_axiom_status(participatory_labor_under_divine_blessing_not_technocratic_optimization, holdable).
narrative_ontology:cs_axiom_grounding('db748806-73f0-424c-b422-cca5d27c6560', participatory_labor_under_divine_blessing_not_technocratic_optimization, theological).
narrative_ontology:cs_axiom('db748806-73f0-424c-b422-cca5d27c6560', secondary, marginalized_as_hermeneutical_center_of_rebuilding).
narrative_ontology:cs_axiom_status(marginalized_as_hermeneutical_center_of_rebuilding, holdable).
narrative_ontology:cs_axiom_grounding('db748806-73f0-424c-b422-cca5d27c6560', marginalized_as_hermeneutical_center_of_rebuilding, deontological).
narrative_ontology:cs_reference_frame('db748806-73f0-424c-b422-cca5d27c6560', post_exilic_covenantal_rebuilding).
narrative_ontology:cs_drift_state('db748806-73f0-424c-b422-cca5d27c6560', contemporary_catholic_social_teaching, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('db748806-73f0-424c-b422-cca5d27c6560', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, returning_exiles).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, community_as_whole).
narrative_ontology:constraint_beneficiary(human_transcendence_pathway__jerusalem_reading, marginalized_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, marginalized_populations).
narrative_ontology:constraint_victim(human_transcendence_pathway__jerusalem_reading, participatory_builders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Displaced communities returning to rebuild Jerusalem. Their marginalization makes them primary beneficiaries of a pathway that centers the vulnerable. Their identity is fused with the return and rebuilding; exit would mean abandoning the covenantal narrative that constitutes them.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, returning_exiles, beneficiary,
    moderate, biographical, identity_locked, local).

% The reconstituted polity centered on Temple and Torah. Benefits from social cohesion, shared liturgical life, and integration of diversity into communion. Exit options constrained by geographic, liturgical, and covenantal bonds.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, community_as_whole, beneficiary,
    organized, generational, constrained, regional).

% Widows, orphans, resident aliens, and the poor who are structurally centered in the Torah's social vision. They benefit from gleaning laws, sabbatical remission, and jubilee restoration. They also bear the cost of communal discipline when the community fails its own standards.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, marginalized_populations, beneficiary,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, marginalized_populations, payer).

% Priests, elders, and lay leaders who enact the slow rebuilding — liturgical, legal, and material. They set the agenda through councils and prophetic discernment. They sacrifice efficiency (foregoing imperial-style forced labor or technocratic shortcuts) for solidarity, bearing the cost of patient formation.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, participatory_builders, agenda_setter,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(human_transcendence_pathway__jerusalem_reading, participatory_builders, payer).

% The prophetic office that holds the community accountable to the covenantal standard, naming when rebuilding becomes self-serving. Not a separate class but a charism recognized within the community. Exit is constrained by vocation.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, prophetic_voice, agenda_setter,
    organized, generational, constrained, national).

% Imperial administrators, Hellenistic reformers, and later modernizers who advocate efficient, unified, top-down reconstruction. They are excluded from the Jerusalem pathway's deliberative structures because their logic (efficiency, uniformity, control) contradicts its form (participatory, plural, gifted). They would object to the slowness and 'inefficiency' of covenantal rebuilding.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, technocratic_proponents, excluded,
    powerful, biographical, arbitrage, global).

% Builders of unified technological/linguistic systems who seek security through homogeneity and centralized control (the Tower logic). They are excluded because the Jerusalem pathway explicitly refuses the Babel temptation to 'make a name for ourselves' through uniform systems. They would object to the pathway's embrace of plurality and dependence on divine gift.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, babel_proponents, excluded,
    powerful, civilizational, mobile, global).

% Scholars of political theology, Catholic social doctrine, and comparative eschatology who analyze the Jerusalem reading as a hermeneutical key for authentic human development. They neither collect nor pay but track the constraint's historical instantiations and theoretical coherence.
narrative_ontology:constraint_stakeholder(human_transcendence_pathway__jerusalem_reading, theological_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Integrates human plurality into communion without erasing difference: the Torah's legal-liturgical framework coordinates diverse tribes, resident aliens, and vocational roles into a single covenantal people through shared worship, sabbatical rhythms, and distributive justice — solving the coordination problem of unity-without-uniformity.
% TRANSFER_FUNCTION: Moves the burden of reconstruction from the vulnerable (exiles, poor) onto the capable (builders, leaders) through voluntary participatory labor and redistributive law (gleaning, tithes, jubilee). Transfers status and voice from the center to the margins. Transfers the logic of security from human control (walls, armies, uniform systems) to divine gift (blessing on faithful cooperation).
% ABSENT_VOICES: The voices of those who would impose unity through force or algorithm — empire builders, totalitarian planners, transhumanist optimizers — are structurally excluded because their anthropology (humanity as raw material for optimization) contradicts the pathway's anthropology (humanity as participatory recipient of gift). They are absent not by accident but because the pathway's form (slow, deliberative, plural) cannot accommodate their logic without ceasing to be itself.
% DISAPPEARANCE_RATIONALE: If the Jerusalem pathway vanished overnight, the post-exilic community would lose its integrating framework: no shared liturgy to bind tribes, no sabbatical/jubilee to reset economic extraction, no prophetic check on elite capture. The community would either fragment into tribal enclaves or be recolonized by imperial logics (Babel/technocratic). The world rearranges because this constraint is the historically operative form of 'communio' for this people.
% FOUNDING_PROBLEM: How to rebuild a people shattered by exile and temple destruction without replicating the imperial violence that destroyed them — how to achieve unity that does not become uniformity, security that does not become domination, and identity that does not become exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by the post-exilic biblical witness (Ezra-Nehemiah, Haggai-Zechariah, Third Isaiah) which records the struggle itself, not a triumphalist outcome. The rabbinic tradition (Mishnah, Talmud) continues the participatory deliberation. Modern Catholic social teaching (from Rerum Novarum to Fratelli Tutti) explicitly retrieves the Jerusalem logic as normative for authentic development. No major theological tradition outside the benefiting community denies the historical reality of the post-exilic rebuilding as a distinctive pathway.
narrative_ontology:disappearance_verdict(human_transcendence_pathway__jerusalem_reading, world_rearranges).
narrative_ontology:founding_problem_status(human_transcendence_pathway__jerusalem_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(human_transcendence_pathway__jerusalem_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(human_transcendence_pathway__jerusalem_reading, 'none', 1).
narrative_ontology:epsilon_provenance(human_transcendence_pathway__jerusalem_reading, 0.25, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(human_transcendence_pathway__jerusalem_reading_tests).
:- end_tests(human_transcendence_pathway__jerusalem_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the pathway operates through formation and consent, not extraction; the 'costs' borne by builders (foregone efficiency) are voluntarily assumed as formative participation, not extracted. Suppression is low (0.15) because the pathway does not forbid alternative visions (Babel, technocratic) — it simply refuses to adopt them, and those alternatives remain available in the wider world. Theater ratio is low but nonzero (0.18) because historical instantiations (Second Temple period, medieval Christendom, modern Catholic integralism) show drift toward ritual performance substituting for participatory justice. Accessibility collapse is moderate (0.45): once the covenantal logic is grasped, alternatives appear as betrayals of the people's identity, but the pathway never achieves the total alternative-closure of a natural law. Resistance is moderate (0.40) because the pathway consistently faces internal resistance (elites preferring efficiency, prophets demanding purity) and external pressure (empires demanding conformity).
 *
 * PERSPECTIVAL GAP:
 *   From the returning_exiles seat, the pathway is pure gift — they receive restoration they could not achieve. From the participatory_builders seat, it is costly discipleship — they forego efficient methods for the sake of communion. From the technocratic_proponents seat (excluded), it is irrational inefficiency. From the theological_observers seat, it is a hermeneutical key for reading history. The engine will compute these as different effective types: likely rope/scaffold for beneficiaries, tangled_rope for builders (coordination + asymmetric sacrifice), mountain-adjacent for observers (the pathway appears as a structural feature of the tradition's self-understanding).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (returning_exiles, community_as_whole, marginalized) have low directionality (d ≈ 0.1–0.2): the constraint subsidizes them via redistributive law and centering of the vulnerable. Agenda_setters (participatory_builders, prophetic_voice) have moderate directionality (d ≈ 0.4–0.5): they both administer and bear costs (sacrifice of efficiency). Excluded agents (technocratic_proponents, babel_proponents) have high directionality (d ≈ 0.8–0.9) if they were inside, but their exclusion means they experience the constraint as an external barrier — the constraint's suppression operates precisely to keep them out. The divine_authority is not modeled as a stakeholder agent (it is the authority_grounding in cs_structure).
 *
 * MANDATROPHY ANALYSIS:
 *   The Jerusalem pathway avoids mandatrophy by embedding its own sunset (eschatological fulfillment) and by centering the founding problem (how to rebuild without becoming empire) as a live, contested question — the prophetic office exists precisely to prevent the arrangement from becoming self-justifying. The mandate has not outlived its function because the founding problem (exile, fragmentation, imperial pressure) recurs in every generation. The constraint would become a piton if the liturgical/legal forms persisted without the participatory justice they were meant to enact — the theater_ratio measurements track this risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the jerusalem_reading a distinct constraint from its sibling readings, or a reinterpretation of the same constraint under different observables?',
    'Apply the ε-invariance test: if measuring ''human transcendence pathway'' via Jerusalem criteria (participatory, plural, gifted) yields ε ≈ 0.25 but measuring via Babel criteria (unified, efficient, self-made) yields ε ≈ 0.7+, they are distinct constraints. The historical record shows they produce different victim/beneficiary structures and different enforcement logics.',
    'If distinct, each reading gets its own constraint story with its own ε, stakeholders, and classification. If same constraint under different observables, the framework''s ε-invariance principle is violated and the kernel decomposition is mis-specified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s contested readings are structurally distinct constraints per ε-invariance.').

omega_variable(
    sacrifice_vs_extraction_boundary,
    'Does the ''sacrifice of efficiency for solidarity'' borne by participatory_builders constitute extraction (making this a tangled_rope) or voluntary formative cost (keeping it rope/scaffold)?',
    'Examine whether builders can exit the sacrificial role without losing their identity in the community. If exit_options for builders are ''constrained'' not ''trapped'' or ''identity_locked'', and if the community has mechanisms to rotate burdens (jubilee, shared priesthood), the sacrifice is participatory not extractive.',
    'If extractive, claimed_type should be tangled_rope (requires victims, which this reading denies). If formative, scaffold/rope stands. The engine''s directionality computation will test this via the builders'' exit_options and power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacrifice_vs_extraction_boundary, conceptual, 'Whether asymmetric cost-bearing in a participatory framework is extraction or formation.').

omega_variable(
    sunset_clause_operationalization,
    'What concrete historical or eschatological event constitutes the sunset of this scaffold constraint?',
    'Trace the tradition''s own eschatology: the messianic age (Jewish), the parousia (Christian), or the full realization of communio (Catholic social doctrine). If the sunset is purely eschatological with no historical proxies, the scaffold classification risks becoming a category error — a permanent structure claiming temporariness.',
    'If the sunset is only eschatological with no historical transition markers, the constraint may be a rope misclassified as scaffold. If there are historical transition points (e.g., the Church''s shift from Christendom to pilgrim church), scaffold stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_operationalization, empirical, 'Whether the scaffold''s sunset clause has historical operationalization or only eschatological reference.').

omega_variable(
    excluded_voice_coercion,
    'Does the structural exclusion of technocratic_proponents and babel_proponents constitute suppression, or is it the necessary boundary of a formative community?',
    'Assess whether excluded agents are prevented from forming their own communities elsewhere, or merely from imposing their logic on this community. The Jerusalem pathway claims the latter (it does not conquer Babylon); historical instantiations (Second Temple under empire, medieval Christendom) show mixed records.',
    'If exclusion operates as suppression of alternatives globally, suppression metric should be higher and claimed_type may shift toward tangled_rope. If exclusion is only boundary-maintenance for a voluntary community, suppression stays low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_voice_coercion, empirical, 'Whether the pathway''s boundary-maintenance against rival logics constitutes coercive suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(human_transcendence_pathway__jerusalem_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(htp_jerusalem_tr_t0, human_transcendence_pathway__jerusalem_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(htp_jerusalem_tr_t20, human_transcendence_pathway__jerusalem_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(htp_jerusalem_tr_t40, human_transcendence_pathway__jerusalem_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(htp_jerusalem_tr_t60, human_transcendence_pathway__jerusalem_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(htp_jerusalem_tr_t80, human_transcendence_pathway__jerusalem_reading, theater_ratio, 80, 0.2).
narrative_ontology:measurement(htp_jerusalem_tr_t100, human_transcendence_pathway__jerusalem_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(htp_jerusalem_be_t0, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(htp_jerusalem_be_t20, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(htp_jerusalem_be_t40, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(htp_jerusalem_be_t60, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 60, 0.25).
narrative_ontology:measurement(htp_jerusalem_be_t80, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 80, 0.24).
narrative_ontology:measurement(htp_jerusalem_be_t100, human_transcendence_pathway__jerusalem_reading, base_extractiveness, 100, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(htp_jerusalem_su_t0, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(htp_jerusalem_su_t20, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement(htp_jerusalem_su_t40, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(htp_jerusalem_su_t60, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(htp_jerusalem_su_t80, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 80, 0.18).
narrative_ontology:measurement(htp_jerusalem_su_t100, human_transcendence_pathway__jerusalem_reading, suppression_requirement, 100, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(human_transcendence_pathway__jerusalem_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(human_transcendence_pathway__jerusalem_reading, 0.08).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__babel_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, human_transcendence_pathway__technocratic_vs_incarnational_reading).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, catholic_social_doctrine__subsidiarity).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, catholic_social_doctrine__solidarity).
narrative_ontology:affects_constraint(human_transcendence_pathway__jerusalem_reading, political_theology__communio_ecclesiology).

% DUAL FORMULATION NOTE:
% This constraint is one member of the human_transcendence_pathway kernel family. The Jerusalem reading (this story) asserts low-extraction participatory rebuilding under divine gift. The Babel reading asserts high-extraction unified self-making. The Technocratic vs Incarnational reading frames the contest as a dichotomy. All three share the kernel but instantiate different constraints with different ε, beneficiaries, and types. This story's ε (0.25) differs from the Babel reading's expected ε (0.7+) because they describe different arrangements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
