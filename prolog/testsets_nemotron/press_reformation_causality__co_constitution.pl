% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print Economy–Religious Controversy Co-Constitution (Reformation)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The Reformation emerges from feedback loops between the print economy and
 *   religious controversy (1517-1648). Print technology functions as a
 *   scaffold: enabling infrastructure for vernacular dissemination that was
 *   meant to be transitional — once publics could access scripture directly,
 *   the coordination problem (authoritative text access) would be solved and
 *   the scaffold could sunset. Instead, the infrastructure was captured by
 *   confessional state-building: territorial princes used print regulation to
 *   stabilize religious identity, printers became dependent on confessional
 *   patronage, and the Church developed counter-printing apparatus. The
 *   constraint is claimed as Scaffold because the print infrastructure's
 *   enabling function (solving the access problem) is structurally distinct
 *   from the extraction patterns that layered onto it. Multiple tangled_rope
 *   dynamics exist: printers coordinate distribution but extract via
 *   confessional monopoly; reformers coordinate publics but extract via
 *   mandatory tithes and state enforcement; the Church coordinates orthodoxy
 *   but extracts via suppression and counter-printing costs. No single
 *   beneficiary captures the constraint — gain_flow is diffuse across
 *   printers, reformers, princes, and Church counter-apparatus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.42).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.38).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, scaffold).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print Economy–Religious Controversy Co-Constitution (Reformation)").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).
narrative_ontology:has_sunset_clause(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '47318def-bf24-4e2a-9dc9-31430c70de03').
narrative_ontology:cs_kernel_codification('47318def-bf24-4e2a-9dc9-31430c70de03', distributed).
narrative_ontology:cs_authority_grounding('47318def-bf24-4e2a-9dc9-31430c70de03', practice).
narrative_ontology:cs_interpretation_layer_present('47318def-bf24-4e2a-9dc9-31430c70de03').
narrative_ontology:cs_reading_relation('47318def-bf24-4e2a-9dc9-31430c70de03', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('47318def-bf24-4e2a-9dc9-31430c70de03', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('47318def-bf24-4e2a-9dc9-31430c70de03', foundational, technology_and_agency_co_constitute_historical_outcomes).
narrative_ontology:cs_axiom_status(technology_and_agency_co_constitute_historical_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('47318def-bf24-4e2a-9dc9-31430c70de03', technology_and_agency_co_constitute_historical_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('47318def-bf24-4e2a-9dc9-31430c70de03', foundational, print_infrastructure_has_transitional_enabling_function).
narrative_ontology:cs_axiom_status(print_infrastructure_has_transitional_enabling_function, holdable).
narrative_ontology:cs_axiom_grounding('47318def-bf24-4e2a-9dc9-31430c70de03', print_infrastructure_has_transitional_enabling_function, empirically_contingent).
narrative_ontology:cs_reference_frame('47318def-bf24-4e2a-9dc9-31430c70de03', vernacular_access_problem_unsolved).
narrative_ontology:cs_drift_state('47318def-bf24-4e2a-9dc9-31430c70de03', post_westphalia_1648, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('47318def-bf24-4e2a-9dc9-31430c70de03', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reform_movements).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, literate_lay_publics).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_institutional_authority).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, latin_literary_monopoly_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, vernacular_printers).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, vernacular_access_to_scripture).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, public_sphere_formation_through_print).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, institutional_adaptation_pressure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Print and distribute vernacular religious texts (pamphlets, bibles, polemics) for profit. Gain coordination benefit: print solves the distribution problem for religious controversy. Bear extraction: confessional licensing fees, censorship risk, dependence on territorial patronage. Exit is constrained — moving cities risks losing guild privileges and confessional market access.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, vernacular_printers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, vernacular_printers, payer).

% Use print to coordinate publics, standardize doctrine, and challenge Latin unity. Gain massive coordination benefit: print enables movement formation at scale. Bear extraction: resources diverted to printing, dependence on printer networks, vulnerability to censorship. Exit is identity_locked — the movement's identity is constituted through print controversy; abandoning print means abandoning the Reformation itself.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reform_movements, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reform_movements, agenda_setter).

% Gain access to vernacular scripture, devotional texts, and religious debate. Bear indirect extraction: higher prices for confessional monopoly texts, conformity pressure from confessional state. Exit is constrained — confessional boundaries (cuius regio, eius religio) make religious mobility costly; literacy itself ties them to the print ecosystem.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, literate_lay_publics, beneficiary,
    moderate, biographical, constrained, regional).

% Loses Latin liturgical monopoly and doctrinal control. Bears extraction: counter-printing costs, Index maintenance, Council of Trent enforcement apparatus, territory losses to Protestant princes. Exit is identity_locked — the Church's self-conception is fused to Latin unity and magisterial authority; adapting to vernacular plurality is experienced as institutional death.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_institutional_authority, payer,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, catholic_institutional_authority, agenda_setter).

% Scholastic publishers, Latin humanist networks, university text suppliers displaced by vernacular market. Bear full extraction with no coordination benefit — their product (Latin texts) loses the coordination function print once gave it. Exit is trapped — retraining for vernacular markets is blocked by confessional polarization and guild structures.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, latin_literary_monopoly_holders, payer,
    organized, biographical, trapped, continental).

% Capture print regulation (licensing, censorship, monopoly grants) to stabilize confessional identity and extract revenue. Gain both coordination (religious uniformity aids governance) and extraction (print fees, church land secularization). Exit is arbitrage — they can shift confessional allegiance (as some did) to maximize advantage; the constraint serves their interests.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, territorial_princes, agenda_setter,
    powerful, generational, arbitrage, national).

% Anabaptists, spiritualists, anti-trinitarians use print but are suppressed by both Catholic and magisterial Protestant authorities. They would object to the confessional capture of print; their exclusion is enforced by the same print licensing that stabilizes the major confessions. Exit is trapped — no territorial protector, printing is illegal for them.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, radical_reformation_groups, excluded,
    powerless, biographical, trapped, regional).

% Analyzes the feedback loop from outside the constraint. Sees the scaffold function (vernacular access enabled), the tangled_rope dynamics (confessional capture), and the distributed extraction (no single capturer). No material stake; exit is analytical.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, historian_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Print technology solved the coordination problem of authoritative textual access: vernacular scripture and religious debate could reach publics beyond manuscript networks, enabling mass religious movements and public opinion formation.
% TRANSFER_FUNCTION: Moves textual authority from Latin clerical monopoly to vernacular public sphere. Printers capture revenue from polemical and devotional printing. Reformers capture tithes, patronage, and state support enabled by print publics. Princes capture church lands and regulatory rents. Church bears counter-printing and enforcement costs. No single capturer — flows are distributed across confessional, commercial, and state channels.
% ABSENT_VOICES: Radical reformation groups (Anabaptists, spiritualists) and non-confessional humanist networks were structurally excluded from the print licensing regime. They would argue for open printing and religious liberty but were suppressed by both major confessions. Peasant and urban poor publics (largely illiterate) were excluded from the vernacular public sphere itself — their voices enter only through rebellion (1525 Peasants' War) which the constraint's beneficiaries crushed.
% DISAPPEARANCE_RATIONALE: If the print-religion feedback loop vanished in 1517: no mass pamphlet circulation, no vernacular bible standardization, no coordinated Reformation publics, no confessional state-building via print regulation. The Reformation as a mass movement fails; the Catholic Church retains Latin unity longer; the public sphere develops differently (perhaps through manuscript academies or oral networks). The world rearranges profoundly.
% FOUNDING_PROBLEM: The Latin liturgical monopoly blocked lay access to scripture and religious debate; manuscript production could not scale to meet demand for vernacular texts; religious controversy was confined to clerical circles. Print infrastructure was built to solve the access problem — enabling vernacular dissemination at scale.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (access to scripture) is attested as solved by 1555: vernacular bibles existed in German, French, English, Dutch; literacy rates rose; public religious debate was normalized. Corroboration from outside beneficiaries: Catholic historians (Jedin, Hubert) acknowledge the access problem was real and substantially solved; secular historians (Eisenstein, Febvre) document the print infrastructure's self-sustaining status post-1648. The arrangement persists (print regulation, confessional censorship) despite the founding problem's resolution — mandatrophy confirmed.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).
:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects layered rents: printer monopolies, confessional tithe systems, state censorship apparatus — but distributed, not concentrated. Suppression (0.38) is moderate: censorship, index librorum prohibitorum, territorial print licensing exist but alternatives (smuggling, manuscript circulation, oral preaching) persist. Theater ratio (0.28) captures performative orthodoxy enforcement vs. genuine coordination of textual access. Accessibility collapse (0.45) is partial: Latin monopoly breaks but confessional canon replacements create new barriers. Resistance (0.55) is high: underground printing, radical reformation, Catholic resistance, peasant revolts all contest the arrangement. The scaffold claim rests on the enabling function (vernacular access) having a declared sunset — the confessional settlements (Augsburg 1555, Westphalia 1648) as structural sunset points where the infrastructure becomes self-sustaining.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute different seat types: from the printer's seat, the constraint is Tangled Rope (genuine coordination + confessional extraction). From the lay public's seat, it is Rope (coordination benefit > cost). From the Catholic authority's seat, it is Snare (extraction without coordination benefit). From the reformer's seat, it is Scaffold (transitional enabling function). The divergence is the measurement — the co_constitution reading insists no single seat owns the truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Vernacular printers are beneficiaries (coordination function: they enable distribution and profit) but also payers (confessional licensing, censorship risk) — dual position yields d ~0.45. Reform movements are beneficiaries (print enables public formation) and agenda_setters (they direct the controversy) — d ~0.25. Literate lay publics are beneficiaries (access) with constrained exit (confessional boundaries) — d ~0.4. Catholic institutional authority is payer (loses monopoly, bears counter-printing costs) with identity_locked exit (institutional identity fused to Latin unity) — d ~0.85. Latin literary monopoly holders are payers (displaced) with trapped exit — d ~0.9. Territorial princes (implicit in stakeholders) are agenda_setters capturing regulatory rent — d ~0.2.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (vernacular scripture access) was live in 1517. By 1555/1648, the problem is substantially solved (vernacular bibles exist, literacy spreads, public sphere forms) but the print regulation apparatus persists and intensifies — confessional censorship, state licensing, monopoly grants. This is mandatrophy: the arrangement outlives its founding function. The scaffold's sunset clause (confessional settlement) was either not honored or was reinterpreted as permanent regulatory authority. The constraint avoids pure Snare classification because the coordination function (textual access) remains real and the extraction is distributed, not captured by one party.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the co_constitution reading of the press_reformation_causality kernel structurally distinct from its sibling readings (technological_determinism, strategic_deployment), or do they describe the same constraint from different angles?',
    'Test epsilon-invariance: measure extractiveness, suppression, and beneficiary structure under each reading. If epsilon differs materially (e.g., technological_determinism yields near-zero extraction as Mountain; strategic_deployment yields concentrated extraction as Snare/Tangled Rope), the readings instantiate different constraints.',
    'If readings map to different epsilon values, they are distinct constraints linked by network.affects_constraints, not one constraint with observer-dependent classification. Confirms the kernel decomposition protocol.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s declared readings decompose into separate constraint stories per epsilon-invariance.').

omega_variable(
    scaffold_sunset_location,
    'Where is the sunset clause for the scaffold function of print technology? Does the enabling infrastructure role terminate at a specific historical moment (e.g., confessionalization, state-church settlement), or does it persist as a permanent coordination layer?',
    'Trace the trajectory of print''s coordination function: from enabling vernacular dissemination (1520s) to stabilizing confessional identities (1550s-1648) to becoming the default public sphere substrate (post-1648). If the coordination problem (access to authoritative text) is solved and the infrastructure becomes self-sustaining without active enforcement, the scaffold has sunset.',
    'If sunset occurred at confessionalization (Peace of Augsburg 1555, Westphalia 1648), the constraint reclassifies from Scaffold to Rope or Mountain for later periods. If no sunset, the scaffold claim is a false summit masking persistent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_location, empirical, 'Historical location of the scaffold''s sunset — when the enabling infrastructure became self-sustaining or was captured.').

omega_variable(
    distributed_extraction_measurement,
    'Can distributed extraction patterns among printers, reformers, and Church be measured without collapsing into a single beneficiary category?',
    'Reconstruct revenue flows: printer profits from polemical pamphlets vs. liturgical texts; reformer resource capture (tithes, patronage, state support) enabled by print; Church counter-printing costs and indulgence revenue protection. If no single agent captures >50% of the constraint''s gains, gain_flow = ''diffuse'' is warranted.',
    'Validates the claimed ''no single beneficiary'' structure. If a dominant capturer emerges (e.g., territorial princes capturing both print regulation and church lands), reclassifies toward Snare/Tangled Rope with named beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_extraction_measurement, empirical, 'Whether extraction is genuinely distributed or covertly concentrated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__co_constitution, theater_ratio, 1517, 0.12).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causality__co_constitution, theater_ratio, 1525, 0.22).
narrative_ontology:measurement(pres_tr_t1535, press_reformation_causality__co_constitution, theater_ratio, 1535, 0.3).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__co_constitution, theater_ratio, 1555, 0.28).
narrative_ontology:measurement(pres_tr_t1618, press_reformation_causality__co_constitution, theater_ratio, 1618, 0.25).
narrative_ontology:measurement(pres_tr_t1648, press_reformation_causality__co_constitution, theater_ratio, 1648, 0.22).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__co_constitution, base_extractiveness, 1517, 0.18).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causality__co_constitution, base_extractiveness, 1525, 0.32).
narrative_ontology:measurement(pres_be_t1535, press_reformation_causality__co_constitution, base_extractiveness, 1535, 0.45).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__co_constitution, base_extractiveness, 1555, 0.42).
narrative_ontology:measurement(pres_be_t1618, press_reformation_causality__co_constitution, base_extractiveness, 1618, 0.38).
narrative_ontology:measurement(pres_be_t1648, press_reformation_causality__co_constitution, base_extractiveness, 1648, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__co_constitution, suppression_requirement, 1517, 0.15).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causality__co_constitution, suppression_requirement, 1525, 0.35).
narrative_ontology:measurement(pres_su_t1535, press_reformation_causality__co_constitution, suppression_requirement, 1535, 0.5).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causality__co_constitution, suppression_requirement, 1555, 0.42).
narrative_ontology:measurement(pres_su_t1618, press_reformation_causality__co_constitution, suppression_requirement, 1618, 0.38).
narrative_ontology:measurement(pres_su_t1648, press_reformation_causality__co_constitution, suppression_requirement, 1648, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__co_constitution, 0.03).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, confessional_print_regulation).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, vernacular_public_sphere_formation).

% DUAL FORMULATION NOTE:
% This constraint is one member of the press_reformation_causality kernel family. technological_determinism treats print as Mountain (natural law enabler, epsilon ~0). strategic_deployment treats print as Snare/Tangled Rope with concentrated beneficiary capture (epsilon ~0.65). co_constitution treats print as Scaffold with distributed tangled_rope dynamics (epsilon 0.42). The three readings have different epsilon values because they describe structurally distinct constraints: the Mountain reading describes the physical press; the Snare reading describes the strategic capture; the Scaffold reading describes the co-constituted feedback loop. They are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__co_constitution, institutional, 0.85).
constraint_indexing:directionality_override(press_reformation_causality__co_constitution, organized, 0.4).
constraint_indexing:directionality_override(press_reformation_causality__co_constitution, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
