% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Study Under Messianic Deferral (Readiness-Maintenance Reading)
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   This story instantiates the messianic_deferral reading of the
 *   kodashim_commandment_status kernel: the sacrificial commandments are
 *   temporally suspended by the Temple's absence but retain binding force,
 *   and sustained communal study of the sacrificial corpus maintains
 *   readiness for a future restoration. The standing arrangement under
 *   contest — the epsilon referent — is the actual deferral-study regime as
 *   this reading sees it: a centuries-old, now mass-institutionalized
 *   diversion of young men's prime decades, donor wealth, and communal budget
 *   share into the study of law that cannot presently be practiced, justified
 *   by a restoration whose timing is unknowable. Assessed by this reading's
 *   own lights, the arrangement is partly coordinative (transmission of an
 *   unperformable legal system across generations; communal continuity and
 *   hope) and partly costly to the present (deferred livelihoods,
 *   subordinated welfare needs), which is why the claimed type is
 *   tangled_rope while the metrics are authored independently from
 *   descriptive evidence. The sibling readings (performance_only,
 *   study_as_performance) are separate constraints in the same family, linked
 *   via network.affects_constraints; their structural deltas are recorded in
 *   the kernel-reading omega, not averaged into this file's numbers. KEY
 *   AGENTS (by structural relationship): - rabbinic_scholarly_class:
 *   agenda-setting beneficiary (institutional/identity_locked) — administers
 *   the study mandate and draws vocation, standing, and livelihood from it -
 *   yeshiva_institutions: collecting beneficiary (institutional/constrained)
 *   — receive the budgets and enrollments the mandate generates -
 *   extended_kollel_students: primary present-generation payer
 *   (moderate/identity_locked) — trade prime decades for stipends, status,
 *   and community - communal_welfare_dependents: silent payer
 *   (powerless/trapped) — absorb the residual after institutional commitments
 *   are met - lay_observant_community: dual-positioned member
 *   (organized/constrained) — draws identity and hope, pays tuition and
 *   opportunity costs - messianic_future_generation: absent party
 *   (powerless/trapped, non-agent) — the heirs in whose name readiness is
 *   claimed; cannot consent or object - comparative_religion_scholars:
 *   analytical observer (analytical/analytical) — traces the deferral economy
 *   from outside the tradition
 *
 * KEY AGENTS:
 *   - rabbinic_scholarly_class: agenda-setting beneficiary (institutional/identity_locked)
 *   - yeshiva_institutions: collecting beneficiary (institutional/constrained)
 *   - extended_kollel_students: primary present-generation payer (moderate/identity_locked)
 *   - communal_welfare_dependents: silent payer (powerless/trapped)
 *   - lay_observant_community: dual-positioned member (organized/constrained)
 *   - messianic_future_generation: absent non-agent party (powerless/trapped)
 *   - comparative_religion_scholars: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.58).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.48).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.58).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Study Under Messianic Deferral (Readiness-Maintenance Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic/commitment-system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '9f571f03-0107-4bb1-b01c-82b1dcf46fb4').
narrative_ontology:cs_kernel_codification('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', fixed_text).
narrative_ontology:cs_authority_grounding('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', lineage).
narrative_ontology:cs_interpretation_layer_present('9f571f03-0107-4bb1-b01c-82b1dcf46fb4').
narrative_ontology:cs_reading_relation('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', kodashim_commandment_status__study_as_performance, influences).
narrative_ontology:cs_axiom('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', foundational, sacrificial_obligation_survives_temple_destruction).
narrative_ontology:cs_axiom_status(sacrificial_obligation_survives_temple_destruction, holdable).
narrative_ontology:cs_axiom_grounding('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', sacrificial_obligation_survives_temple_destruction, deontological).
narrative_ontology:cs_axiom('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', foundational, study_maintains_restoration_readiness).
narrative_ontology:cs_axiom_status(study_maintains_restoration_readiness, holdable).
narrative_ontology:cs_axiom_grounding('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', study_maintains_restoration_readiness, instrumental).
narrative_ontology:cs_axiom('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', secondary, eschatological_preparation_outranks_present_welfare_priority).
narrative_ontology:cs_axiom_status(eschatological_preparation_outranks_present_welfare_priority, holdable).
narrative_ontology:cs_axiom_grounding('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', eschatological_preparation_outranks_present_welfare_priority, deontological).
narrative_ontology:cs_reference_frame('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', suspended_praxis_pending_restoration).
narrative_ontology:cs_drift_state('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', contemporary_mass_yeshiva_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f571f03-0107-4bb1-b01c-82b1dcf46fb4', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinic_scholarly_class).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, lay_observant_community).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, extended_kollel_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, communal_welfare_dependents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, extended_kollel_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, lay_observant_community).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, study_readiness_efficacy).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, mesorah_transmission_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide what counts as faithful engagement with the sacrificial corpus: they set curricula, certify teachers, and articulate the readiness justification from positions of recognized authority. Their vocation, standing, and livelihood are constituted by the study enterprise they administer; stepping outside it would mean relinquishing the identity and community that formed them. They do not personally bear the deferred-livelihood costs their students carry.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinic_scholarly_class, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, rabbinic_scholarly_class, beneficiary).

% Receive tuition, philanthropic donations, and state study subsidies; employ the teaching class and house enrolled students. Budgets scale with enrollment in the sacrificial-law tracks, giving them a durable stake in the mandate's continuation. Dissolution is the only real exit, so they compete for students and funding within the arrangement rather than against it.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, agenda_setter).

% Spend their twenties and often thirties studying material that cannot presently be practiced, supported by stipends, parents, and working spouses. They receive real goods — learning, status, community, structure — while deferring income, savings, pension formation, and occupational choice. Leaving means family disappointment, diminished marriage prospects for themselves and their siblings, and loss of their social world.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, extended_kollel_students, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, extended_kollel_students, beneficiary).

% Fund the institutions, host the students, and marry their children into the system. They draw identity, continuity, and messianic hope from the practice, and they pay for it: tuition burdens, daughters' earnings redirected to supporting learning, and communal funds routed to study halls rather than other needs. Exit means leaving the community, not merely changing an activity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, lay_observant_community, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, lay_observant_community, payer).

% Poor, elderly, and ill members whose needs compete with study institutions for the same charitable and municipal budgets. They receive what remains after institutional commitments are honored, and they have no seat where allocations are decided; dependence on communal services makes withholding support or relocating unrealistic.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, communal_welfare_dependents, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, communal_welfare_dependents, excluded).

% The descendants in whose name readiness is maintained. They do not yet exist: they cannot consent to the configuration of their inheritance, cannot confirm they will want it, and cannot object to what their forebears forgo on their behalf. Every justification spoken for them is spoken by others.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_future_generation, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(kodashim_commandment_status__messianic_deferral, messianic_future_generation).

% Study the deferral arrangement from outside the tradition, comparing how religious systems maintain unperformable practices across long suspensions. They bear none of its costs, receive none of its flows, and take no part in allocation decisions.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, yeshiva_institutions).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the complete procedural corpus of the sacrificial system — species, disqualifications, measurements, service orders — across generations in which it cannot be performed, so that a restored service could resume without re-derivation; secondarily, sustains communal boundary and eschatological hope through a shared, demanding curriculum.
% TRANSFER_FUNCTION: Moves the prime study-years of young men, donor wealth, and communal budget share from present-generation uses (livelihood formation, welfare provision, practical-halakhic education) to the institutional study apparatus and to an indefinitely deferred restoration scenario.
% ABSENT_VOICES: The future generation in whose name readiness is claimed cannot speak. Welfare-dependent households competing with study institutions for the same communal funds have no seat in allocation decisions. Students who doubt the restoration premise face family and marriage-market sanctions. Halakhic voices citing Maimonides' prohibition on making Torah a profession survive as minority citations without institutional standing.
% DISAPPEARANCE_RATIONALE: If the readiness-study mandate vanished overnight, yeshiva curricula would rebalance toward practicable law, thousands of kollel families would re-enter labor markets, institutional budgets would compress, and the community's eschatological posture would shift from preparation to waiting — the arrangement, not the underlying texts, organizes these flows.
% FOUNDING_PROBLEM: After 70 CE the sacrificial commandments became unperformable; the founding problem was preventing irreversible loss of the system's knowledge and preserving the covenantal possibility of restoration.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of rabbinic literature corroborate the founding problem's historical reality (the Yavneh-era canonization project as a response to destruction). On current status, the yeshiva establishment attests it remains live until restoration; critics inside the tradition cite completed codification and Maimonides' professional-study prohibition to argue the loss-prevention rationale is discharged; secular scholarship on yeshiva economics supports the shifted-function reading. Corroboration for the genealogy exists outside the benefiting parties; the status itself is disputed across seats, with no arbiter all parties accept.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58 (interval end): the arrangement's costs fall concretely on identifiable present parties (decades of deferred livelihood formation, welfare-competing budgets) while its justifying good is indefinitely deferred and unfalsifiable, but the study transmits a real legal corpus and confers real goods (learning, status, community), so the profile is moderately rather than severely extractive. Suppression is authored at 0.48 as a raw structural property — social and economic sanctions on exit, curriculum control, marriage-market alignment — and is deliberately NOT scaled by power or scope; only extractiveness is scaled, by the engine, from directionality and scope. Theater_ratio 0.32: most study is genuine intellectual engagement, but as restoration recedes a growing share becomes ritualized repetition whose readiness function is nominal. Accessibility_collapse 0.4: alternatives (practical-halakha emphasis, welfare priority, other tractates, Maimonides' own prohibition on making Torah a profession) remain articulable and partly exercised, so the arrangement competes with rather than annihilates alternatives. Resistance 0.35: recurring internal critique (welfare-priority arguments, religious-Zionist and Modern Orthodox objections to extended kollel) without majority traction. The three temporal series run on one shared grid (points 0/20/40/60/80/100, mapped to 1920-2020, the modern yeshiva-expansion era) so no metric borrows another's endpoints; trajectories are monotonic intensification, not cyclical, so no intermittent-reinforcement reading applies. Receipt surface: the gains demonstrably accrue to institutional budgets, hence gain_flow names yeshiva_institutions; fixing is prohibitive because redirecting the arrangement would impose concentrated, identity-laden costs on locked-in seats for diffusely distributed benefits. Scaffold was rejected: the deferral declares no sunset — its transition is precisely what never ends. Pure extraction was rejected: the transmission function is genuine and exits, while costly, are not sealed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setting seat (rabbinic_scholarly_class) should compute as coordinated-and-benefiting: from inside, the arrangement is fidelity itself, and its identity lock runs through vocation. The payer seats compute otherwise: extended_kollel_students share the administrators' identity lock but sit on the paying side of the same structure — identical exit atom, opposite directionality — while communal_welfare_dependents, powerless and trapped, experience the sharpest version of the arrangement with the least voice. Two institutional seats at equal formal power diverge structurally: the rabbinic class administers and is locked in; the institutions collect and could in principle dissolve. Lay members sit near symmetric, drawing hope and identity against tuition and foregone alternatives. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (rabbinic_scholarly_class, yeshiva_institutions, lay_observant_community) drive d toward the subsidized end for those seats; victim declarations (extended_kollel_students, communal_welfare_dependents) drive d toward the target end. Exit modulation then separates seats sharing a declaration: the students' identity_locked status (family, marriage market, community) pushes them toward the full-target end despite their secondary beneficiary goods (stipend, status, learning), whereas the lay community's constrained-but-real exit keeps it nearer symmetric. Welfare dependents combine the victim declaration with trapped exit and powerless standing — the highest-d seat in the story. The messianic_future_generation is authored as a non-agent precisely so that a party who cannot act does not feed the directionality arithmetic as if it collected; its presence is recorded as an absent voice instead. No directionality overrides were needed: the derivation from declarations plus exit options matches the structural relationships as this reading sees them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing irreversible loss of sacrificial-law knowledge after 70 CE — was structurally discharged by total textual codification (Mishnah, Talmud, and the medieval codifications), which is why founding_problem_status is authored 'contested' rather than 'live': the establishment attests ongoing necessity until restoration, while critics inside and outside the tradition cite completed codification. What persists is maintained by a successor justification (restoration contingency) rather than the founding problem — the classic shape of a mandate outliving its function without the arrangement dying. The tangled_rope classification prevents mislabeling in both directions: a pure-extraction reading would erase the genuine multi-century transmission achievement and the real goods students receive; a pure-coordination reading would erase the asymmetric opportunity cost borne by identifiable present parties. The forward risk is decay toward theatrical maintenance: if restoration belief weakened materially, the readiness rationale would hollow out and the arrangement would persist mainly as institutional inertia — the theater_ratio series is the designated early indicator, and the restoration_imminence_contingency omega records the trigger.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates one reading (messianic_deferral) of the kodashim_commandment_status kernel; what would adopting a sibling reading change structurally?',
    'Track which reading each community''s authorities adopt and re-run classification per reading: performance_only removes the present-normative-force premise (the victim set collapses toward zero, since nothing binds and no opportunity cost is wrongfully imposed); study_as_performance converts the study cost into the commanded act itself (present burden falls toward zero as cost becomes fulfillment).',
    'Under performance_only this arrangement loses its victim set and its burden claim entirely; under study_as_performance the same activity computes as near-benign occupation of the kernel. The moderate tangled_rope profile is specific to the deferral reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Sibling readings would restructure the beneficiary/victim sets and epsilon; this file authors only the deferral reading.').

omega_variable(
    restoration_imminence_contingency,
    'How durable is the restoration belief that alone justifies the readiness expenditure, and what happens to the arrangement''s justification if belief weakens across generations?',
    'Longitudinal survey data on eschatological belief within the study communities, plus enrollment elasticity in sacrificial-law tracks relative to alternative tracks.',
    'If belief decays faster than institutions adapt, the readiness rationale hollows out and the arrangement persists by inertia, shifting the computed profile toward theatrical maintenance; if belief is robust, the deferral justification stays live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_imminence_contingency, empirical, 'The entire justification hangs on an unfalsifiable future contingency.').

omega_variable(
    opportunity_cost_magnitude,
    'What fraction of communal resources and student lifetimes is genuinely diverted from present-welfare-valuable uses, net of the goods study itself confers?',
    'Communal budget analysis (charitable flows to study institutions versus welfare provision), time-use studies of kollel populations, and earnings trajectories of leavers versus stayers.',
    'A large net diversion supports the authored victim set and a heavier computed burden on the payer seats; a small net diversion would push the profile toward the coordination-dominated end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_magnitude, empirical, 'Size of the present-generation subsidy to the deferral arrangement.').

omega_variable(
    readiness_vs_identity_function_split,
    'Is the study''s dominant function actually readiness-maintenance (transmitting a usable legal system) or identity and boundary maintenance (sustaining communal distinctiveness and hope)?',
    'Curriculum analysis: does sacrificial-law study emphasize practically restorable procedure (species, measurements, service orders, disqualifications) or dialectical method? Compare retention of operative detail against identity-marking engagement.',
    'If identity dominates, the coordination-type declaration should shift and the readiness justification reads increasingly as cover, warranting decomposition into separate stories per the epsilon-invariance principle; if transmission dominates, the coordination claim is strong.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(readiness_vs_identity_function_split, conceptual, 'Which function would fail first if the arrangement stopped.').

omega_variable(
    exit_barrier_composition,
    'For kollel students, is the measured exit difficulty structural (stipend dependence, family economics, marriage-market position) or internalized (identity fusion, fear of communal judgment persisting after exit)?',
    'Post-exit trajectory studies: if sanction-like costs persist after students leave the system, the barrier is partly carried internally; if leavers integrate quickly, the barrier was structural.',
    'Internalized barriers raise the effective weight on the payer seat beyond what structural measures show and strengthen the identity-lock attribution; purely structural barriers would respond to economic remedies alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_barrier_composition, empirical, 'Structural versus internalized composition of student exit costs.').

omega_variable(
    future_generation_standing,
    'Can a not-yet-existing generation hold beneficiary standing such that present sacrifice is genuinely for them, or is the appeal to them a rhetorical device that launders present interests?',
    'Conceptual analysis within intergenerational ethics plus revealed-preference tests: do institutions actually optimize for restorability (preserving operative procedure) or for present institutional growth?',
    'Genuine standing strengthens the coordination half of the profile; rhetorical standing would reclassify the future-generation appeal as justification rather than benefit, raising the computed asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_generation_standing, conceptual, 'Standing of the absent party in whose name the deferral is justified.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_deferral_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(kodashim_deferral_tr_t0, observed).
narrative_ontology:measurement(kodashim_deferral_tr_t20, kodashim_commandment_status__messianic_deferral, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(kodashim_deferral_tr_t20, observed).
narrative_ontology:measurement(kodashim_deferral_tr_t40, kodashim_commandment_status__messianic_deferral, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(kodashim_deferral_tr_t40, observed).
narrative_ontology:measurement(kodashim_deferral_tr_t60, kodashim_commandment_status__messianic_deferral, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(kodashim_deferral_tr_t60, observed).
narrative_ontology:measurement(kodashim_deferral_tr_t80, kodashim_commandment_status__messianic_deferral, theater_ratio, 80, 0.3).
narrative_ontology:measurement_basis(kodashim_deferral_tr_t80, observed).
narrative_ontology:measurement(kodashim_deferral_tr_t100, kodashim_commandment_status__messianic_deferral, theater_ratio, 100, 0.32).
narrative_ontology:measurement_basis(kodashim_deferral_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(kodashim_deferral_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(kodashim_deferral_be_t0, observed).
narrative_ontology:measurement(kodashim_deferral_be_t20, kodashim_commandment_status__messianic_deferral, base_extractiveness, 20, 0.47).
narrative_ontology:measurement_basis(kodashim_deferral_be_t20, observed).
narrative_ontology:measurement(kodashim_deferral_be_t40, kodashim_commandment_status__messianic_deferral, base_extractiveness, 40, 0.51).
narrative_ontology:measurement_basis(kodashim_deferral_be_t40, observed).
narrative_ontology:measurement(kodashim_deferral_be_t60, kodashim_commandment_status__messianic_deferral, base_extractiveness, 60, 0.54).
narrative_ontology:measurement_basis(kodashim_deferral_be_t60, observed).
narrative_ontology:measurement(kodashim_deferral_be_t80, kodashim_commandment_status__messianic_deferral, base_extractiveness, 80, 0.56).
narrative_ontology:measurement_basis(kodashim_deferral_be_t80, observed).
narrative_ontology:measurement(kodashim_deferral_be_t100, kodashim_commandment_status__messianic_deferral, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(kodashim_deferral_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_deferral_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(kodashim_deferral_su_t0, observed).
narrative_ontology:measurement(kodashim_deferral_su_t20, kodashim_commandment_status__messianic_deferral, suppression_requirement, 20, 0.34).
narrative_ontology:measurement_basis(kodashim_deferral_su_t20, observed).
narrative_ontology:measurement(kodashim_deferral_su_t40, kodashim_commandment_status__messianic_deferral, suppression_requirement, 40, 0.38).
narrative_ontology:measurement_basis(kodashim_deferral_su_t40, observed).
narrative_ontology:measurement(kodashim_deferral_su_t60, kodashim_commandment_status__messianic_deferral, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(kodashim_deferral_su_t60, observed).
narrative_ontology:measurement(kodashim_deferral_su_t80, kodashim_commandment_status__messianic_deferral, suppression_requirement, 80, 0.45).
narrative_ontology:measurement_basis(kodashim_deferral_su_t80, observed).
narrative_ontology:measurement(kodashim_deferral_su_t100, kodashim_commandment_status__messianic_deferral, suppression_requirement, 100, 0.48).
narrative_ontology:measurement_basis(kodashim_deferral_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, performance_only).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'status of the sacrifice commandments after the destruction' decomposes into three structurally distinct claims with different epsilon. performance_only (minimal claim: nothing binds without an altar — near-zero present burden, no victim set) sits upstream; messianic_deferral (this file: obligation survives, study maintains readiness — moderate burden via opportunity cost, identifiable present victims) builds on the non-obsolete premise; study_as_performance (strongest claim: study itself fulfills the commandment — converts the study cost into the commanded act, collapsing the opportunity-cost objection) draws on the same transmitted corpus. Each is a separate file with its own epsilon, beneficiaries, and victims; the edges here record the family linkage required by the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
