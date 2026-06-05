% ============================================================================
% CONSTRAINT STORY: procedural_due_process__new_property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_procedural_due_process__new_property_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: procedural_due_process__new_property_reading
 *   human_readable: New Property Reading: Government Largesse as Modern Estate (Reich Doctrine)
 *   domain: constitutional_law/due_process_doctrine
 *
 * SUMMARY:
 *   This constraint represents ONE READING of the contested kernel of
 *   procedural due process doctrine: the 'new property' reading, instantiated
 *   in Charles Reich's seminal work arguing that government licenses,
 *   benefits, and public employment should be treated as 'property' for due
 *   process purposes because government largesse has become the functional
 *   equivalent of the medieval estate system. The reading suppresses the
 *   classical rights-privilege distinction (under which government benefits
 *   were mere gratuitous grants subject to removal at-will) and establishes
 *   that whenever the government becomes the primary allocator of livelihood
 *   resources, individuals acquire a protected interest in those resources
 *   and thus a right to process before deprivation. This reading coexists
 *   with two sibling readings: the Goldberg hearing-rights reading (which
 *   grounds the hearing requirement in the brutal need of the recipient) and
 *   the Mathews balancing reading (which treats due process as an
 *   optimization function, sizing the hearing to match error risk and
 *   administrative burden). The three readings are doctrinally compatible but
 *   rest on incompatible normative premises about WHY process is owed and
 *   what process should look like. This constraint story instantiates only
 *   the new property reading, treating it as a clean structural story with
 *   its own extractiveness, beneficiary/victim structure, and perspectival
 *   landscape.
 *
 * KEY AGENTS:
 *   - Statutory Entitlement Holders: Primary beneficiary (institutional/arbitrage) — benefits, licenses, and public employment now protected by hearing rights; the new property doctrine recognizes their interests
 *   - At-Will Discretionary State Power: Primary victim and beneficiary paradox — the state retains substantive allocation authority but loses speed and flexibility; the state's power is constrained procedurally but preserved substantively
 *   - Welfare Recipients / License Holders / Public Employees: Powerless/trapped — depend on state discretion for livelihood; the hearing right provides procedural armor but does not eliminate underlying dependence
 *   - Procedural Rights Institution: Institutional beneficiary (institutional/arbitrage) — courts and administrative law expand jurisdiction; the doctrine creates more cases and more authority
 *   - Administering Agency: Institutional actor (institutional/constrained) — loses discretionary speed but retains substantive control; must conduct hearings but can still deny/terminate on policy grounds
 *   - Rights-Privilege Distinction: Superseded doctrine (institutional/arbitrage) — the new property reading explicitly rejects the old doctrine; it persists as piton through institutional inertia
 *   - Due Process Reform Coalition: Organized actors (organized/mobile) — see the new property reading as a temporary scaffold; statutory entitlements are the permanent solution
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating state dependence as an immutable constraint when it is actually a contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(procedural_due_process__new_property_reading, 0.58).
domain_priors:suppression_score(procedural_due_process__new_property_reading, 0.62).
domain_priors:theater_ratio(procedural_due_process__new_property_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(procedural_due_process__new_property_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(procedural_due_process__new_property_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(procedural_due_process__new_property_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(procedural_due_process__new_property_reading, tangled_rope).
narrative_ontology:human_readable(procedural_due_process__new_property_reading, "New Property Reading: Government Largesse as Modern Estate (Reich Doctrine)").
narrative_ontology:topic_domain(procedural_due_process__new_property_reading, "constitutional_law/due_process_doctrine").

domain_priors:requires_active_enforcement(procedural_due_process__new_property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(procedural_due_process__new_property_reading, '6c5ab428-ca72-4e5e-b1e6-7aa489c120f1').
narrative_ontology:cs_kernel_codification('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', fixed_text).
narrative_ontology:cs_authority_grounding('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', extraction).
narrative_ontology:cs_interpretation_layer_present('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1').
narrative_ontology:cs_reading_relation('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', procedural_due_process__goldberg_hearing_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', procedural_due_process__mathews_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', foundational, government_largesse_as_functional_property).
narrative_ontology:cs_axiom_status(government_largesse_as_functional_property, holdable).
narrative_ontology:cs_axiom_grounding('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', government_largesse_as_functional_property, instrumental).
narrative_ontology:cs_axiom('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', foundational, procedural_constraint_on_discretion).
narrative_ontology:cs_axiom_status(procedural_constraint_on_discretion, holdable).
narrative_ontology:cs_axiom_grounding('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', procedural_constraint_on_discretion, deontological).
narrative_ontology:cs_reference_frame('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', government_discretionary_power_over_livelihood_allocation).
narrative_ontology:cs_drift_state('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', contemporary_statutory_entrenchment, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('6c5ab428-ca72-4e5e-b1e6-7aa489c120f1', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(procedural_due_process__new_property_reading, procedural_due_process).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(procedural_due_process__new_property_reading, statutory_entitlement_holders).
narrative_ontology:constraint_beneficiary(procedural_due_process__new_property_reading, procedural_rights_beneficiaries).
narrative_ontology:constraint_victim(procedural_due_process__new_property_reading, at_will_discretionary_state_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WELFARE RECIPIENT / AT-WILL TERMINATION (SNARE) — Before the new property reading, the recipient had no process right because benefits were treated as privilege/gratuitous largesse. Termination could occur without hearing, without notice, without cause. The reading recognizes the recipient's dependence on state benefits as a structural position that demands process protection. But the recognition comes AFTER the fact of dependence — the state has captured the recipient through benefits conditionality. The new property framing provides procedural armor (hearing rights) but does not eliminate the underlying extraction: the recipient remains dependent on state discretion for survival. Maximum experienced extraction because the procedural right, while real, arrives too late to prevent the fundamental vulnerability.
constraint_indexing:constraint_classification(procedural_due_process__new_property_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LICENSE HOLDER / PUBLIC EMPLOYEE (TANGLED ROPE) — Professional licenses (driver, medical, professional) and public employment create genuine coordination functions: the state licenses professionals to assure public safety; public employment coordinates collective services. But the same licenses and jobs create extraction: the state can revoke a license on discretionary grounds, terminate employment at-will, use the threat of revocation/termination to enforce compliance beyond statutory duty. The new property reading extends hearing rights to revocation — genuine coordination benefit (the licensing system) coupled with asymmetric extraction (discretionary termination power). Constrained exit: the license holder can ostensibly work elsewhere, but professional licensing is nationally coordinated; the public employee can ostensibly seek private employment, but government employment is often the only stable employer in the jurisdiction.
constraint_indexing:constraint_classification(procedural_due_process__new_property_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROCEDURAL RIGHTS INSTITUTION / HEARING REQUIREMENT (ROPE) — From the viewpoint of the due process infrastructure (courts, hearing officers, administrative law), the new property reading solves a pure coordination problem: it establishes a common frame for determining when a hearing is required. Before Reich, the privilege doctrine created chaos — some benefits got hearings, others did not, based on judge-made distinctions between property/privilege that lacked statutory warrant. The hearing requirement creates clarity and enables adjudication. The institution experiences this as coordination, not extraction. Net beneficiary because the doctrine expands the institution's jurisdiction and authority over administrative action. The extraction runs TOWARD this institution (more cases, more authority), not away from it.
constraint_indexing:constraint_classification(procedural_due_process__new_property_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADMINISTERING AGENCY / DISCRETIONARY STATE (TANGLED ROPE) — The welfare administration, licensing board, or public employer must now hold hearings before terminating benefits, licenses, or employment. This is a genuine cost: administrative burden, loss of speed and flexibility in removing individuals from the rolls. But the new property reading ALSO preserves the state's fundamental extraction mechanism: the state still decides WHO gets benefits, WHAT conditions attach to them, and HOW much the benefit is worth. The hearing is a procedural check, not a substantive one — the state can deny benefits for policy reasons; it just cannot terminate them without process. The constraint is tangled because the hearing requirement represents a real coordination function (fairness, rule of law) coupled with preservation of the state's fundamental power to allocate largesse (extraction mechanism). Constrained because the agency cannot exit the hearing requirement, but it retains control over substantive allocation.
constraint_indexing:constraint_classification(procedural_due_process__new_property_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RIGHTS-PRIVILEGE DISTINCTION / SUPERSEDED DOCTRINE (PITON) — The new property reading explicitly rejects the classical rights-privilege distinction: government benefits are no longer 'mere privileges' subject to removal at-will, but 'new property' deserving due process protection. This rejection kills the old doctrine's legitimacy as a legal rule. Yet the distinction persists in practice: many administrative terminations proceed without hearings; many entitlements are not recognized as 'property' for process purposes; the courts continue to draw subtle boundary lines between cognizable interests and bare expectancies. The doctrine has lost its intellectual foundation but remains in operation through institutional inertia and because the new property reading has not been fully implemented. Theater ratio reflects this: the courts perform the work of determining which interests constitute property (functional verification), but the underlying legitimacy principle (the rejected distinction) is no longer operative. The doctrine is degraded, maintained because alternatives have not fully replaced it.
constraint_indexing:constraint_classification(procedural_due_process__new_property_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DUE PROCESS REFORM COALITION / STATUTORY EXPANSION (SCAFFOLD) — Organized actors (legal aid organizations, civil rights groups, administrative law scholars) see the new property reading as a temporary institutional fix that is being progressively displaced by statutory entitlements. The hearing requirement solved the immediate problem (wholesale termination without process), but the lasting solution is statutory protections: Social Security Act amendments specifying hearing rights, civil service protections in public employment, professional licensing standards codified in state statute. As statutory entitlements become the primary protection mechanism, the reliance on judicially-recognized 'property' for process purposes diminishes. This perspective sees the new property reading as a scaffold: a temporary doctrinal platform that enabled transition from pure discretion to statutory entitlement, with an implicit sunset as legislatures enact specific procedural protections. Low extractiveness from this perspective because the organized agents see the exit path (statutory codification) and have the power to build it.
constraint_indexing:constraint_classification(procedural_due_process__new_property_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL DEPENDENCE (MOUNTAIN) — From a civilizational perspective, the tension between government discretion and individual livelihood is immutable: wherever the state distributes essential resources (welfare, licenses, employment), individuals become dependent on state action; wherever individuals depend on state discretion, the state has structural power to extract compliance or remove support. The new property reading acknowledges this structural reality — government largesse IS the modern equivalent of the medieval estate system — but cannot resolve the fundamental asymmetry. Procedural due process is a genuine protection (the hearing requirement is real), but it is a cosmetic fix on a structural dependence. From this view, the constraint appears as a natural law of state power: wherever the state becomes the primary allocator of livelihood, the state's discretionary power becomes a fundamental constraint on individual autonomy. However, this mountain classification is potentially a false summit: the 'inevitability' of state discretionary power over livelihood is itself a political choice, not a law of nature. Other systems (strong unions, cooperative provisioning, decentralized mutual aid) demonstrate that livelihood can be decoupled from state discretion.
constraint_indexing:constraint_classification(procedural_due_process__new_property_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(procedural_due_process__new_property_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(procedural_due_process__new_property_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(procedural_due_process__new_property_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(procedural_due_process__new_property_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(procedural_due_process__new_property_reading, TR),
    TR >= 0.70.

:- end_tests(procedural_due_process__new_property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58, moderate-high): The new property reading reduces extractiveness compared to the pure at-will regime (pre-Reich ε ≈ 0.72) by requiring hearings before termination. But extractiveness remains high because the hearing is procedural, not substantive — the state retains the power to terminate on policy grounds; the hearing merely ensures that termination is not arbitrary within its policy bounds. The measurement trajectory (0.72 → 0.65 → 0.58) reflects the gradual reduction in extractiveness as statutory entitlements replace judicial property recognition, but even at contemporary levels (0.58), extractiveness is well above the Rope ceiling (0.45) because the fundamental dependence persists. Suppression (0.62, moderate-high): Reflects barriers to exit (career professionals depend on licenses; welfare recipients depend on benefits; public employees depend on government salary) and the state's capacity to withhold resources. The trajectory (0.85 → 0.70 → 0.62) reflects diminishing suppression as statutory protections reduce the state's unilateral discretion. Theater ratio (0.45, moderate): The hearing requirement is functional (not merely performative) compared to post-hoc administrative review, but the doctrine still contains performative elements — boundary-drawing about which interests count as property, reversal rates that remain low despite hearing protections. Theater is lower than in purely discretionary systems because the hearing is a genuine process constraint.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single doctrinal reading generates six structurally distinct classifications depending on the observer's position. The welfare recipient sees extraction (Snare) because dependence on state benefits is structurally prior to any hearing right. The license holder sees mixed coordination and extraction (Tangled Rope) because licensing is both a coordination mechanism (public safety) and a discretionary power. The procedural rights institution sees pure coordination (Rope) because the doctrine expands their authority and enables predictable adjudication. The administering agency sees mixed coordination and constraint (Tangled Rope) because the hearing requirement is costly but the substantive allocation power is preserved. The rights-privilege distinction sees itself as degraded (Piton) because its intellectual foundation has been rejected but it persists in practice. The due process reform coalition sees a temporary fix with a sunset (Scaffold) as statutory entitlements gradually replace reliance on judicial property recognition. The civilizational analytical observer risks naturalizing the constraint as an immutable law (Mountain) when it is actually a contingent institutional choice. The perspectival gaps reveal that the new property reading does not resolve the fundamental tension between individual dependence and state discretion; it merely displaces and procedurally regulates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in the new property reading is determined by the agent's structural position relative to the hearing requirement and the underlying state discretion. Beneficiaries with statutory entitlements experience low d (beneficiary position) because the doctrine protects their interests. Victims experiencing at-will termination without process experience high d (target position) because the state's discretion falls on them. The administering agency experiences moderate d (split position) — it is constrained procedurally (must hold hearings) but benefits substantively (retains allocation power). The key insight is that the 'new property' doctrine doesn't eliminate extraction; it procedurally regulates it. A welfare recipient with a hearing right still experiences extraction through state dependence; the hearing merely ensures the extraction is not arbitrary. This is why even the beneficiary perspective (Perspective 2) classifies as Tangled Rope, not Rope — the coordination function (fair process) is genuine, but the underlying extraction (state dependence for livelihood) persists. The scaffold perspective sees the doctrine as temporary because statutory entitlements are progressively displacing reliance on judicial property recognition.
 *
 * MANDATROPHY ANALYSIS:
 *   The new property reading is a classic case of doctrine that attempts to resolve mandatrophy (the false binary: is government largesse coordination or extraction?) by reconceptualizing its fundamental category. The rights-privilege distinction assumed that benefits were mere privileges, categorically distinct from property, and thus not subject to due process constraints. This categorization allowed the state to terminate at-will (pure extraction, snare). The new property reading collapses the category: largesse IS property (for process purposes), and thus IS subject to due process. This recategorization creates a genuine Tangled Rope structure: the hearing requirement establishes a coordination function (fair process), while the underlying state discretion preserves an extraction mechanism (state dependence for livelihood). The mandatrophy is not resolved; it is acknowledged and then procedurally managed. The new property reading thus produces a structurally honest classification: Tangled Rope instead of either false Mountain (discretion as natural law) or false Rope (hearings as pure coordination without extraction). The measurement trajectory shows the gradual reduction in extractiveness as statutory entitlements replace reliance on the new property doctrine, suggesting that the true resolution of the mandatrophy lies beyond the new property reading itself, in the codification of entitlements in statute.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_definition_boundary,
    'What distinguishes a cognizable ''property'' interest deserving due process from a mere ''expectancy'' that remains purely discretionary?',
    'Doctrinal analysis of post-Reich case law identifying the criteria courts use to determine property status; comparison of statutory entitlements vs. benefits defined by administrative discretion',
    'Narrow property boundary: many government benefits remain unprotected, classification skews toward snare. Broad property boundary: more benefits receive hearing rights, classification shifts toward tangled rope. The boundary is the location of extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_definition_boundary, conceptual, 'Boundary between protected property and unprotected expectancy in due process doctrine').

omega_variable(
    hearing_effectiveness_vs_substantive_deprivation,
    'Does the hearing requirement actually prevent wrongful termination, or does it merely delay and proceduralize terminations that would occur anyway under substantive policy?',
    'Empirical study of outcomes post-hearing: reversal rates, reinstatement rates, average delay between termination and reinstatement; comparison of recipients who receive hearings vs. those who forgo them',
    'If hearings are effective (high reversal rates): the new property reading provides real protection, extractiveness lower. If hearings are procedural theater (low reversal rates, long delays): the doctrine is a snare disguised as rope, extractiveness higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hearing_effectiveness_vs_substantive_deprivation, empirical, 'Whether due process hearings provide substantive protection or procedural theater').

omega_variable(
    kernel_reading_contest__new_property_vs_goldberg_vs_mathews,
    'Is the new property reading logically compatible with the Goldberg hearing-rights reading and the Mathews balancing reading, or does one reading foreclose the others?',
    'Doctrinal analysis of the three readings'' foundational commitments: (1) new property = government largesse deserves process protection because it is functionally equivalent to estate/property; (2) Goldberg = brutal need of the recipient outweighs fiscal convenience, hearing is a duty of compassion; (3) Mathews = process is an optimization function, size the hearing to the error risk and burden. The three readings can coexist doctrinally (different courts emphasize different rationales), but they rest on incompatible normative premises about WHY process is owed.',
    'If readings coexist: all three remain live positions in doctrine, no foreclosure. If one reading forecloses another: the rejected reading becomes unstable and is eventually abandoned. Current doctrine uses all three rationales; the question is whether they can hold together or whether pressure will eventually eliminate some.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest__new_property_vs_goldberg_vs_mathews, conceptual, 'Logical compatibility of new property reading with sibling readings of procedural due process').

omega_variable(
    state_dependence_as_contingent_vs_structural,
    'Is the modern state''s role as primary allocator of livelihood (welfare, licensing, employment) a contingent institutional choice or a structural feature of developed economies?',
    'Comparative institutional analysis: societies with strong cooperatives, unions, or mutual aid systems that reduce state dependence; historical analysis of pre-welfare-state livelihood allocation; design exercises exploring decentralized alternatives',
    'If contingent: the state dependence is a political choice, not a law of nature. The mountain perspective is a false summit — the constraint could be restructured through institutional change. If structural: the state''s role is the inevitable form of complex modernity, and the extraction is unchangeable. The mountain perspective is justified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_dependence_as_contingent_vs_structural, conceptual, 'Whether state dependence for livelihood is inevitable or contingent').

omega_variable(
    false_summit_detection_new_property,
    'Does the new property reading naturalize a contingent doctrinal choice (the rights-privilege distinction was itself a constructed doctrine, now being reconstructed) as if it were inevitable?',
    'Historical analysis: the rights-privilege distinction was invented in the 19th century to protect government discretion; the new property reading invents a competing doctrine (property-in-government-largesse) to provide process protection. Both are constructed doctrines claiming natural-law status. The question is whether the new reading is truly a discovery of immutable structure or a deliberate reconstruction of legal doctrine.',
    'If construction: the new property reading is a political/doctrinal choice, not a law of nature. It can be unmade, refined, or replaced. The mountain perspective at the analytical level is a false summit. If natural law: the constraint is unchangeable, and the new property reading merely recognizes an existing structure. Determining this impacts assessment of whether the doctrine''s extractiveness can be reduced through further doctrinal evolution or statute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_detection_new_property, conceptual, 'Whether new property doctrine is a natural law or a constructed/reconstructed legal category').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(procedural_due_process__new_property_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(procdp_newprop_theater_t0_no_hearing, procedural_due_process__new_property_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(procdp_newprop_theater_t5_early_hearings, procedural_due_process__new_property_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(procdp_newprop_theater_t10_routinized_process, procedural_due_process__new_property_reading, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(procdp_newprop_extractiveness_t0_pre_reich, procedural_due_process__new_property_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(procdp_newprop_extractiveness_t5_post_goldberg, procedural_due_process__new_property_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(procdp_newprop_extractiveness_t10_contemporary, procedural_due_process__new_property_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(procdp_newprop_suppression_t0_pure_discretion, procedural_due_process__new_property_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(procdp_newprop_suppression_t5_hearing_requirement, procedural_due_process__new_property_reading, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(procdp_newprop_suppression_t10_statutory_entitlements, procedural_due_process__new_property_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(procedural_due_process__new_property_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(procedural_due_process__new_property_reading, procedural_due_process__goldberg_hearing_rights_reading).
narrative_ontology:affects_constraint(procedural_due_process__new_property_reading, procedural_due_process__mathews_balancing_reading).
narrative_ontology:affects_constraint(procedural_due_process__new_property_reading, government_dependence_and_autonomy).
narrative_ontology:affects_constraint(procedural_due_process__new_property_reading, administrative_discretion_constraint).

% DUAL FORMULATION NOTE:
% The new property reading is one reading of the procedural due process kernel; it coexists with the Goldberg and Mathews readings. The three readings have different ε values reflecting different empirical assessments of whether process protections are effective. This story treats the new property reading as a self-contained constraint with its own extractiveness (0.58), perspectives, and measurements. Siblings are documented in network.affects_constraints and in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(procedural_due_process__new_property_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
