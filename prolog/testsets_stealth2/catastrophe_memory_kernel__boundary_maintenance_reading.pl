% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__boundary_maintenance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__boundary_maintenance_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__boundary_maintenance_reading
 *   human_readable: Shared Mourning Practice as Group-Boundary Enforcement (Boundary-Maintenance Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A catastrophe-survivor diaspora community maintains an annual mourning
 *   cycle: appointed fast and memorial days, lament liturgy, mandated
 *   communal gatherings, and a standing apparatus for deciding who mourns,
 *   how, and with whom. Under the boundary-maintenance reading instantiated
 *   here, the practice's operative function is drawing and policing the line
 *   between member and non-member: participation is the recurring membership
 *   test, grief expression is standardized, and outsiders and partial
 *   insiders (the intermarried, the secular) are structurally held at the
 *   perimeter. Costs fall on individual autonomy (prescribed grief,
 *   conformity surveillance) and on out-group relations (exclusion from the
 *   circle of shared mourning); the return is in-group cohesion. Per the
 *   epsilon-invariance principle, the colloquial label 'catastrophe mourning
 *   ritual' decomposes into a four-story family — boundary maintenance (this
 *   file), symbolic continuity, survival-competence transmission, and trauma
 *   encoding — each with its own epsilon, victim set, and classification;
 *   this file authors ONLY the boundary-maintenance arrangement as this
 *   reading assesses it, with siblings linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   communal_leadership: agenda-setter (institutional/identity_locked) — sets
 *   the memorial calendar, prescribes liturgy, adjudicates standing -
 *   observant_core_members: primary beneficiary (organized/identity_locked) —
 *   full participants whose belonging the cycle confirms; supply the
 *   sanctioning majority - marginal_members: primary target
 *   (moderate/constrained) — secular and doubting members under conformity
 *   pressure - intermarried_households: target (moderate/constrained) —
 *   households re-tested at every lifecycle event - rising_generation_youth:
 *   target with secondary benefit (moderate/mobile) — inherit the obligation
 *   before consenting; principal source of attrition - out_group_neighbors:
 *   target (powerful/mobile) — host-society members kept outside the circle
 *   of shared grief - secularized_descendants: excluded voice
 *   (moderate/mobile) — left the community; would contest the
 *   boundary-policing but are out of the conversation - memory_researchers:
 *   analytical observer (analytical/analytical) — study the practice from
 *   outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_kernel__boundary_maintenance_reading, 0.52).
domain_priors:theater_ratio(catastrophe_memory_kernel__boundary_maintenance_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__boundary_maintenance_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__boundary_maintenance_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__boundary_maintenance_reading, "Shared Mourning Practice as Group-Boundary Enforcement (Boundary-Maintenance Reading)").
narrative_ontology:topic_domain(catastrophe_memory_kernel__boundary_maintenance_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__boundary_maintenance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__boundary_maintenance_reading, 'b0bfc292-763a-4719-931d-689b486419f0').
narrative_ontology:cs_kernel_codification('b0bfc292-763a-4719-931d-689b486419f0', fixed_text).
narrative_ontology:cs_authority_grounding('b0bfc292-763a-4719-931d-689b486419f0', lineage).
narrative_ontology:cs_interpretation_layer_present('b0bfc292-763a-4719-931d-689b486419f0').
narrative_ontology:cs_reading_relation('b0bfc292-763a-4719-931d-689b486419f0', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0bfc292-763a-4719-931d-689b486419f0', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('b0bfc292-763a-4719-931d-689b486419f0', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('b0bfc292-763a-4719-931d-689b486419f0', foundational, boundary_firmness_preserves_the_group).
narrative_ontology:cs_axiom_status(boundary_firmness_preserves_the_group, holdable).
narrative_ontology:cs_axiom_grounding('b0bfc292-763a-4719-931d-689b486419f0', boundary_firmness_preserves_the_group, empirically_contingent).
narrative_ontology:cs_axiom('b0bfc292-763a-4719-931d-689b486419f0', secondary, belonging_requires_prescribed_mourning).
narrative_ontology:cs_axiom_status(belonging_requires_prescribed_mourning, holdable).
narrative_ontology:cs_axiom_grounding('b0bfc292-763a-4719-931d-689b486419f0', belonging_requires_prescribed_mourning, conventional).
narrative_ontology:cs_reference_frame('b0bfc292-763a-4719-931d-689b486419f0', mourning_as_boundary_institution).
narrative_ontology:cs_drift_state('b0bfc292-763a-4719-931d-689b486419f0', third_generation_present, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0bfc292-763a-4719-931d-689b486419f0', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, observant_core_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, marginal_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_households).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_neighbors).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__boundary_maintenance_reading, rising_generation_youth).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__boundary_maintenance_reading, rising_generation_youth).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__boundary_maintenance_reading, communal_survival_through_distinctiveness).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__boundary_maintenance_reading, grief_as_membership_proof).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbis, cantors, and lay officers who set the memorial calendar, select the lament liturgy, and decide each year who is called to lead prayer and who counts as a mourner in good standing. They train successors, answer to senior members, and stake their standing on the community's continuity; stepping outside the inherited forms would cost them their position. Leaving the role would mean abandoning the vocation and community that constitute their working life.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, communal_leadership, agenda_setter,
    institutional, generational, identity_locked, regional).

% Longstanding member families who attend every memorial observance, fast the appointed fasts, and staff the committees that run them. The yearly cycle confirms their place in the chain of remembrance and organizes their friendships, marriages, and charitable giving. Opting out would mean forfeiting the web of regard and mutual obligation their lives are built on.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, observant_core_members, beneficiary,
    organized, generational, identity_locked, regional).

% Secular, doubting, and culturally-only members who attend the major memorial dates but observe loosely the rest of the year. They feel the gap between their inner life and the prescribed script: grief is supposed to arrive on schedule and in approved form. Skipping or improvising draws censure, cooler greetings, and thinner marriage and business networks; drifting away entirely means losing childhood friends and family gatherings.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, marginal_members, payer,
    moderate, biographical, constrained, regional).

% Households with a non-member spouse or parent. Every lifecycle event runs them through the membership question anew: where the non-member may sit, whether the children count as mourners, who may be named in prayer. They keep one foot in each world and absorb friction from both sides; full exit would cut them off from aging parents and burial plots, and full inclusion is not on offer.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, intermarried_households, payer,
    moderate, biographical, constrained, regional).

% Host-society residents, colleagues, civic partners, and interfaith contacts. During memorial seasons the community turns inward: joint events are declined, explanations are brief, and the neighbors register a polite distance they did not choose. Most shrug and continue their week; a few who sought deeper partnership find the door opens only partway.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, out_group_neighbors, payer,
    powerful, biographical, mobile, national).

% Teenagers and young adults born decades after the catastrophe, who inherit the duty to mourn events they never witnessed. Some find in the calendar an anchor and a ready-made answer to who they are; others experience it as a claim on their attention made before they could consent. They vote with their feet more easily than their elders did — university, intermarriage, and geographic mobility are all real options.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, rising_generation_youth, payer,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__boundary_maintenance_reading, rising_generation_youth, beneficiary).

% People raised inside the community who left for secular lives. They remember the scripts and could say precisely which parts of the yearly cycle feel like devotion and which feel like attendance-taking, but they are no longer consulted on how commemoration is designed; their critique arrives only as absence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, secularized_descendants, excluded,
    moderate, biographical, mobile, national).

% Historians, sociologists, and anthropologists of collective memory who attend commemorations, interview participants, and compare communities. They hold no stake in the calendar's continuation and publish what they find, including findings the community's leadership would rather not see framed that way.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__boundary_maintenance_reading, memory_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__boundary_maintenance_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__boundary_maintenance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains solidarity and shared identity in a dispersed, catastrophe-marked minority: synchronized mourning gives scattered members a common emotional calendar, a recurring occasion for assembly, and a visible, repeatable demonstration of continued belonging.
% TRANSFER_FUNCTION: Moves conformity labor, grief-expression time, and relational openness from individual members (disproportionately marginal, intermarried, and young ones) into boundary upkeep for the collective; moves social standing and belonging back to compliant participants; and withholds the circle of shared grief from out-group members.
% ABSENT_VOICES: Secularized descendants and out-group members are not in the room where commemoration formats are decided; intermarried parents rarely sit on ritual committees; dissenting youth voice surfaces mostly as quiet attrition rather than argument. The consensus that the practice 'works' is reached largely among those the calendar serves.
% DISAPPEARANCE_RATIONALE: If the boundary-enforcing mourning practice vanished overnight, the community's membership criteria, calendar, and internal policing would rearrange: assimilation dynamics would accelerate, lifecycle events would lose their sorting function, and the community would reorganize around other markers (dietary practice, language, philanthropy) or dissolve markedly faster — the parties' arrangements visibly depend on it.
% FOUNDING_PROBLEM: After catastrophe destroyed territory, institutions, and much of the population, a scattered survivor community needed to prevent dissolution into host societies — to keep a dispersed, traumatized minority coherent across generations when ordinary markers of nationhood were unavailable. The mourning cycle was built as a portable, repeatable act of collective self-demarcation.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem was real is corroborated from outside the benefiting parties: catastrophe historiography documents the destruction and dispersal, and demographic studies of assimilation document the dissolution pressure. That the problem REMAINS live in its original form is attested mainly by the community's own leadership; outside scholarship broadly treats the acute survival phase as substantially past, and no external body currently attests continuing acuity — that asymmetry is itself signal.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__boundary_maintenance_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__boundary_maintenance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__boundary_maintenance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__boundary_maintenance_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__boundary_maintenance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__boundary_maintenance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58: the arrangement transfers real goods — grief-time, expressive autonomy, relational openness — from individuals (disproportionately those who never consented to the obligation) into collective boundary upkeep, but the transfer is bounded by solidarity returns that participants demonstrably receive. Suppression is 0.52: enforcement runs on social sanction (censure, cooled networks, marriage-market and burial consequences) layered over internalized obligation, with no physical coercion; it is authored as a raw structural property and is deliberately NOT scaled here — directionality and scope scaling happen engine-side. Theater_ratio is 0.28: a growing share of later-generation observance is rote performance, but the core functions (assembly, synchronized grief, mutual-aid mobilization on memorial occasions) are still delivered. Accessibility_collapse is 0.42: alternatives (private commemoration, other communities, exit) remain visible and legible but carry severance costs that keep them from being live options for most. Resistance is 0.48: secular drift, intermarriage, quiet attrition, and periodic reform movements meet the enforcement apparatus continuously. The three measurement series share one grid ({0,15,30,45,60,75}) so every metric is authored at every examined time point. Base extractiveness rises across the interval because the felt connection to the catastrophe thins generationally while the obligation persists — the cost migrates from consenting survivors to unconsenting inheritors. Theater rises with rote performance. Suppression_requirement is authored because enforcement capacity genuinely changed over the interval: explicit communal sanction decayed as observance internalized (0.65 down to 0.49), then stabilized as a steady-state policing level against rising attrition (0.51-0.52) — an enforcement-decay-then-restabilization trajectory, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and core-beneficiary seats should compute the arrangement as a load-bearing institution — their identity, authority, and social world rest on it — while the marginal and intermarried seats compute enforced extraction from the same structure. Same-level lateral differentiation matters: marginal members and intermarried households hold comparable nominal standing, but the intermarried face the membership test at every lifecycle event (weddings, namings, burials), which makes their exit costlier and their exposure stickier. The out-group seat is declared a victim yet holds powerful/mobile position — its effective burden is heavily damped by trivially available exit, unlike the constrained in-group targets. The youth seat splits internally between inherited burden and received anchor. Coalition note: marginal members could in principle convert diffuse discontent into reform majorities, but the ritual calendar monopolizes communal assembly, depriving them of organizing occasions — a structural feature worth weighting in any coalition-power assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: observant_core_members and communal_leadership sit near the beneficiary end (low d) — the cycle subsidizes their belonging and authority. Victim declarations drive the target end: marginal_members and intermarried_households derive high d (constrained exit traps them near full-target exposure); rising_generation_youth derive high structural targeting damped by mobile exit; out_group_neighbors are structurally targeted (excluded from the grief-circle by design) but their powerful/mobile position damps effective extraction sharply — they bear the relation-cost almost optionally. memory_researchers hold the analytical seat. No directionality overrides were authored: the beneficiary/victim declarations plus exit atoms capture the structure, and the mobile-exit damping of the out-group seat is exactly what the derivation should express.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards in both directions. Against pure-snare misreading: the coordination function is real and currently delivered — synchronized mourning genuinely assembles a dispersed community, mobilizes mutual aid on memorial occasions, and processes grief collectively — so the presence of victims does not make this a snare. Against rope misreading: the costs are asymmetric and identifiable (unconsenting inheritors, the intermarried, the excluded out-group), and enforcement machinery actively holds the boundary, so this is not voluntary coordination. On obsolescence: the founding problem's status is contested rather than dead — the acute survival emergency has receded (external historiography and demography corroborate the founding conditions were real) while leadership attests continued acuity — so mandatrophy is NOT declared resolved; the solidarity-delivery function remains demonstrably operative, and theater_ratio is tracked precisely because rote observance is the leading indicator of mandate decay. The receipt surface records facts, not classifications: gain_flow is authored 'diffuse' as an affirmative finding after checking every seat — cohesion gains are held jointly by participants and no seat pockets the extracted conformity labor; fixing_cost is 'prohibitive' because relaxing the boundary function risks dissolving the solidarity the practice delivers, an active-coordination cost rather than inertial neglect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which of the four readings of the catastrophe_memory_kernel tracks the mourning practice''s operative function — is boundary enforcement the live mechanism, or a retrospective gloss on a practice sustained for other reasons?',
    'Compare what the community actually sanctions (missed attendance, intermarriage, liturgical deviation) against what it invests in (survival-skills teaching, archive maintenance, trauma education); the sanctioned category reveals the operative function.',
    'If sanctioning clusters elsewhere, this reading''s epsilon overstates extraction and the boundary-maintenance constraint misdescribes the arrangement; each sibling reading carries its own epsilon and would take precedence as the accurate instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Four readings of one kernel; this file authors only the boundary-maintenance instantiation.').

omega_variable(
    consent_distribution_across_generations,
    'What share of the conformity cost falls on members who consent to the mourning obligation versus inheritors who never accepted it?',
    'Cohort-stratified surveys of participation motivation and regret across first-, second-, and third-generation members.',
    'If most cost falls on consenting participants the arrangement sits nearer pure coordination; if on unconsenting inheritors, extraction is higher than the aggregate measure suggests and the victim set should weight the young.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_distribution_across_generations, empirical, 'Distribution of burden between consenting and inherited participation.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression holding marginal members in line structural (community-dependent livelihoods, marriage networks, burial rights) or internalized (filial guilt, fear of betraying the dead)?',
    'Post-exit trajectory of leavers: if conformity pressure and grief-policed self-monitoring persist after severing community ties, the internalized share is substantial.',
    'Internalized suppression travels with the target after exit, raising effective suppression above the structural measure and limiting what rule-level reform alone could achieve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the measured suppression.').

omega_variable(
    outgroup_exclusion_cost_incidence,
    'Does the exclusion of out-group members cost them, the community, or both — and does the host society''s power make the declared out-group burden largely nominal?',
    'Comparative intergroup-trust outcomes for communities with closed versus open memorial practices, controlling for host-society hostility levels.',
    'If exclusion costs the community more than the out-group (lost alliances, suspicion cycles), the out-group victim declaration overstates their burden and the extraction is more purely intra-communal than the victim set implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(outgroup_exclusion_cost_incidence, empirical, 'Incidence of the out-group-relations cost.').

omega_variable(
    rote_observance_goodhart_threshold,
    'Is the multigenerational rise in rote, unfelt observance proxy-drift (performance replacing function) or stable ritual form?',
    'Track whether solidarity outputs (mutual-aid mobilization, retention under pressure) track attendance or diverge from it as theater_ratio climbs.',
    'Sustained theater_ratio above 0.5 with diverging outputs would indicate the boundary shell persisting after the coordinating function has thinned — a materially different persistence mechanism than enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rote_observance_goodhart_threshold, empirical, 'Whether rising performative observance signals function decay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__boundary_maintenance_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 15, 0.14).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(cata_tr_t45, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 45, 0.23).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_kernel__boundary_maintenance_reading, theater_ratio, 75, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(cata_be_t45, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 45, 0.55).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 60, 0.57).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_kernel__boundary_maintenance_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(cata_su_t45, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 45, 0.49).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 60, 0.51).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_kernel__boundary_maintenance_reading, suppression_requirement, 75, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__boundary_maintenance_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__boundary_maintenance_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'catastrophe mourning ritual' covers four structurally distinct claims with different epsilon: boundary enforcement (this file — moderate extraction with identifiable autonomy and out-group-relation costs), symbolic continuity (low extraction), survival-competence transmission (contested functionality), and trauma encoding (warning-system function). Authored as a four-story family linked by affects_constraints; upstream/downstream edges are documented per-file in reading_relations. This reading's epsilon refers solely to the boundary-enforcement arrangement assessed by this reading's own lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
