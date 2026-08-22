% ============================================================================
% CONSTRAINT STORY: ihl_distinction_proportionality__human_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ihl_distinction_proportionality__human_agency_reading, []).

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
 *   constraint_id: ihl_distinction_proportionality__human_agency_reading
 *   human_readable: IHL Distinction and Proportionality — Mandatory Human Final Targeting Decision (Human Agency Reading)
 *   domain: international_humanitarian_law/military_ethics/technology_governance
 *
 * SUMMARY:
 *   International Humanitarian Law's distinction and proportionality
 *   obligations, read through the Martens Clause, are asserted to require
 *   irreducible human moral judgment at the moment lethal force is applied:
 *   no machine may make the final targeting decision. Operationally this
 *   renders fully autonomous weapons unlawful and confines permissible
 *   autonomy to systems supervised by a responsible human. The arrangement
 *   under contest is that requirement itself. This story instantiates ONE
 *   reading of the contested kernel ihl_distinction_proportionality only; the
 *   outcomes-based and categorical-prohibition readings are separate
 *   constraint stories linked through the network section. The claim/metric
 *   gap is deliberate: the reading CLAIMS the requirement as a demand of
 *   humanity and law, while the authored metrics describe its actual
 *   political economy — concentrated interpretive-authority gains, diffuse
 *   military-efficiency costs, maturing enforcement machinery, and partial
 *   nominalization of 'human control' in fielded practice. The engine
 *   measures the divergence; nothing here reconciles claim to metrics. KEY
 *   AGENTS (by structural relationship): - ihl_interpretive_authorities:
 *   Primary beneficiary (institutional/identity_locked) — collects
 *   interpretive centrality and moral authority -
 *   state_military_establishments: Primary target (institutional/constrained)
 *   — bears operational-tempo, staffing, and commander-liability costs -
 *   combatant_operators: Dual-positioned bearer (moderate/trapped) — carries
 *   decision burden and liability, retains moral agency -
 *   autonomous_weapons_developers: Secondary target (powerful/mobile) —
 *   full-autonomy product lines closed out of adhering markets -
 *   civilian_populations_conflict_zones: Declared protectee
 *   (powerless/trapped) — intended recipients of the judgment floor -
 *   ccw_gge_state_parties: Agenda setter (institutional/constrained) —
 *   administers the diplomatic process - humanitarian_advocacy_organizations:
 *   Secondary beneficiary (organized/identity_locked) -
 *   non_state_armed_groups: Excluded voice (organized/trapped) — bound by
 *   IHL, absent from the room - military_ethics_analysts: Analytical observer
 *   — sees the full structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, 0.65).
domain_priors:suppression_score(ihl_distinction_proportionality__human_agency_reading, 0.72).
domain_priors:theater_ratio(ihl_distinction_proportionality__human_agency_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ihl_distinction_proportionality__human_agency_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ihl_distinction_proportionality__human_agency_reading, tangled_rope).
narrative_ontology:human_readable(ihl_distinction_proportionality__human_agency_reading, "IHL Distinction and Proportionality — Mandatory Human Final Targeting Decision (Human Agency Reading)").
narrative_ontology:topic_domain(ihl_distinction_proportionality__human_agency_reading, "international_humanitarian_law/military_ethics/technology_governance").

domain_priors:requires_active_enforcement(ihl_distinction_proportionality__human_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ihl_distinction_proportionality__human_agency_reading, 'c5a24ac2-0356-4622-a215-537a5e779685').
narrative_ontology:cs_kernel_codification('c5a24ac2-0356-4622-a215-537a5e779685', fixed_text).
narrative_ontology:cs_authority_grounding('c5a24ac2-0356-4622-a215-537a5e779685', lineage).
narrative_ontology:cs_interpretation_layer_present('c5a24ac2-0356-4622-a215-537a5e779685').
narrative_ontology:cs_reading_relation('c5a24ac2-0356-4622-a215-537a5e779685', ihl_distinction_proportionality__outcomes_based_reading, forecloses).
narrative_ontology:cs_reading_relation('c5a24ac2-0356-4622-a215-537a5e779685', ihl_distinction_proportionality__categorical_prohibition_reading, coexists_with).
narrative_ontology:cs_axiom('c5a24ac2-0356-4622-a215-537a5e779685', foundational, lethal_force_requires_irreducible_human_judgment).
narrative_ontology:cs_axiom_status(lethal_force_requires_irreducible_human_judgment, holdable).
narrative_ontology:cs_axiom_grounding('c5a24ac2-0356-4622-a215-537a5e779685', lethal_force_requires_irreducible_human_judgment, deontological).
narrative_ontology:cs_axiom('c5a24ac2-0356-4622-a215-537a5e779685', foundational, delegating_kill_decisions_to_machines_violates_principles_of_humanity).
narrative_ontology:cs_axiom_status(delegating_kill_decisions_to_machines_violates_principles_of_humanity, holdable).
narrative_ontology:cs_axiom_grounding('c5a24ac2-0356-4622-a215-537a5e779685', delegating_kill_decisions_to_machines_violates_principles_of_humanity, deontological).
narrative_ontology:cs_reference_frame('c5a24ac2-0356-4622-a215-537a5e779685', geneva_tradition_human_centered_targeting).
narrative_ontology:cs_drift_state('c5a24ac2-0356-4622-a215-537a5e779685', contemporary_autonomous_systems_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c5a24ac2-0356-4622-a215-537a5e779685', '').
narrative_ontology:cs_kernel_id(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, humanitarian_advocacy_organizations).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, civilian_populations_conflict_zones).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, state_military_establishments).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers).
narrative_ontology:constraint_victim(ihl_distinction_proportionality__human_agency_reading, combatant_operators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ihl_distinction_proportionality__human_agency_reading, combatant_operators).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, martens_clause_principles_of_humanity).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, command_responsibility_doctrine).
narrative_ontology:constraint_vindicates(ihl_distinction_proportionality__human_agency_reading, meaningful_human_control_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodians of the Geneva tradition — the ICRC above all — who publish interpretive guidance on how distinction and proportionality apply to new weapons. The requirement that a human make the final targeting decision keeps contextual legal judgment, and therefore professional interpretation of it, at the center of lawful-conduct debates. Conference access, advisory standing, and moral authority flow from that continued centrality. Their organizational identity is fused with guardianship of the humanitarian-law tradition; stepping back from the role is not a live option for them.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities, beneficiary,
    institutional, generational, identity_locked, global).

% Campaign networks and NGOs that organize public and diplomatic pressure around keeping humans responsible for kill decisions. The human-control framing supplies their agenda, mobilization narratives, and funding cases. Their institutional identities are bound to the cause; abandoning it would dissolve their reason for convening.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, humanitarian_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, global).

% Residents of areas where strikes occur. The requirement is designed so that someone answerable to law weighs their status before force is used. The protection reaches them indirectly and unevenly — it depends on how faithfully the human decision is made — and they have no channel to negotiate the terms; they live under whatever standard the belligerents settle on.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, civilian_populations_conflict_zones, beneficiary,
    powerless, immediate, trapped, regional).

% Armed forces of states party to IHL. They staff supervision loops, accept slower engagement timelines, train operators for legal accountability, and expose commanders to prosecution risk when judgments fail. They cannot withdraw from IHL's reach — customary obligations follow them — so their practical choices are shaping interpretation, resisting binding instruments, or absorbing the costs.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, state_military_establishments, payer,
    institutional, generational, constrained, global).

% Defense contractors and research programs building toward full autonomy in targeting. The requirement closes their end-state product out of adhering markets. Their realistic paths are pivoting to human-supervised product lines, selling to non-adhering states, or redirecting investment. Capital mobility makes redirection possible, though sunk program costs are real.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, autonomous_weapons_developers, payer,
    powerful, biographical, mobile, global).

% Soldiers and officers placed in the decision seat. They carry the cognitive load of compressed-time moral judgment, the legal exposure of being the accountable node, and the psychological weight of firing; they also retain moral agency, professional standing, and the legal protections that attach to human decision-makers. They do not choose whether to occupy the seat — assignment places them there.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, combatant_operators, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(ihl_distinction_proportionality__human_agency_reading, combatant_operators, beneficiary).

% Diplomats and ministries administering the CCW process where the status of autonomous weapons is negotiated. They draft guiding texts, decide whether rules bind or merely guide, and balance alliance commitments, industrial interests, and humanitarian pressure. They set the agenda but are themselves bound by consensus rules and by obligations they cannot shed.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, ccw_gge_state_parties, agenda_setter,
    institutional, generational, constrained, global).

% Armed groups outside the treaty-diplomatic room who would nonetheless be bound by any crystallized requirement. They hold no seat in the negotiation that defines their obligations, and their practical exit from IHL's reach is nil. Their objection — that states reserve advanced supervised systems for themselves while criminalizing lesser-capacity actors — goes unheard in the forum.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, non_state_armed_groups, excluded,
    organized, biographical, trapped, regional).

% Scholars and analysts across law, philosophy, and strategic studies who examine how the requirement operates: whether mandated judgment is genuine, whom it protects, and what it costs. They hold no stake in adoption or rejection and can see the whole structure.
narrative_ontology:constraint_stakeholder(ihl_distinction_proportionality__human_agency_reading, military_ethics_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ihl_distinction_proportionality__human_agency_reading, ihl_interpretive_authorities).
narrative_ontology:fixing_cost_class(ihl_distinction_proportionality__human_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common accountability floor for lethal force: guarantees that every engagement decision passes through an agent who can weigh distinction and proportionality contextually, bear legal responsibility, and extend or withhold mercy — preserving command-responsibility chains as decision speed increases.
% TRANSFER_FUNCTION: Moves final engagement authority from machine processes to designated human decision-makers; correspondingly moves decision latency, cognitive load, and legal exposure onto operators and commands, and moves operational-tempo value away from state militaries while consolidating interpretive authority over lawful targeting in the humanitarian-law apparatus.
% ABSENT_VOICES: Non-state armed groups are bound by IHL but hold no seat in CCW diplomacy; conflict-zone civilians are represented only vicariously through advocacy organizations; conscripted operators enter liability roles without negotiating standing. All sit outside the treaty-diplomatic room where the requirement's terms are set.
% DISAPPEARANCE_RATIONALE: If the human-final-decision requirement vanished overnight, procurement and doctrine would shift rapidly toward full autonomy, accountability would reorganize around system certification rather than commander judgment, the interpretive apparatus would lose its central adjudicative role, and the advocacy sector would lose its organizing frame — the entire governance architecture of lethal automation would rebuild around a different axis.
% FOUNDING_PROBLEM: IHL's core conduct-of-hostilities duties were written for human combatants exercising contextual judgment; increasing automation threatened to sever lethal decisions from responsible judgment, creating accountability gaps and mass-violation risk at machine speed. The requirement was articulated to keep distinction and proportionality executable by a responsible human agent.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: state defense doctrines themselves (e.g., US DoD Directive 3000.09's requirement for appropriate levels of human judgment), CCW GGE records in which the paying states acknowledge the compliance question, and independent academic international-law literature. No party disputes that the compliance problem exists; the dispute is over whether human judgment is its solution.
narrative_ontology:disappearance_verdict(ihl_distinction_proportionality__human_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(ihl_distinction_proportionality__human_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ihl_distinction_proportionality__human_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ihl_distinction_proportionality__human_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ihl_distinction_proportionality__human_agency_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ihl_distinction_proportionality__human_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ihl_distinction_proportionality__human_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ihl_distinction_proportionality__human_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.65: the requirement concentrates durable gains — interpretive centrality, agenda control, a protected market for supervised architectures — while spreading its costs diffusely across every military's operational tempo and across an entire suppressed development class; a genuine accountability floor tempers but does not erase the asymmetry. Suppression (0.72) is a raw structural property, unscaled by power or scope: adherence forecloses the full-autonomy path outright regardless of demonstrated performance, and enforcement machinery (GGE guiding principles, hardening national positions, export controls, prosecutorial exposure) has matured steadily across the interval — hence the rising suppression_requirement series. Theater_ratio (0.40) reflects the documented tendency of 'meaningful human control' to degrade into nominal approval of machine-proposed engagements. Accessibility_collapse (0.60): once the requirement is accepted, machine-decided killing collapses as an option within the framework, but supervised alternatives remain fully workable, so collapse is partial. Resistance (0.65): major military powers have consistently resisted binding instruments, preferring political declarations. Claimed type is tangled_rope on independent structural grounds: the arrangement solves a real collective problem (keeping lethal decisions tethered to answerable judgment) AND runs asymmetric collection through the same structure (interpretive authorities gain centrality; militaries pay tempo and liability), held together by active enforcement. All three tracked series share one time grid (biennial, t=0..12) so temporal analysis reads aligned rows.
 *
 * PERSPECTIVAL GAP:
 *   Seats should classify differently. From the ihl_interpretive_authorities seat the requirement is the condition of the profession's existence — subsidy-shaped, near-zero effective burden. From the state_military_establishments seat it is a binding charge on tempo, staffing, and commander safety with no exit from customary reach. Combatant_operators straddle: they pay in load and liability yet collect in moral agency and legal personhood. Autonomous_weapons_developers experience a market closure they can partially arbitrage by pivoting. Civilians, the declared protectees, receive the arrangement's promise secondhand. The engine computes these divergences from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (interpretive authorities, advocacy organizations, conflict-zone civilians) drive those seats toward the beneficiary end of d; victim declarations (military establishments, autonomy developers, operators as primary) drive them toward the target end. Exit modulation separates otherwise similar seats: interpretive authorities are identity_locked (guardianship is who they are), developers are mobile (capital redirects), militaries are constrained (customary law follows them), operators are trapped (assignment places them in the seat). The civilian beneficiary seat is flagged by omega: their benefit is indirect and contested, so their derived low d is provisional. No directionality overrides are authored — the beneficiary/victim declarations plus exit options carry the differentiation, and the two institutional seats differ by role rather than by power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — keeping distinction and proportionality executable by a responsible agent as automation accelerates — is live and intensifying, so no mandatrophy resolution is declared. The tangled_rope classification prevents two mislabels: reading the arrangement as pure extraction would erase the real accountability floor that even skeptics rely on; reading it as pure coordination would hide the concentrated interpretive-authority gains riding on diffuse military costs. The R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no dead-mandate flag — while the theater series tracks the separate risk that the mandated judgment hollows into performance over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates the human_agency_reading of kernel ihl_distinction_proportionality; do the sibling readings (outcomes_based_reading, categorical_prohibition_reading) instantiate structurally different constraints — different ε, victim sets, and enforcement structures — or variants of one?',
    'Generate the sibling stories and compare computed classifications; divergence across the family confirms indexical separation rather than one observable-dependent constraint.',
    'If siblings compute identical types and ε, the kernel decomposes differently than assumed and the three files should merge; if they diverge as expected, the family models the CCW dispute as competing constraints rather than one contested rule.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer-frame routing: one reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    disagreement_locus_decision_vs_outcome,
    'Where is the structural disagreement located: does IHL''s distinction/proportionality obligation attach to the locus of the lethal decision (who judges) or to the quality of outcomes achieved?',
    'Treaty interpretation history and GGE negotiating records: whether states treat ''attack'' as a human act requiring judgment or as an event assessable by results.',
    'If the obligation attaches to outcomes, this reading''s closure of full autonomy loses its legal foundation and the constraint migrates toward the outcomes_based sibling; if it attaches to the decision locus, outcomes parity is irrelevant and the reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_locus_decision_vs_outcome, conceptual, 'Locates the kernel dispute: decision locus versus outcome quality.').

omega_variable(
    genuine_judgment_vs_nominal_approval,
    'Does the human decision the constraint mandates function as irreducible moral judgment in practice, or does it degrade into nominal approval of machine-proposed engagements?',
    'Operator workload and engagement-time studies; analysis of decision latency between machine proposal and human confirmation in fielded systems.',
    'If approval is nominal, theater_ratio rises further, the coordination function decays toward performance, and the constraint drifts piton-ward despite formal compliance; if judgment is genuine, the coordination function holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_judgment_vs_nominal_approval, empirical, 'Whether mandated human control is substantive or rubber-stamp in fielded practice.').

omega_variable(
    civilian_protective_effect_contested,
    'Do conflict-zone civilians actually receive the protection the constraint declares, given that human judgment carries its own error classes (fatigue, bias, vengeance) alongside machine error classes?',
    'Comparative casualty-pattern analysis across human-decided versus machine-assisted engagements, controlling for mission type.',
    'If civilian benefit is largely illusory, the civilian seat''s directionality shifts from beneficiary toward bystander-bearer and the constraint''s coordination function narrows toward institutional self-maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_protective_effect_contested, empirical, 'Contested protective benefit for the declared civilian beneficiaries.').

omega_variable(
    enforcement_pathway_crystallization,
    'Will the human-final-decision requirement bind through a new CCW protocol, customary-law crystallization, or remain soft law — and does the pathway change who bears enforcement costs?',
    'Track state-practice and opinio juris accumulation; observe whether major military powers incorporate the requirement into doctrine absent a treaty.',
    'Customary crystallization raises suppression and spreads costs to all belligerents including currently excluded non-state groups; soft-law stasis keeps enforcement cheap for resisters and concentrates costs on complying states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_pathway_crystallization, empirical, 'Uncertainty over the binding pathway and its distributional consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ihl_distinction_proportionality__human_agency_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ihl__tr_t0, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(ihl__tr_t0, observed).
narrative_ontology:measurement(ihl__tr_t2, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 2, 0.26).
narrative_ontology:measurement_basis(ihl__tr_t2, observed).
narrative_ontology:measurement(ihl__tr_t4, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement_basis(ihl__tr_t4, observed).
narrative_ontology:measurement(ihl__tr_t6, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(ihl__tr_t6, observed).
narrative_ontology:measurement(ihl__tr_t8, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(ihl__tr_t8, observed).
narrative_ontology:measurement(ihl__tr_t10, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(ihl__tr_t10, observed).
narrative_ontology:measurement(ihl__tr_t12, ihl_distinction_proportionality__human_agency_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(ihl__tr_t12, observed).

% Extraction over time
narrative_ontology:measurement(ihl__be_t0, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(ihl__be_t0, observed).
narrative_ontology:measurement(ihl__be_t2, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 2, 0.47).
narrative_ontology:measurement_basis(ihl__be_t2, observed).
narrative_ontology:measurement(ihl__be_t4, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(ihl__be_t4, observed).
narrative_ontology:measurement(ihl__be_t6, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 6, 0.56).
narrative_ontology:measurement_basis(ihl__be_t6, observed).
narrative_ontology:measurement(ihl__be_t8, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement_basis(ihl__be_t8, observed).
narrative_ontology:measurement(ihl__be_t10, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(ihl__be_t10, observed).
narrative_ontology:measurement(ihl__be_t12, ihl_distinction_proportionality__human_agency_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement_basis(ihl__be_t12, observed).

% Suppression requirement over time
narrative_ontology:measurement(ihl__su_t0, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(ihl__su_t0, observed).
narrative_ontology:measurement(ihl__su_t2, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 2, 0.54).
narrative_ontology:measurement_basis(ihl__su_t2, observed).
narrative_ontology:measurement(ihl__su_t4, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement_basis(ihl__su_t4, observed).
narrative_ontology:measurement(ihl__su_t6, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement_basis(ihl__su_t6, observed).
narrative_ontology:measurement(ihl__su_t8, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement_basis(ihl__su_t8, observed).
narrative_ontology:measurement(ihl__su_t10, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement_basis(ihl__su_t10, observed).
narrative_ontology:measurement(ihl__su_t12, ihl_distinction_proportionality__human_agency_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement_basis(ihl__su_t12, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ihl_distinction_proportionality__human_agency_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__outcomes_based_reading).
narrative_ontology:affects_constraint(ihl_distinction_proportionality__human_agency_reading, ihl_distinction_proportionality__categorical_prohibition_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'IHL rules on autonomous weapons' covers three structurally distinct claims sharing one kernel (ihl_distinction_proportionality). This file authors the human_agency_reading only — ε refers solely to the human-final-decision arrangement as this reading assesses it. The outcomes_based_reading (performance parity suffices; technology-neutral) and categorical_prohibition_reading (dignity violated per se by machine-decided killing) get their own ε, beneficiary/victim structures, and classifications in their own files. Upstream/downstream: the human-agency reading is the interpretive center of gravity that the outcomes-based reading pushes against and the categorical reading strengthens from the flank; all three are linked via affects_constraints so contamination and legitimacy shifts propagate across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
