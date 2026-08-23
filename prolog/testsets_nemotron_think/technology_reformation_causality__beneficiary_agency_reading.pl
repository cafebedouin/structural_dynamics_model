% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__beneficiary_agency_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition Strategic Deployment of Printing to Bypass Church Authority
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story models the 'beneficiary agency' reading of the
 *   technology-Reformation causality kernel. It treats the strategic alliance
 *   between Protestant reformers (Luther, Calvin, Zwingli, and their
 *   networks) and printing entrepreneurs (Froben, Petri, Estienne, and the
 *   wider print trade) as a tangled_rope: a coordination mechanism that
 *   solved the genuine collective-action problem of disseminating reformist
 *   ideas past Church censorship, while simultaneously extracting religious
 *   authority and economic revenue from the Catholic Church. The printing
 *   press itself functions as a scaffold — a temporary support that enabled
 *   the coalition's rise but whose necessity diminishes once alternative
 *   dissemination networks (correspondence, preaching, manuscript
 *   circulation) are established. The constraint's extractiveness (ε=0.68)
 *   derives from the value of the authority bypass: every vernacular Bible
 *   printed and sold is a transfer of interpretive monopoly from the Church
 *   to the coalition.
 *
 * KEY AGENTS:
 *   - protestant_reformers: Primary agenda_setters (organized/identity_locked) — drive the theological and political program, depend on print for reach
 *   - printing_entrepreneurs: Primary beneficiaries (moderate/constrained) — capture commercial revenue, depend on reformist demand for market
 *   - catholic_church_authority: Primary payer/victim (institutional/trapped) — loses doctrinal monopoly, revenue from indulgences, and control over religious discourse
 *   - lay_population: Secondary beneficiaries (powerless/constrained) — gain vernacular scripture access but face religious conflict and social disruption
 *   - rival_catholic_printers: Excluded (moderate/constrained) — displaced from reformist markets, some adapt via Counter-Reformation printing
 *   - historical_analyst: Observer (analytical/analytical) — evaluates causal claims from outside the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.68).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.72).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition Strategic Deployment of Printing to Bypass Church Authority").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, 'a9dddde4-7f6f-40b9-8259-69387e44b8ec').
narrative_ontology:cs_kernel_codification('a9dddde4-7f6f-40b9-8259-69387e44b8ec', distributed).
narrative_ontology:cs_authority_grounding('a9dddde4-7f6f-40b9-8259-69387e44b8ec', distributed).
narrative_ontology:cs_reading_relation('a9dddde4-7f6f-40b9-8259-69387e44b8ec', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('a9dddde4-7f6f-40b9-8259-69387e44b8ec', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('a9dddde4-7f6f-40b9-8259-69387e44b8ec', foundational, technology_as_instrument_not_cause).
narrative_ontology:cs_axiom_status(technology_as_instrument_not_cause, holdable).
narrative_ontology:cs_axiom_grounding('a9dddde4-7f6f-40b9-8259-69387e44b8ec', technology_as_instrument_not_cause, empirically_contingent).
narrative_ontology:cs_axiom('a9dddde4-7f6f-40b9-8259-69387e44b8ec', foundational, reformer_printer_coalition_as_primary_driver).
narrative_ontology:cs_axiom_status(reformer_printer_coalition_as_primary_driver, holdable).
narrative_ontology:cs_axiom_grounding('a9dddde4-7f6f-40b9-8259-69387e44b8ec', reformer_printer_coalition_as_primary_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('a9dddde4-7f6f-40b9-8259-69387e44b8ec', pre_reformation_church_monopoly).
narrative_ontology:cs_drift_state('a9dddde4-7f6f-40b9-8259-69387e44b8ec', post_reformation_fragmentation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('a9dddde4-7f6f-40b9-8259-69387e44b8ec', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, printing_entrepreneurs).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, catholic_church_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, lay_population).
narrative_ontology:constraint_vindicates(technology_reformation_causality__beneficiary_agency_reading, sola_scriptura).
narrative_ontology:constraint_vindicates(technology_reformation_causality__beneficiary_agency_reading, priesthood_of_all_believers).
narrative_ontology:constraint_vindicates(technology_reformation_causality__beneficiary_agency_reading, vernacular_scripture_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They drive the theological and political program of the Reformation, using printing to bypass Church censorship and disseminate vernacular scripture, polemics, and catechisms. Their authority and legitimacy depend on the bypass; exit would mean abandoning their vocation and the movement they lead. They invest in protecting printers, establishing distribution networks, and negotiating with princes for printing privileges.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, protestant_reformers, agenda_setter,
    organized, generational, identity_locked, continental).

% They print and distribute reformist works, capturing a new mass market for vernacular religious texts. They navigate censorship, secure privileges from sympathetic princes, and build transnational distribution networks. Their profits depend on reformist demand; exit would mean returning to the smaller, Church-controlled Latin market. Some diversify into Catholic printing during Counter-Reformation.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, printing_entrepreneurs, beneficiary,
    moderate, biographical, constrained, continental).

% They lose doctrinal monopoly, revenue from indulgences and annates, control over religious education, and temporal power in Protestant territories. They respond with the Index, the Inquisition, the Council of Trent, and political-military suppression (Schmalkaldic War, Thirty Years' War). They cannot exit the challenge — the constraint directly threatens their institutional survival.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, catholic_church_authority, payer,
    institutional, civilizational, trapped, continental).

% They gain access to scripture in vernacular, enabling personal interpretation and new forms of piety. They also face religious conflict, iconoclasm, social disruption, and coerced confessionalization (cuius regio, eius religio). Their exit options are limited by geography, ruler's confession, and literacy.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, lay_population, beneficiary,
    powerless, biographical, constrained, continental).

% They are marginalized from the booming reformist print market. Some adapt by printing Catholic polemics, Tridentine catechisms, and Index-compliant works for the Counter-Reformation market. Others relocate to Catholic territories. Their exclusion is maintained by the reformer-printer coalition's control of distribution networks in Protestant zones.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, rival_catholic_printers, excluded,
    moderate, biographical, constrained, continental).

% Evaluates the causal role of printing in the Reformation from outside the contest. Reads the same archives (print runs, correspondence, censorship records) but applies different causal ontologies: technological determinism, social agency, or co-constitution. Their classification of the constraint depends on which reading they adopt.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reformer-printer coalition coordinates the production, financing, and distribution of reformist literature across political fragmentation, solving the collective-action problem of bypassing Church censorship and Latin monolingualism.
% TRANSFER_FUNCTION: Moves interpretive authority over scripture, revenue from book sales and diverted ecclesiastical incomes, and control over religious education from the Catholic Church to the reformer-printer coalition and, secondarily, to lay readers and Protestant princes.
% ABSENT_VOICES: The diverse lay population (not a monolith) — many resisted vernacularization, feared social disruption, or remained Catholic. Catholic printers displaced from reformist markets — their economic loss and adaptation strategies are understudied. Jewish and Muslim communities in Europe — affected by the religious polarization but excluded from the coalition's framing.
% DISAPPEARANCE_RATIONALE: If the reformer-printer coalition and its printing bypass vanished overnight, the Church's monopoly on Latin scripture and doctrinal authority would likely have persisted for centuries. The Reformation as a mass movement depended on printed vernacular texts; without them, dissent would have remained localized and suppressible (as with Hussites, Lollards). The political map of Europe, the rise of nation-states, and the secularization of authority would follow radically different trajectories.
% FOUNDING_PROBLEM: The Catholic Church's monopoly on scriptural interpretation (Latin Vulgate, clerical exegesis) and its control over the means of salvation (sacraments, indulgences, purgatory) prevented reformist ideas from spreading beyond local circles. The coalition was built to break this monopoly by putting vernacular scripture directly into lay hands.
% FOUNDING_PROBLEM_CORROBORATION: The reformers themselves (Luther's prefaces, Calvin's institutes) attest the problem was live. Catholic apologists (Eck, Bellarmine) attest it was a manufactured crisis. Modern historians outside the benefiting parties: Eisenstein (printing as agent of change) corroborates the monopoly's reality; Scribner and Pettegree (reformation as social movement) corroborate the coalition's agency; Duffy (stripping of the altars) corroborates the lay experience of loss. No single consensus exists.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68) because the coalition captures the Church's former monopoly rents: interpretive authority, tithe flows, and educational control. Suppression is high (0.72) because the constraint's persistence depends on active enforcement — censorship evasion, protection of printers, smuggled distribution networks — not on participant preference. Theater ratio is moderate (0.38): the coordination function (getting texts to readers) is real, but a growing share of printing activity serves factional signaling and market positioning rather than pure dissemination. Accessibility collapse (0.55) reflects that once the bypass exists, returning to Church-controlled Latin monolingualism becomes difficult but not impossible (as Counter-Reformation showed). Resistance (0.75) captures the Church's sustained Counter-Reformation, Index librorum prohibitorum, and political-military suppression. The measurement series uses a shared time grid (0-100, normalized across 1517-1648) so every metric is authored at each point.
 *
 * PERSPECTIVAL GAP:
 *   The reformer seat experiences the constraint as rope (coordination enabling their mission); the printer seat experiences it as rope with extractive upside (market creation); the Church seat experiences it as snare (pure extraction of their authority); the lay seat experiences it as scaffold (temporary access that becomes permanent). The engine computes this divergence from the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers are agenda_setters with identity_locked exit (their vocation is bound to the bypass) — directionality near target end for the constraint's maintenance costs but beneficiary end for its gains. Printers are beneficiaries with constrained exit (market-dependent) — directionality near symmetric. Church authority is payer with trapped exit (institutional survival at stake) — directionality at full target. Lay population are beneficiaries with constrained exit — directionality damped toward beneficiary. Rival Catholic printers are excluded — their exclusion is the enforcement object. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Church monopoly on scripture and authority) is contested — the Church argues it remains live (unity, tradition), reformers argue it is dead (scripture accessible), historians argue it transformed. The coalition persists long after the initial bypass because it has become the new establishment in Protestant territories (mandatrophy unresolved). The tangled_rope classification captures this: the coordination function (dissemination) remains live, but the extraction (authority capture) has become the dominant logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    printing_necessity_vs_accelerant,
    'Was the printing press a necessary condition for the Reformation''s success, or merely an accelerant that made an already-brewing movement faster and wider?',
    'Counterfactual historical analysis comparing regions with and without early printing infrastructure; econometric modeling of idea diffusion speeds with manuscript vs. print networks.',
    'If necessary condition, the technology itself carries more causal weight (supporting determinism reading); if accelerant, the agency of the reformer-printer coalition is primary (supporting this reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printing_necessity_vs_accelerant, empirical, 'Necessity vs. acceleration of printing technology for Reformation outcomes').

omega_variable(
    coalition_power_asymmetry,
    'Was the reformer-printer coalition a genuinely mutual extraction (both parties extracting from the Church and each other), or did one party (reformers or printers) dominate the value capture?',
    'Archival research on printing contracts, profit-sharing arrangements, and censorship negotiations; network analysis of printer-reformer correspondence and financial records.',
    'If asymmetric, the tangled_rope classification may need refinement to snare (if one party exploits the other) or rope (if coordination dominates).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_power_asymmetry, empirical, 'Internal power dynamics within the reformer-printer coalition').

omega_variable(
    kernel_framing_underdetermination,
    'Does the kernel ''technology_reformation_causality'' admit a single coherent framing, or do the sibling readings operate on fundamentally different causal ontologies (technological vs. social vs. interactional)?',
    'Philosophical analysis of causal claims in historiography; comparison of explanatory frameworks across the three readings to identify incommensurable premises.',
    'If framings are incommensurable, the kernel itself may be a category error — the ''contest'' is not between readings of the same claim but between different causal languages.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel''s framing underdetermines the readings'' structural relationships').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trc_bar_tr_t0, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(trc_bar_tr_t20, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(trc_bar_tr_t40, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 40, 0.33).
narrative_ontology:measurement(trc_bar_tr_t60, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(trc_bar_tr_t80, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 80, 0.37).
narrative_ontology:measurement(trc_bar_tr_t100, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 100, 0.38).

% Extraction over time
narrative_ontology:measurement(trc_bar_be_t0, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(trc_bar_be_t20, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(trc_bar_be_t40, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(trc_bar_be_t60, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(trc_bar_be_t80, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(trc_bar_be_t100, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trc_bar_su_t0, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(trc_bar_su_t20, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(trc_bar_su_t40, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(trc_bar_su_t60, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(trc_bar_su_t80, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 80, 0.71).
narrative_ontology:measurement(trc_bar_su_t100, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, information_standard).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__beneficiary_agency_reading, 0.03).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the single historiographical label 'printing caused the Reformation' into three structurally distinct causal claims with different ε values, stakeholder structures, and temporal dynamics. The beneficiary_agency_reading isolates the reformer-printer coalition as the primary extractive agent; the technological_determinism_reading isolates the press as an autonomous causal force; the co_constitution_reading isolates the feedback loop. They are linked because each cites the others' evidence selectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__beneficiary_agency_reading, powerless, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
