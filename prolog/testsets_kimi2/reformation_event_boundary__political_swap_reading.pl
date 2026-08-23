% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__political_swap_reading, []).

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
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Political Realignment and Asset Transfer
 *   domain: historical_epistemology/religious_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the political_swap_reading of the
 *   contested reformation_event_boundary kernel. It models the Reformation as
 *   a primarily political realignment in which territorial princes exploited
 *   theological disputes to break papal authority, seize church assets, and
 *   consolidate sovereign state power, with theology functioning as post-hoc
 *   rationalization. The periodization runs from the 1517 theses to the Peace
 *   of Westphalia in 1648, when the political settlement stabilizes. The
 *   story treats the transfer of wealth and jurisdiction from the Catholic
 *   Church to secular rulers as the central arrangement, with the princes as
 *   agenda-setters and beneficiaries, the Church and peasantry as payers, and
 *   theological reformers as secondary beneficiaries of state patronage.
 *
 * KEY AGENTS:
 *   - territorial_princes: Primary agenda-setter and beneficiary (institutional/arbitrage) â architects of the political realignment and direct recipients of seized assets
 *   - catholic_church: Primary target (institutional/constrained) â loses wealth, jurisdiction, and territorial allegiance
 *   - rural_peasantry: Diffuse target (powerless/trapped) â bears fiscal and military costs of confessional warfare
 *   - theological_reformers: Secondary beneficiary (moderate/constrained) â provides ideological cover and gains institutional positions
 *   - radical_reformers: Excluded voice (powerless/trapped) â suppressed by both confessions, absent from state-formation narratives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.75).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.8).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Realignment and Asset Transfer").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_systems").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '682961e0-9fe7-4d07-b47b-3ab79cd29751').
narrative_ontology:cs_kernel_codification('682961e0-9fe7-4d07-b47b-3ab79cd29751', distributed).
narrative_ontology:cs_authority_grounding('682961e0-9fe7-4d07-b47b-3ab79cd29751', distributed).
narrative_ontology:cs_reading_relation('682961e0-9fe7-4d07-b47b-3ab79cd29751', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('682961e0-9fe7-4d07-b47b-3ab79cd29751', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('682961e0-9fe7-4d07-b47b-3ab79cd29751', foundational, territorial_supremacy_over_papal_jurisdiction).
narrative_ontology:cs_axiom_status(territorial_supremacy_over_papal_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('682961e0-9fe7-4d07-b47b-3ab79cd29751', territorial_supremacy_over_papal_jurisdiction, conventional).
narrative_ontology:cs_axiom('682961e0-9fe7-4d07-b47b-3ab79cd29751', foundational, theology_as_political_instrument).
narrative_ontology:cs_axiom_status(theology_as_political_instrument, holdable).
narrative_ontology:cs_axiom_grounding('682961e0-9fe7-4d07-b47b-3ab79cd29751', theology_as_political_instrument, empirically_contingent).
narrative_ontology:cs_reference_frame('682961e0-9fe7-4d07-b47b-3ab79cd29751', territorial_sovereignty_consolidation).
narrative_ontology:cs_drift_state('682961e0-9fe7-4d07-b47b-3ab79cd29751', contemporary_historiography, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('682961e0-9fe7-4d07-b47b-3ab79cd29751', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, territorial_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, theological_reformers).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, rural_peasantry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Territorial princes and city magistrates who leveraged theological disputes to break papal legal and fiscal supremacy, dissolve monasteries, and transfer ecclesiastical wealth and jurisdiction to state treasuries and courts. They set the political agenda, chose confessional alignment based on dynastic interest, and enforced the new order through edicts, church ordinances, and military force. Exit meant returning to papal supremacy and surrendering seized assets, which was economically and politically unthinkable once the transfer was underway.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, territorial_princes, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, territorial_princes, beneficiary).

% The papacy, bishoprics, monastic orders, and ecclesiastical courts that lost landed estates, tithe revenues, legal jurisdiction over marriage and morals, and the allegiance of entire territories to secular rulers. They paid through direct asset seizure and permanent authority diminution. Their exit options were constrained by military defeat, territorial encirclement, and the structural impossibility of restoring the medieval papal monarchy after Westphalia.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church, payer,
    institutional, civilizational, constrained, global).

% Agrarian laborers and village communes who bore the direct costs of the Reformation: taxes redirected from church to princely treasuries, conscription into confessional armies, pillage and devastation during the German Peasants' War and Thirty Years' War, and the loss of monastic charity and poor relief. They had no meaningful exit; local identity was confessionalized and migration was economically catastrophic.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, rural_peasantry, payer,
    powerless, immediate, trapped, local).

% Lutheran, Calvinist, and Zwinglian clergy who supplied theological justification for princely policy and received institutional positions, salaries, printing privileges, and state protection in return. They did not set the political agenda but benefited materially from the new state-church arrangements. Their exit options were constrained by career dependence on princely patronage and congregational loyalty; rejecting the political instrumentalization of theology meant losing their posts.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, theological_reformers, beneficiary,
    moderate, biographical, constrained, regional).

% Anabaptists and radical spiritualists who sought ecclesiastical reform without princely control and were suppressed by both Catholic and Protestant authorities. They would object to the political-swap narrative from the left, arguing that the Reformation betrayed spiritual renewal by subordinating it to state power. They were excluded from the Peace of Westphalia and from mainstream historiography of state formation.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, radical_reformers, excluded,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, territorial_princes).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the late-medieval problem of competing papal and territorial sovereignty by transferring legal, fiscal, and military authority from transnational church institutions to territorial princes, enabling the consolidation of the modern state system culminating in Westphalia.
% TRANSFER_FUNCTION: Moves ecclesiastical land, tithe revenue, legal jurisdiction over marriage and morals, and military allegiance from the papacy, monastic orders, and Catholic ecclesiastical courts to secular rulers and their state-controlled churches.
% ABSENT_VOICES: The papal curia and Catholic historians who read the event as apostasy rather than political rationalization; radical reformers and Anabaptists who wanted spiritual renewal without princely control; peasant communes seeking economic justice rather than dynastic asset transfers. These voices are structurally excluded from the political-swap narrative or suppressed by confessionalization.
% DISAPPEARANCE_RATIONALE: Without the political realignment, papal legal supremacy and ecclesiastical tax flows would persist in Northern Europe; the Westphalian state system would not emerge; church wealth would remain monastic rather than princely. The political map of Europe would require fundamental rearrangement.
% FOUNDING_PROBLEM: Late-medieval secular rulers faced dual sovereignty: papal claims to supreme jurisdiction, ecclesiastical immunity from taxation, and transnational church wealth that escaped princely extraction. The arrangement was built to solve the sovereignty and revenue crises of territorial state formation.
% FOUNDING_PROBLEM_CORROBORATION: Political historians of state formation and international relations scholars corroborate the sovereignty crisis from an analytical seat. Catholic institutional historians attest the revenue and jurisdiction losses from the victim side. No non-beneficiary contemporary attested the problem as framed by secular rulers while the arrangement was forming; the papacy contested the very existence of the problem as illegitimate.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__political_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__political_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the arrangement transferred massive ecclesiastical wealth and permanent legal jurisdiction to secular treasuries. Suppression (0.80) is high because the constraint's persistence required active military enforcement through the Schmalkaldic War, French Wars of Religion, Dutch Revolt, and Thirty Years' War, alongside legal confessionalization. Theater ratio (0.55) reflects the post-hoc instrumentalization of theology: religious discourse was performatively maintained to legitimize what was structurally a power and asset transfer. Accessibility collapse (0.80) is high because once church lands were secularized and monasteries dissolved, restoration became materially and institutionally nearly impossible. Resistance (0.75) is high because the Catholic Church and Catholic powers mounted sustained counter-reformation military and political resistance. The temporal series run on a shared grid from 1517 to 1648, showing extraction peaking during the Thirty Years' War and stabilizing at Westphalia.
 *
 * PERSPECTIVAL GAP:
 *   The territorial princes experience the constraint as necessary state-building and sovereignty consolidation (low d, near-beneficiary), while the Catholic Church experiences it as institutional dispossession (high d, near-target). The peasantry experiences it as undifferentiated warfare and taxation (high d, full target). Theological reformers occupy an intermediate position: they benefit from state protection but are captured by it (moderate d). The engine computes these divergences from the structural beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (territorial_princes, theological_reformers) are declared in base_properties.beneficiaries and derive low directionality: the constraint subsidizes their power and positions. Victims (catholic_church, rural_peasantry) are declared in base_properties.victims and derive high directionality: the constraint extracts wealth, jurisdiction, and labor from them. The princes' arbitrage-grade exit (they could strategically choose confessional alignment) places them near the full-beneficiary end; the peasantry's trapped exit places them near the full-target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpapal external sovereignty blocking territorial state formationâwas dead by 1648, solved by Westphalia. The arrangement persisted beyond its mandate, but not as the same constraint: the Reformation event boundary closes at 1648, after which the extracted assets and authority are embedded in the new state system. The R5 interview (founding_problem_status: dead, disappearance_verdict: world_rearranges) flags this as a resolved mandate that produced permanent rearrangement, distinguishing it from a zombie constraint. This prevents mislabeling the post-1648 state system as an ongoing extraction mechanism; the extraction was front-loaded into the event boundary itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Reformation event boundary best captured by a single-cause political reading, or does it require decomposition into multiple structurally distinct constraints?',
    'Comparative evaluation of the sibling readings'' predictive and explanatory power against archival evidence of reformer-ruler interactions.',
    'If the kernel is irreducibly composite, the political_swap reading''s high extractiveness is accurate for the power-transfer component but not for the whole event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the kernel admits single-cause decomposition or requires multiple constraints').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (princely military enforcement, legal penalties) or internalized (confessional identity fusion making return to Catholicism unthinkable)?',
    'Post-Westphalian confessionalization studies measuring rates of reconversion and crypto-religiosity versus formal legal compliance.',
    'If internalized, effective suppression exceeds the structural measure because subjects carry the constraint after legal enforcement relaxes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in confessionalization').

omega_variable(
    asset_seizure_vs_authority_transfer,
    'What proportion of the constraint''s extraction consists of one-time asset seizure versus ongoing authority transfer?',
    'Archival quantification of monastic dissolutions and secularizations compared to longitudinal analysis of jurisdiction and tithe diversion.',
    'High one-time seizure would suggest the constraint''s extractiveness peaked before Westphalia and declined afterward; ongoing authority transfer sustains extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asset_seizure_vs_authority_transfer, empirical, 'Decomposition of extraction into seizure versus ongoing authority transfer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_event_boundary__political_swap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(refo_tr_t5, reformation_event_boundary__political_swap_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(refo_tr_t10, reformation_event_boundary__political_swap_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(refo_tr_t15, reformation_event_boundary__political_swap_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(refo_tr_t20, reformation_event_boundary__political_swap_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(refo_tr_t25, reformation_event_boundary__political_swap_reading, theater_ratio, 25, 0.65).
narrative_ontology:measurement(refo_tr_t30, reformation_event_boundary__political_swap_reading, theater_ratio, 30, 0.63).
narrative_ontology:measurement(refo_tr_t35, reformation_event_boundary__political_swap_reading, theater_ratio, 35, 0.58).
narrative_ontology:measurement(refo_tr_t40, reformation_event_boundary__political_swap_reading, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_event_boundary__political_swap_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(refo_be_t5, reformation_event_boundary__political_swap_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(refo_be_t10, reformation_event_boundary__political_swap_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(refo_be_t15, reformation_event_boundary__political_swap_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(refo_be_t20, reformation_event_boundary__political_swap_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(refo_be_t25, reformation_event_boundary__political_swap_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement(refo_be_t30, reformation_event_boundary__political_swap_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement(refo_be_t35, reformation_event_boundary__political_swap_reading, base_extractiveness, 35, 0.76).
narrative_ontology:measurement(refo_be_t40, reformation_event_boundary__political_swap_reading, base_extractiveness, 40, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_event_boundary__political_swap_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(refo_su_t5, reformation_event_boundary__political_swap_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(refo_su_t10, reformation_event_boundary__political_swap_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(refo_su_t15, reformation_event_boundary__political_swap_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(refo_su_t20, reformation_event_boundary__political_swap_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(refo_su_t25, reformation_event_boundary__political_swap_reading, suppression_requirement, 25, 0.92).
narrative_ontology:measurement(refo_su_t30, reformation_event_boundary__political_swap_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(refo_su_t35, reformation_event_boundary__political_swap_reading, suppression_requirement, 35, 0.75).
narrative_ontology:measurement(refo_su_t40, reformation_event_boundary__political_swap_reading, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% This constraint instantiates the political_swap_reading of the reformation_event_boundary kernel, decomposing the Reformation into a structurally specific claim about authority and asset transfer. Sibling readings (theological_climb_reading, composite_overdetermination_reading) instantiate different constraints from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
