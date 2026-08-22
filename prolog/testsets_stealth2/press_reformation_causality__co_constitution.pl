% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Economy / Religious-Controversy Co-Constitution Loop (Early Reformation)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Between 1517 and 1555 the market for printed matter and the crisis of
 *   Western Christendom drove each other: printers discovered that
 *   controversy sold, reformers discovered that print made their arguments
 *   irreversible, and church enforcement discovered it could not police an
 *   industry whose capital was mobile and whose customers were pious. This
 *   story instantiates the co-constitution reading of the press-Reformation
 *   causality kernel: neither the press as autonomous cause nor reformer
 *   strategy as prime mover, but a reciprocal loop in which press economics
 *   and religious controversy co-produced the Reformation's form and speed.
 *   The arrangement under contest is that loop itself, assessed here as
 *   transitional enabling infrastructure whose open phase closed when
 *   confessional settlement re-imposed licensing and orthodoxy. Beneficiaries
 *   and victims are declared across nine seats; extraction is distributed
 *   rather than captured. KEY AGENTS (by structural relationship): -
 *   reformation_printers: Dual-position pivot actors (organized/constrained)
 *   — collect pamphlet profits, bear ban-and-ruin risk -
 *   reformist_theologians: Primary beneficiary (powerful/identity_locked) —
 *   message multiplied beyond any pulpit; cannot exit the controversy -
 *   catholic_church_hierarchy: Primary target (institutional/trapped) — loses
 *   interpretive monopoly, indulgence revenue, and doctrinal gatekeeping -
 *   territorial_princes: Windfall beneficiaries turned settlement
 *   administrators (institutional/arbitrage) - manuscript_culture_artisans:
 *   Displaced payers (powerless/trapped) - vernacular_reading_public:
 *   Subsidized beneficiaries (moderate/mobile) - radical_reformers: Amplified
 *   then crushed payers (powerless/trapped) - holy_roman_emperor: Enforcement
 *   agenda-setter whose capacity repeatedly collapsed
 *   (institutional/constrained) - media_history_analysts: Analytical observer
 *   — sees the full loop from ledger and edition-count evidence
 *
 * KEY AGENTS:
 *   - reformation_printers: dual-position pivot actors (organized/constrained) — collect pamphlet profits, bear ban-and-ruin risk
 *   - reformist_theologians: primary beneficiary (powerful/identity_locked) — message multiplied; cannot exit the controversy
 *   - catholic_church_hierarchy: primary target (institutional/trapped) — loses interpretive monopoly and revenue
 *   - territorial_princes: windfall beneficiaries turned settlement administrators (institutional/arbitrage)
 *   - manuscript_culture_artisans: displaced payers (powerless/trapped)
 *   - vernacular_reading_public: subsidized beneficiaries (moderate/mobile)
 *   - radical_reformers: amplified then crushed payers (powerless/trapped)
 *   - holy_roman_emperor: enforcement agenda-setter whose capacity collapsed (institutional/constrained)
 *   - media_history_analysts: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.62).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.68).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.43).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.43).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, scaffold).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Economy / Religious-Controversy Co-Constitution Loop (Early Reformation)").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

narrative_ontology:has_sunset_clause(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '9325ad13-6466-4e46-a9bb-985002a8e7e4').
narrative_ontology:cs_kernel_codification('9325ad13-6466-4e46-a9bb-985002a8e7e4', distributed).
narrative_ontology:cs_authority_grounding('9325ad13-6466-4e46-a9bb-985002a8e7e4', expertise).
narrative_ontology:cs_interpretation_layer_present('9325ad13-6466-4e46-a9bb-985002a8e7e4').
narrative_ontology:cs_reading_relation('9325ad13-6466-4e46-a9bb-985002a8e7e4', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('9325ad13-6466-4e46-a9bb-985002a8e7e4', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('9325ad13-6466-4e46-a9bb-985002a8e7e4', foundational, causality_is_reciprocal_feedback_not_unilateral_force).
narrative_ontology:cs_axiom_status(causality_is_reciprocal_feedback_not_unilateral_force, holdable).
narrative_ontology:cs_axiom_grounding('9325ad13-6466-4e46-a9bb-985002a8e7e4', causality_is_reciprocal_feedback_not_unilateral_force, empirically_contingent).
narrative_ontology:cs_axiom('9325ad13-6466-4e46-a9bb-985002a8e7e4', secondary, agent_strategies_are_loop_components_not_loop_drivers).
narrative_ontology:cs_axiom_status(agent_strategies_are_loop_components_not_loop_drivers, holdable).
narrative_ontology:cs_axiom_grounding('9325ad13-6466-4e46-a9bb-985002a8e7e4', agent_strategies_are_loop_components_not_loop_drivers, empirically_contingent).
narrative_ontology:cs_reference_frame('9325ad13-6466-4e46-a9bb-985002a8e7e4', reciprocal_print_agency_loop).
narrative_ontology:cs_drift_state('9325ad13-6466-4e46-a9bb-985002a8e7e4', contemporary_book_history_synthesis, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9325ad13-6466-4e46-a9bb-985002a8e7e4', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformation_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformist_theologians).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, vernacular_reading_public).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, territorial_princes).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, manuscript_culture_artisans).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, radical_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, reformation_printers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, vernacular_reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Master printers who shifted their presses to reformist pamphlets, vernacular Bibles, and illustrated polemic. Pamphlet runs sold out in days and early titles returned several times their cost, but the same inventory could be banned by imperial or episcopal edict, stranding capital; several prominent printers were fined, exiled, or executed for banned editions. Exit meant reverting to liturgical and official printing at margins that could not service press debts.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformation_printers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reformation_printers, payer).

% University-trained reformers whose arguments reached audiences no pulpit or disputation could assemble. Their texts were reprinted, excerpted, translated, and pirated faster than they could revise them; they depended on printers' commercial judgment for reach and bore excommunication, imperial ban, and for some death. Leaving the controversy would have dissolved the movement each had come to embody.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformist_theologians, beneficiary,
    powerful, generational, identity_locked, continental).

% The clerical establishment whose interpretive monopoly, indulgence revenues, and doctrinal gatekeeping had rested on controlling text production and dissemination. Within a decade, unauthorized vernacular scripture and polemic circulated beyond any ban it issued; its censures multiplied while compliance fell. Adaptation — its own print offensive, reformed catechisms, index-based licensing — came late in the window and at great cost.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, global).

% Scribes, illuminators, and scriptorium workers whose livelihoods rested on hand-copying. Each year of print expansion cut the price of a reproduced page below what hand labor could meet; retraining into the print trades was possible for a few young compositors and blocked for most by guild rules and capital barriers.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, manuscript_culture_artisans, payer,
    powerless, biographical, trapped, regional).

% Urban laypeople acquiring scripture, sermons, satire, and news in their own language, much of it cheap enough for household ownership. They paid pamphlet prices that funded the loop and, as confessionalization advanced, found their reading policed by the very authorities print had empowered — visitations, oaths, and licensed title lists replacing open browsing.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, vernacular_reading_public, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, vernacular_reading_public, payer).

% Territorial rulers who watched imperial and episcopal authority fragment, taxed or confiscated church property upon adopting reform, and used print for ordinances, catechisms, and war propaganda. By the window's end they administered the settlement itself — licensing presses, dictating confession, enforcing the Augsburg freeze — having entered the window as spectators to a quarrel they did not start.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, territorial_princes, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, territorial_princes, agenda_setter).

% Anabaptist, spiritualist, and apocalyptic writers whose critiques of both the old church and magisterial reform spread through the same pamphlet channels early in the window. After the Peasants' War and Munster, both confessions used print to denounce them; their meetings were located through printed notices and their leaders executed. Exit meant flight to marginal territories or silence.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, radical_reformers, payer,
    powerless, immediate, trapped, continental).

% Charles V and his successors, who issued the bans (Worms 1521 and the interim instruments) and commissioned the enforcement that repeatedly failed. Imperial capacity was mortgaged to wars with France and the Ottomans and to electoral politics; each suspension of enforcement to buy loyalty reopened the channels the bans had closed.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, holy_roman_emperor, agenda_setter,
    institutional, generational, constrained, continental).

% Historians of the book and of the Reformation working from printer ledgers, edition counts, and library provenance. They observe the full loop — production decisions, price series, censorship records, transmission speeds — from outside any confessional seat, and their reconstructions are the evidence base this reading draws on.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, media_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Connected dispersed authors, master printers, and newly literate urban readers into a functioning market for religious information: reproducible pamphlets, standardized vernacular scripture, and illustrated polemic moved at a speed and price manuscript culture never achieved. The loop matched idle press capacity to latent demand for doctrinal participation.
% TRANSFER_FUNCTION: Moved money from pamphlet buyers to printers and authors; moved interpretive authority from the clerical hierarchy to vernacular readers and reformist publics; moved taxable wealth and jurisdiction from church institutions to territorial princes; moved reputational and physical risk onto printers whose stock could be banned.
% ABSENT_VOICES: Illiterate majorities — especially rural peasants and women outside the print trades — had no seat in the pamphlet sphere that reorganized their religious world; their objections surface only indirectly, in peasant-war grievances of 1525 and in visitation complaints recorded by the confessional states. Radical dissenters participated early and loudly, then were written out of the settlement by both confessions' print machines.
% DISAPPEARANCE_RATIONALE: Without the loop, reformist ideas circulate at manuscript speed through universities and preaching networks; there is no mass vernacular scripture movement, no pamphlet war, no rapid princely conversions; the confessional map of Europe forms differently or not at all, and printing's collapse back to liturgical and official uses leaves Rome's jurisdiction substantially intact.
% FOUNDING_PROBLEM: Early presses were expensive capital goods in a manuscript economy with thin demand for long vernacular runs; printers needed products that turned capital quickly, and the indulgence controversy supplied the first reliably sellable religious commodity. Nobody designed the loop that followed: it emerged from printers' balance sheets meeting reformers' arguments.
% FOUNDING_PROBLEM_CORROBORATION: Hostile contemporaries outside any benefiting seat attest the dynamic: Catholic controversialists such as Cochlaeus and Emser documented the commercial appetite driving heterodox publication, and imperial diet proceedings recorded enforcement failure against an industry no authority could police. Modern book-history scholarship — production ledgers, edition counts, transmission reconstructions — corroborates from outside the confessional benefiting parties. No participant designed or attests a founding plan, which is itself the co-constitution reading's claim and the reason the founding-problem status is contested rather than dead.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__co_constitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62: the loop moved substantial value out of identifiable seats — the church hierarchy's interpretive monopoly and indulgence revenues, manuscript artisans' livelihoods, radical dissenters' autonomy — while genuinely subsidizing access, movement growth, and princely jurisdiction; the aggregate is substantial but distributed, not predatory concentration. Suppression 0.68 is the end-state value: by 1555 territorial licensing, index-listing, and visitation regimes had rebuilt coercive control over print after the enforcement collapse of the 1520s. Theater_ratio 0.43: orthodoxy performance (oaths, visitations, index ritual, public burnings) grew steadily while functional production (catechisms, bibles, schoolbooks) remained dominant — below the piton threshold, consistent with a scaffold entering its sunset rather than a dead shell. Accessibility_collapse 0.40: alternatives to the loop (manuscript circulation, preaching networks, woodcut sheets, communal reading aloud) did not collapse; they persisted and interpenetrated with print, which is why the loop is enabling infrastructure rather than a totalizing order. Resistance 0.70: episcopal and imperial bans, printer evasion through false imprints and smuggling, clandestine colportage, and radical persistence under persecution — the arrangement met sustained, organized opposition throughout. The measurement series run on one shared eight-point grid (1517, 1521, 1524, 1529, 1534, 1540, 1546, 1555) so every tracked metric is authored at every examined time point. Suppression_requirement is authored as a series because enforcement capacity is the dynamic this story traces: decay from 0.74 to 0.41 as the old regime's machinery failed against an unpolicable industry, then a ratchet back to 0.68 as confessional states built new enforcement. The U-shape is not oscillation but a single decay-rebuild cycle; it is documented here rather than treated as noise. Claim/metric independence: the scaffold claim comes from the arrangement's transitional structure (an enabling loop whose open phase structurally terminated); the metrics describe its operation as measured. Per-seat engine computations may surface tangled-rope-like asymmetries at the payer seats — that divergence is signal, not error.
 *
 * PERSPECTIVAL GAP:
 *   The same loop computes as different arrangements from different seats. From the church hierarchy's position it is catastrophic dispossession: a life's monopoly on meaning dissolved in a decade by commodities it could ban but not suppress. From the printers' position it is a high-variance opportunity: fortunes made on pamphlet cycles, fortunes ruined by stranded banned inventory. From the princes' position it is a windfall that matures into administrative control — spectators to a quarrel in 1517, licensors of every press by 1555. From the reading public's position it is emancipation shading into policing: open browsing in the 1520s, oath-enforced orthodoxy by the 1540s. The engine computes this divergence from the structural data; the authored claim does not adjudicate it. Coalition note: the powerless seats (manuscript artisans, radical reformers) never combined — artisans' employment interest partially bound them to the printing workshops that displaced them, splitting any potential coalition with the persecuted dissenters.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map to directionality as follows. The church hierarchy (victim, trapped, institutional) derives near the full-target end: it bears the largest transfers and cannot exit its own doctrine. Radical reformers and manuscript artisans (victims, trapped, powerless) derive at or near full-target: they bear costs with no mobility and no leverage. Vernacular readers (beneficiary, mobile) derive near the beneficiary end: subsidized access with purchasable exit. Territorial princes (beneficiary, arbitrage) derive nearest the beneficiary end of anyone: arbitrage-grade exit (playing confessional options for land and revenue) places them at maximum subsidy. Two overrides are declared where the derivation chain would misread the seat. First, organized -> 0.45 for the printers: deriving from beneficiary-plus-constrained-exit would read roughly 0.2, but their realized position is near-symmetric — early pamphlet cycles returned multiples of cost while banned inventory, fines, and executions destroyed a substantial fraction of the trade; the honest d sits near midpoint. Second, powerful -> 0.35 for the theologians: beneficiary-plus-identity_locked would otherwise read as pure subsidy, but identity lock cuts both ways — they cannot exit the controversy that consumes them, and their reach depended on printers' commercial judgment rather than their own. Deliberately NO override is declared for the institutional power atom: three institutional seats (church, princes, emperor) diverge sharply in role and exit options, and the structural data differentiates them far better than any shared override could. Scope note: continental scope raises verification difficulty, which amplifies effective extraction on the trapped targets (church, radicals) relative to the mobile beneficiaries. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the loop as a snare would erase its genuine coordination achievement — it solved, for one transitional window, the problem of moving religious information to dispersed populations at affordable cost, and no party designed it. Reading it as a permanent rope would miss that its mandate — open controversy at scale — expired when confessionalization closed the market it fed on: once each territory had its catechism, its licensed presses, and its consistory, the loop's founding function was gone. The scaffold classification locates the arrangement in its transitional truth: enabling infrastructure whose sunset arrived not by written clause but by success — the Peace of Augsburg froze the confessional map, the Index and the Stationers' Company re-monopolized control of print, and controversy became a policed genre rather than an open market. Mandatrophy is resolved: the loop's founding function (turning controversy into circulation) is dead; what persists after 1555 is confessional print discipline, which is a different arrangement. The classification prevents the twin mislabels — mistaking a real coordination achievement for pure extraction, and mistaking a closed transitional window for a standing order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (co_constitution) of the press_reformation_causality kernel; which reading''s causal attribution — autonomous artifact, intentional strategy, or reciprocal loop — does the compiled corpus support, and what would adopting a sibling reading change structurally?',
    'Compile all three sibling files and compare per-seat classifications and epsilon profiles against the same book-history evidence base; locate divergence in beneficiary concentration and type attribution.',
    'Adopting technological_determinism would strip the human seats of directional agency and push the artifact toward mountain-like certification; adopting strategic_deployment would concentrate beneficiaries on a printer-reformer coalition and pull the story toward snare or tangled_rope; retaining co_constitution keeps the distributed beneficiary/victim structure and the scaffold claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: reading selection within the press_reformation_causality kernel.').

omega_variable(
    sunset_endogeneity,
    'Did the loop''s open phase close endogenously (controversy market saturating, demand shifting toward catechism and scripture stability) or exogenously (the enforcement ratchet finally succeeding where earlier bans failed)?',
    'Production-composition series independent of ban dates: if the polemic share of output falls while catechism, scripture, and devotional shares rise before enforcement capacity recovers, closure is endogenous; if output composition tracks the enforcement recovery, closure is exogenous.',
    'Endogenous closure confirms the scaffold''s structural sunset; exogenous closure recasts the arrangement as an open-ended process suppressed by force — nearer a broken rope than a completed scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_endogeneity, empirical, 'Whether the loop''s termination was self-limiting or imposed.').

omega_variable(
    distributed_extraction_verification,
    'Is extraction genuinely diffuse across seats, or does one seat (master printers as a class, or territorial princes) capture net gains once ruin rates and enforcement costs are netted?',
    'Printer ledger panels (bankruptcy and workshop-succession rates against pamphlet-cycle exposure) and princely fiscal records (confiscation and church-property income against war and administrative costs).',
    'Concentrated capture would re-tint the story toward tangled_rope with a named capturer seat; verified diffusion sustains the scaffold reading and the diffuse gain_flow assertion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_extraction_verification, empirical, 'Whether the loop''s gains accrued diffusely or concentrated in one seat.').

omega_variable(
    phase_decomposition_ambiguity,
    'Is the 1517-1555 span one epsilon-invariant constraint, or do the open-feedback phase (to roughly 1530) and the confessional-closure phase differ enough in structure to require two separate stories?',
    'Re-run classification on a truncated interval ending 1530: if type, epsilon, and beneficiary structure hold, one story stands; if the open phase certifies differently (lower suppression, lower theater, weaker extraction), split into open_loop and confessional_closure files linked by network edges.',
    'Splitting would date the scaffold claim to the open phase and reassign the closure phase to a distinct enforcement-order constraint; keeping one story treats closure as the scaffold''s sunset mechanics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phase_decomposition_ambiguity, conceptual, 'Possible epsilon-decomposition of the loop''s open and closure phases.').

omega_variable(
    enforcement_order_novelty,
    'Is the post-1534 enforcement recovery a restoration of the pre-1517 episcopal licensing order, or a novel confessional enforcement order (territorial licensing, indexes, visitations) with different targets and instruments?',
    'Compare enforcement targets and instruments before 1517 and after 1540: doctrinal-deviation texts versus confessional-boundary texts; ecclesiastical courts versus territorial consistories and print licensers.',
    'A restoration reading weakens world_rearranges (the world partially reverts to the old order); a novelty reading strengthens it (the loop''s closure produced a new order rather than the old one).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_order_novelty, conceptual, 'Whether the enforcement ratchet restored the old regime or built a new one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__co_constitution, theater_ratio, 1517, 0.18).
narrative_ontology:measurement_basis(pres_tr_t1517, observed).
narrative_ontology:measurement(pres_tr_t1521, press_reformation_causality__co_constitution, theater_ratio, 1521, 0.22).
narrative_ontology:measurement_basis(pres_tr_t1521, observed).
narrative_ontology:measurement(pres_tr_t1524, press_reformation_causality__co_constitution, theater_ratio, 1524, 0.24).
narrative_ontology:measurement_basis(pres_tr_t1524, observed).
narrative_ontology:measurement(pres_tr_t1529, press_reformation_causality__co_constitution, theater_ratio, 1529, 0.28).
narrative_ontology:measurement_basis(pres_tr_t1529, observed).
narrative_ontology:measurement(pres_tr_t1534, press_reformation_causality__co_constitution, theater_ratio, 1534, 0.33).
narrative_ontology:measurement_basis(pres_tr_t1534, observed).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__co_constitution, theater_ratio, 1540, 0.36).
narrative_ontology:measurement_basis(pres_tr_t1540, observed).
narrative_ontology:measurement(pres_tr_t1546, press_reformation_causality__co_constitution, theater_ratio, 1546, 0.39).
narrative_ontology:measurement_basis(pres_tr_t1546, observed).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__co_constitution, theater_ratio, 1555, 0.43).
narrative_ontology:measurement_basis(pres_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__co_constitution, base_extractiveness, 1517, 0.44).
narrative_ontology:measurement_basis(pres_be_t1517, observed).
narrative_ontology:measurement(pres_be_t1521, press_reformation_causality__co_constitution, base_extractiveness, 1521, 0.52).
narrative_ontology:measurement_basis(pres_be_t1521, observed).
narrative_ontology:measurement(pres_be_t1524, press_reformation_causality__co_constitution, base_extractiveness, 1524, 0.57).
narrative_ontology:measurement_basis(pres_be_t1524, observed).
narrative_ontology:measurement(pres_be_t1529, press_reformation_causality__co_constitution, base_extractiveness, 1529, 0.59).
narrative_ontology:measurement_basis(pres_be_t1529, observed).
narrative_ontology:measurement(pres_be_t1534, press_reformation_causality__co_constitution, base_extractiveness, 1534, 0.61).
narrative_ontology:measurement_basis(pres_be_t1534, observed).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__co_constitution, base_extractiveness, 1540, 0.62).
narrative_ontology:measurement_basis(pres_be_t1540, observed).
narrative_ontology:measurement(pres_be_t1546, press_reformation_causality__co_constitution, base_extractiveness, 1546, 0.62).
narrative_ontology:measurement_basis(pres_be_t1546, observed).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__co_constitution, base_extractiveness, 1555, 0.62).
narrative_ontology:measurement_basis(pres_be_t1555, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__co_constitution, suppression_requirement, 1517, 0.74).
narrative_ontology:measurement_basis(pres_su_t1517, observed).
narrative_ontology:measurement(pres_su_t1521, press_reformation_causality__co_constitution, suppression_requirement, 1521, 0.66).
narrative_ontology:measurement_basis(pres_su_t1521, observed).
narrative_ontology:measurement(pres_su_t1524, press_reformation_causality__co_constitution, suppression_requirement, 1524, 0.47).
narrative_ontology:measurement_basis(pres_su_t1524, observed).
narrative_ontology:measurement(pres_su_t1529, press_reformation_causality__co_constitution, suppression_requirement, 1529, 0.41).
narrative_ontology:measurement_basis(pres_su_t1529, observed).
narrative_ontology:measurement(pres_su_t1534, press_reformation_causality__co_constitution, suppression_requirement, 1534, 0.47).
narrative_ontology:measurement_basis(pres_su_t1534, observed).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__co_constitution, suppression_requirement, 1540, 0.56).
narrative_ontology:measurement_basis(pres_su_t1540, observed).
narrative_ontology:measurement(pres_su_t1546, press_reformation_causality__co_constitution, suppression_requirement, 1546, 0.63).
narrative_ontology:measurement_basis(pres_su_t1546, observed).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causality__co_constitution, suppression_requirement, 1555, 0.68).
narrative_ontology:measurement_basis(pres_su_t1555, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, resource_allocation).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% The colloquial label 'print caused the Reformation' decomposes into three epsilon-distinct constraints forming one family: this co-constitution file (the reciprocal loop as transitional enabling infrastructure; distributed extraction; epsilon 0.62; scaffold), press_reformation_causality__technological_determinism (artifact-autonomy and inevitability; near-zero extraction if a genuine natural-law claim, an FSM candidate if beneficiaries are declared), and press_reformation_causality__strategic_deployment (intentional weaponization; concentrated beneficiaries on a printer-reformer coalition; snare or tangled_rope flavor). Upstream empirical book-history findings feed all three readings; the family is linked through affects_constraints in every member file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__co_constitution, organized, 0.45).
constraint_indexing:directionality_override(press_reformation_causality__co_constitution, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
