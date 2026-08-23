% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Strategic Print Deployment (Beneficiary-Agency Reading)
 *   domain: historical/media/religious
 *
 * SUMMARY:
 *   This story instantiates the beneficiary_agency_reading of the
 *   technology_reformation_causality kernel: between 1517 and 1555 a
 *   coalition of reformer-theologians and commercial printers deliberately
 *   deployed the printing press to route around church gatekeeping, and the
 *   technology's role is explained by what agents did with it, not by the
 *   technology acting as an autonomous cause. The constraint under assessment
 *   is that standing arrangement — the strategic-deployment coalition — and
 *   epsilon is authored for that arrangement as this reading sees it: the
 *   arrangement extracted the value of doctrinal gatekeeping from the church
 *   hierarchy's position and converted it into movement reach and printer
 *   profit, while simultaneously solving a genuine movement-coordination
 *   problem no other channel could solve at the time. Sibling readings
 *   (technological_determinism_reading, co_constitution_reading) are separate
 *   constraints with their own epsilon values and are not averaged into this
 *   one; the press-infrastructure-as-transitional-support idea flagged in the
 *   kernel delta is routed to an omega rather than folded in here. Claim and
 *   metrics are authored independently: the reading claims a tangled_rope
 *   structure (genuine coordination plus asymmetric extraction, actively
 *   enforced), and the metrics describe the arrangement's observed operation
 *   without being tuned to that claim.
 *
 * KEY AGENTS:
 *   - reformer_theologians: agenda-setting core of the coalition (organized/identity_locked) — set the strategy of taking disputes to a reading public; collected the dissemination value; exit meant recantation, refused at Worms
 *   - commercial_printers: administering beneficiaries (moderate/constrained, immediate horizon) — ran the pipeline, captured the commercial gains, bore boom-bust and confiscation risk
 *   - church_hierarchy: the seat the arrangement extracted from (institutional/identity_locked) — held the gatekeeping position being routed around; could not concede it without dissolving itself
 *   - territorial_princes: opportunistic beneficiaries (powerful/mobile) — converted the realignment into jurisdiction and revenue; their protection decided where the coalition could operate
 *   - vernacular_reading_public: diffuse beneficiaries with indirect costs (powerless/constrained) — gained unmediated access; paid in prices and polarization
 *   - humanist_moderates: excluded voices (moderate/constrained) — wanted non-polemical print; marginalized by the pamphlet war
 *   - historiographical_analysts: analytical observers (analytical/analytical) — reconstruct production and circulation; adjudicate among the kernel's readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.58).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.5).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Strategic Print Deployment (Beneficiary-Agency Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "historical/media/religious").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, '86675933-2d2d-4ebd-8a9f-f6a21d69b9a9').
narrative_ontology:cs_kernel_codification('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', distributed).
narrative_ontology:cs_authority_grounding('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', expertise).
narrative_ontology:cs_interpretation_layer_present('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9').
narrative_ontology:cs_reading_relation('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', foundational, technology_as_strategic_instrument).
narrative_ontology:cs_axiom_status(technology_as_strategic_instrument, holdable).
narrative_ontology:cs_axiom_grounding('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', technology_as_strategic_instrument, empirically_contingent).
narrative_ontology:cs_axiom('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', foundational, coalition_interest_explains_deployment).
narrative_ontology:cs_axiom_status(coalition_interest_explains_deployment, holdable).
narrative_ontology:cs_axiom_grounding('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', coalition_interest_explains_deployment, empirically_contingent).
narrative_ontology:cs_reference_frame('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', strategic_instrumentality).
narrative_ontology:cs_drift_state('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', contemporary_print_history, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('86675933-2d2d-4ebd-8a9f-f6a21d69b9a9', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reformer_theologians).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, commercial_printers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, territorial_princes).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, vernacular_reading_public).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, church_hierarchy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, vernacular_reading_public).
narrative_ontology:constraint_vindicates(technology_reformation_causality__beneficiary_agency_reading, technology_as_strategic_instrument).
narrative_ontology:constraint_vindicates(technology_reformation_causality__beneficiary_agency_reading, agent_centered_media_causality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wrote the pamphlets, sermons, and translations the coalition circulated, and set the strategy of taking disputes directly to a reading public rather than through church courts and universities. Gained a continent-wide audience and the protection of sympathetic cities; bore excommunication, outlawry, and the pressure of the pamphlet war. Leaving the movement meant recantation, which its leaders refused publicly at Worms.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, reformer_theologians, agenda_setter,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, reformer_theologians, beneficiary).

% Ran the presses, chose formats and print runs, and moved pamphlets through the book-fair network. The controversy gave them the first true mass-market product after a decade of slack demand; profits peaked at the boom's height, while the risks were confiscation, bans, and a saturated market that bankrupted many houses by the 1530s. They could shift output to other goods in principle, but capital, workshops, and reform-city locations tied them to the trade.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, commercial_printers, beneficiary,
    moderate, immediate, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, commercial_printers, agenda_setter).

% Held the gatekeeping position the coalition routed around: licensing of doctrine, control of pulpits and universities, and the indulgence and benefice revenues that vernacular pamphlets attacked. Responded with bans, confiscations, a prohibited-books index, and eventually war. Conceding vernacular unmediated access would have dissolved its own mediating role, so it fought the channel rather than joining it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, church_hierarchy, payer,
    institutional, generational, identity_locked, continental).

% Used the controversy to assert jurisdiction over church property and courts in their territories, and protected or suppressed printers as their interests dictated. Gained leverage and revenue from the realignment; their protection largely decided where the coalition could operate, and their support could be withdrawn at will.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, territorial_princes, beneficiary,
    powerful, generational, mobile, regional).

% Urban laypeople who bought or heard read the pamphlets, woodcuts, and vernacular Bibles. Gained direct access to scripture and polemic without clerical mediation; paid for the pamphlets, absorbed the confessional polarization that narrowed later choices, and in some territories faced renewed compulsion when authorities flipped confession.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, vernacular_reading_public, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, vernacular_reading_public, payer).

% Scholarly reformers in the Erasmus circle who wanted print used for edited sources and measured critique rather than confessional war. Drowned out by the pamphlet boom, pressured to declare sides, and marginalized in both camps; several withdrew from publishing altogether.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, humanist_moderates, excluded,
    moderate, biographical, constrained, continental).

% Modern historians of print and the Reformation who reconstruct production runs, printer contracts, and circulation from archives; they adjudicate among competing causal accounts of the episode and supply the evidence base for and against each reading of it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, historiographical_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, reformer_theologians).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a movement-coordination problem: a dispersed reform cause needed to circulate identical doctrine, answer opponents quickly, and reach literate laypeople across hundreds of kilometers faster than authorities could respond. Print gave the coalition a shared channel that preaching networks and manuscript copy could not match for speed and volume.
% TRANSFER_FUNCTION: Moved vernacular religious content from reformer authors through commercial printers to urban reading publics; moved the value of doctrinal gatekeeping from the church hierarchy's position to the coalition; moved money from pamphlet buyers to printers; and moved jurisdictional leverage to territorial princes who sheltered the trade.
% ABSENT_VOICES: Humanist moderates who wanted non-polemical print, catholic authors and printers whose works were confiscated in reform cities, and the largely illiterate rural majorities for whom the pamphlet wave meant little directly — all outside the coalition's terms. The coalition's agreement about print's strategic value was formed without them in the conversation.
% DISAPPEARANCE_RATIONALE: Without the strategic print deployment, the reform cause coordinates through preaching, university disputations, and manuscript copy — slower, costlier, and far easier to contain locally. The schism's speed, shape, and territorial spread would have been radically different; the printer boom-bust cycle never occurs; and the church hierarchy's gatekeeping position is not dissolved on the same schedule, if at all.
% FOUNDING_PROBLEM: Two closed positions met after 1517-1519: reformers were locked out of official channels (pulpit, university, manuscript patronage), and printers were emerging from a decade of depressed demand needing a mass-market product. The arrangement was built to solve both at once — open a dissemination channel the authorities could not close, and fill the presses with a product readers would buy.
% FOUNDING_PROBLEM_CORROBORATION: Hostile contemporaries attest both the problem and its solution: the Edict of Worms and papal nuncio Aleander's dispatches describe the pamphlet flood as a deliberate channel the bans failed to close. Modern print histories — production-run reconstructions and printer bankruptcy records — corroborate the market side from outside the beneficiary set. No beneficiary self-assertion is load-bearing for the status: the channel was opened, the market was filled and then busted, and both facts are attested by non-beneficiary sources.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Epsilon (0.58 at interval end) derives from the authority-bypass value: the arrangement's distinctive product was doctrinal dissemination without gatekeeper consent, and the series shows that value peaking at the height of the pamphlet boom (0.70, 1524) when the bypass was most load-bearing, dipping after the Peasants' War strained the coalition, and settling near 0.58 as confessional territories normalized their own print regimes. Suppression (0.50) is authored as a raw structural property and is not scaled by power or scope: the arrangement required active defense — Worms-ban evasion, city print privileges, confiscation-risk pricing — but it competed with the church's voice rather than silencing it, so its coercive overhead sits mid-range. Theater (0.15) is low because the pamphlets did the work; the modest rise after 1525 marks apologetic self-justification displacing some dissemination. Accessibility collapse (0.50): alternatives — manuscript circulation, preaching networks, the church's own counter-print — remained partly available throughout. Resistance (0.65): bans, index enforcement, and one war were mounted against the channel. All three series run on one shared nine-point grid. The oscillation in the extractiveness series is not noise: each enforcement wave raised the scarcity value of forbidden print and revived the bypass premium (an intermittent-reinforcement dynamic), while market saturation pulled it down — the cycle is partly the extraction mechanism itself, and the base_properties scalars reflect the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the church_hierarchy seat the arrangement is pure loss — its mediating position dissolved by a channel it could not join without ceasing to be itself, and its identity-locked exit drives that seat toward the full-target end. From the coalition seats the same structure is the coordination that made the movement possible. Inside the coalition the seats split again: reformers on doctrinal horizons experienced the trade as mission; printers on immediate commercial horizons experienced it as a boom that bankrupted many of them within a decade. Territorial princes experienced it as opportunity with optionality — the only seat that could take the gains and exit the risks. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries declared: reformer_theologians, commercial_printers, territorial_princes, vernacular_reading_public — all sit toward the beneficiary end of d, with the reading public near symmetric because its gains (access) and costs (prices, polarization) roughly offset. Victim declared: church_hierarchy — identity-locked to the gatekeeping position the arrangement dissolves, so it sits at the full-target end and its effective extraction is amplified. No directionality overrides are needed: the beneficiary/victim declarations plus exit options reproduce the true structural relationships. The mutual-extraction character the kernel delta predicts shows up inside the coalition: printers' immediate horizons and constrained exit mean the coalition's gains flowed through them at real risk, which keeps their d from sitting at the pure-beneficiary end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — closed official channels for reformers, depressed demand for printers — was solved by the arrangement's own success and was dead by interval end: channels reorganized confessionally, and the pamphlet market matured and then busted. The mismatch consumer will read dead-founding-problem plus world_rearranges as a capture/zombie candidate; the honest resolution here is completion, not capture — the arrangement wound down with its function (pamphlet production collapsed after 1525 and again in the 1540s; the strategic coalition dissolved into ordinary confessional publishing) rather than persisting as theater, which the low theater_ratio (0.15) corroborates. The classification prevents two mislabels: a determinist framing would naturalize the arrangement as an inevitable mountain — no agents, no extraction, nothing to attribute; a pure-rope framing would miss the asymmetric extraction from the church hierarchy's position. Tangled_rope holds both: genuine coordination, real extraction, active enforcement, and a dissolvable rather than zombified structure. The press-as-transitional-infrastructure idea belongs to a candidate sibling story (see omega press_infrastructure_scaffold_split), not to this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This story instantiates the beneficiary_agency_reading of the technology_reformation_causality kernel; what would change if the technological_determinism_reading were adopted instead?',
    'Historiographical adjudication on the inevitability question: if manuscript-channel capacity or preaching networks could have carried equivalent dissemination, the strategic-deployment account loses its load-bearing claim and the determinist reading''s naturalized framing takes over.',
    'Under the determinist reading the arrangement would be framed as inevitable infrastructure rather than a constructed, enforced coalition — extraction analysis collapses toward zero (nothing constructed to extract with), and the tangled_rope classification would not be available.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this constraint is one reading of the causality kernel; sibling readings instantiate structurally different constraints.').

omega_variable(
    press_infrastructure_scaffold_split,
    'Does part of the authority-bypass value belong to the printing infrastructure itself rather than to the coalition arrangement — i.e., should the press-as-deployed be authored as a separate transitional-support story with its own epsilon and a settlement-date sunset?',
    'Decompose per the epsilon-invariance rule: author a sibling story for the press infrastructure (decisive while the bypass was contested, ordinary confessional infrastructure after territorial settlement) and link the two via network edges.',
    'If split, this story''s epsilon drops by the infrastructure share and the infrastructure story carries the transitional-support classification; the current single story slightly overstates coalition extractiveness by folding in the technology''s share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_infrastructure_scaffold_split, conceptual, 'The scaffold-character of the technology is a candidate sibling story, deliberately not folded into this one.').

omega_variable(
    coalition_internal_extraction_symmetry,
    'How symmetric was the mutual extraction inside the coalition — did printers bear net costs (boom-bust bankruptcies, confiscation losses) that the reformers'' gains were extracted through, or were internal flows roughly reciprocal?',
    'Printer account books, production-run data, and bankruptcy records for 1518-1535, compared against authors'' honoraria and movement subsidies.',
    'If printers were net internal victims, the arrangement''s internal structure shifts toward extraction-from-printers with coordination-for-reformers, sharpening the tangled_rope reading; if reciprocal, the extraction is almost entirely external, against the church hierarchy''s position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_internal_extraction_symmetry, empirical, 'Internal symmetry of the reformer-printer exchange.').

omega_variable(
    manuscript_counterfactual_capacity,
    'Could manuscript circulation and preaching networks have carried the reform''s dissemination load without the strategic print deployment?',
    'Compare measured manuscript-circulation rates and preaching-network reach in 1510-1520 against the pamphlet wave''s measured reach in 1518-1524.',
    'If manuscript channels were near-sufficient, the bypass value — and with it epsilon — collapses toward ordinary advocacy; if far insufficient, the arrangement was the load-bearing coordination structure this reading claims it was.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(manuscript_counterfactual_capacity, empirical, 'Counterfactual capacity of non-print channels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1517, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement_basis(tech_tr_t1517, observed).
narrative_ontology:measurement(tech_tr_t1521, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1521, 0.12).
narrative_ontology:measurement_basis(tech_tr_t1521, observed).
narrative_ontology:measurement(tech_tr_t1524, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1524, 0.14).
narrative_ontology:measurement_basis(tech_tr_t1524, observed).
narrative_ontology:measurement(tech_tr_t1525, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1525, 0.2).
narrative_ontology:measurement_basis(tech_tr_t1525, observed).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1530, 0.18).
narrative_ontology:measurement_basis(tech_tr_t1530, observed).
narrative_ontology:measurement(tech_tr_t1534, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1534, 0.16).
narrative_ontology:measurement_basis(tech_tr_t1534, observed).
narrative_ontology:measurement(tech_tr_t1541, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1541, 0.15).
narrative_ontology:measurement_basis(tech_tr_t1541, observed).
narrative_ontology:measurement(tech_tr_t1546, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1546, 0.17).
narrative_ontology:measurement_basis(tech_tr_t1546, observed).
narrative_ontology:measurement(tech_tr_t1555, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1555, 0.15).
narrative_ontology:measurement_basis(tech_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t1517, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1517, 0.45).
narrative_ontology:measurement_basis(tech_be_t1517, observed).
narrative_ontology:measurement(tech_be_t1521, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1521, 0.62).
narrative_ontology:measurement_basis(tech_be_t1521, observed).
narrative_ontology:measurement(tech_be_t1524, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1524, 0.7).
narrative_ontology:measurement_basis(tech_be_t1524, observed).
narrative_ontology:measurement(tech_be_t1525, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1525, 0.6).
narrative_ontology:measurement_basis(tech_be_t1525, observed).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1530, 0.56).
narrative_ontology:measurement_basis(tech_be_t1530, observed).
narrative_ontology:measurement(tech_be_t1534, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1534, 0.54).
narrative_ontology:measurement_basis(tech_be_t1534, observed).
narrative_ontology:measurement(tech_be_t1541, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1541, 0.55).
narrative_ontology:measurement_basis(tech_be_t1541, observed).
narrative_ontology:measurement(tech_be_t1546, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1546, 0.57).
narrative_ontology:measurement_basis(tech_be_t1546, observed).
narrative_ontology:measurement(tech_be_t1555, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1555, 0.58).
narrative_ontology:measurement_basis(tech_be_t1555, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1517, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1517, 0.25).
narrative_ontology:measurement_basis(tech_su_t1517, observed).
narrative_ontology:measurement(tech_su_t1521, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1521, 0.55).
narrative_ontology:measurement_basis(tech_su_t1521, observed).
narrative_ontology:measurement(tech_su_t1524, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1524, 0.6).
narrative_ontology:measurement_basis(tech_su_t1524, observed).
narrative_ontology:measurement(tech_su_t1525, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1525, 0.7).
narrative_ontology:measurement_basis(tech_su_t1525, observed).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1530, 0.62).
narrative_ontology:measurement_basis(tech_su_t1530, observed).
narrative_ontology:measurement(tech_su_t1534, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1534, 0.6).
narrative_ontology:measurement_basis(tech_su_t1534, observed).
narrative_ontology:measurement(tech_su_t1541, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1541, 0.58).
narrative_ontology:measurement_basis(tech_su_t1541, observed).
narrative_ontology:measurement(tech_su_t1546, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1546, 0.72).
narrative_ontology:measurement_basis(tech_su_t1546, observed).
narrative_ontology:measurement(tech_su_t1555, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1555, 0.5).
narrative_ontology:measurement_basis(tech_su_t1555, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, identity_coordination).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, co_constitution_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'printing caused the Reformation' decomposes per the epsilon-invariance principle into structurally distinct claims, each its own constraint: the determinist claim (naturalized inevitability — negligible extraction, mountain-adjacent), the co-constitution claim (mutual shaping — moderate and framing-dependent), and this beneficiary-agency claim (a constructed, enforced coalition with asymmetric extraction — tangled_rope). The determinist claim is upstream in the literature: it is the claim the other two answer, and both cite the production and circulation record against it. The press-as-transitional-infrastructure idea is a candidate fourth story with a settlement-date sunset and is routed to omega press_infrastructure_scaffold_split rather than folded into this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
