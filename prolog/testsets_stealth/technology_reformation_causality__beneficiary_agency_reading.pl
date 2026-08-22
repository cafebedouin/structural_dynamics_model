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
 *   human_readable: Reformer-Printer Coalition Authority-Bypass Arrangement (Beneficiary-Agency Reading)
 *   domain: history of technology/religious history/media studies
 *
 * SUMMARY:
 *   This story instantiates the beneficiary-agency reading of the contested
 *   kernel technology_reformation_causality: between 1517 and 1555 a
 *   coalition of reformer theologians and commercial printers deliberately
 *   deployed the printing press to route around the Catholic Church's
 *   gatekeeping of doctrinal communication, and the press was an instrument
 *   in that strategy, not its cause. The standing arrangement under contest,
 *   and the sole referent of epsilon, is that coalition's operation: what it
 *   coordinated, whom it enriched, whom it cost. Assessed by the reading's
 *   own lights, the arrangement is a genuine but asymmetric partnership: real
 *   coordination (mass vernacular distribution neither party could achieve
 *   alone) entangled with mutual extraction (printers bore uncompensated risk
 *   for reformers' content; reformers surrendered message control to
 *   printers' commercial judgment; both jointly stripped authority-rents from
 *   the Church hierarchy). Sibling readings are different constraints with
 *   different epsilon and different victim sets; they are not averaged into
 *   this story. Claim and metrics are independent authored facts: the claimed
 *   type records the reading's structural verdict (tangled_rope); the metrics
 *   record descriptive operation, and the engine computes per-seat
 *   classifications from the structural data.
 *
 * KEY AGENTS:
 *   - reformer_theologians: Agenda-setting beneficiary (organized/identity_locked) — directs what goes to press, collects dissemination and doctrinal influence, bears no press capital or confiscation losses
 *   - commercial_printers: Dual-positioned payer/beneficiary (moderate/constrained) — advances capital and absorbs bans and unpaid debts while keeping sale revenue from demand no other line offered
 *   - catholic_church_hierarchy: Primary target (institutional/trapped) — loses gatekeeping rents and doctrinal control; cannot exit its own universal-jurisdiction claim
 *   - literate_urban_readers: Incidental beneficiary (moderate/mobile) — buys cheap vernacular access; participation voluntary and reversible
 *   - territorial_princes_and_city_councils: Beneficiary-administrator (powerful/constrained) — licenses presses, collects confiscated property and jurisdictional gains, locked to the confession already staked
 *   - manuscript_scribes_and_scriptoria: Collateral target (powerless/trapped) — lose commission work to pamphlet economics without seat or leverage
 *   - peasant_pamphleteers: Excluded voice (powerless/trapped) — used the channel in 1525, then closed out of it by both confessions' authorities
 *   - historians_of_the_book: Analytical observer (analytical/analytical) — reconstructs ledgers and print runs; adjudicates among readings with no stake in the sixteenth-century balance sheet
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.67).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.66).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition Authority-Bypass Arrangement (Beneficiary-Agency Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history of technology/religious history/media studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, '9721eede-0e2b-4076-8a12-b3c7bf2c1728').
narrative_ontology:cs_kernel_codification('9721eede-0e2b-4076-8a12-b3c7bf2c1728', distributed).
narrative_ontology:cs_authority_grounding('9721eede-0e2b-4076-8a12-b3c7bf2c1728', expertise).
narrative_ontology:cs_interpretation_layer_present('9721eede-0e2b-4076-8a12-b3c7bf2c1728').
narrative_ontology:cs_reading_relation('9721eede-0e2b-4076-8a12-b3c7bf2c1728', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('9721eede-0e2b-4076-8a12-b3c7bf2c1728', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('9721eede-0e2b-4076-8a12-b3c7bf2c1728', foundational, technology_is_instrument_not_agent).
narrative_ontology:cs_axiom_status(technology_is_instrument_not_agent, holdable).
narrative_ontology:cs_axiom_grounding('9721eede-0e2b-4076-8a12-b3c7bf2c1728', technology_is_instrument_not_agent, empirically_contingent).
narrative_ontology:cs_axiom('9721eede-0e2b-4076-8a12-b3c7bf2c1728', foundational, authority_bypass_value_capture).
narrative_ontology:cs_axiom_status(authority_bypass_value_capture, holdable).
narrative_ontology:cs_axiom_grounding('9721eede-0e2b-4076-8a12-b3c7bf2c1728', authority_bypass_value_capture, empirically_contingent).
narrative_ontology:cs_created_at('9721eede-0e2b-4076-8a12-b3c7bf2c1728', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reformer_theologians).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, commercial_printers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, literate_urban_readers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, territorial_princes_and_city_councils).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, catholic_church_hierarchy).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, commercial_printers).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, manuscript_scribes_and_scriptoria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decide which doctrinal arguments go to press, sequence the pamphlet campaigns, and supply the content that keeps presses running. Dissemination reach and doctrinal influence flow to them; press capital, paper costs, and confiscation losses do not. Publication under papal and imperial ban made recantation the only way out, and their public standing was constituted by the quarrel itself, so leaving meant silencing the movement and themselves with it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, reformer_theologians, agenda_setter,
    organized, generational, identity_locked, continental).

% Operate presses, advance paper and labor, and absorb confiscations, banning edicts, and reformers' unpaid debts; in exchange they sell into demand no other product line offered and keep the sale revenue, since authors rarely held royalties. Capital sunk in type and presses, plus municipal licences that tie permission to confessional alignment, means leaving the trade means writing off the shop; switching patrons is the realistic move.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, commercial_printers, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, commercial_printers, beneficiary).

% Holds the gatekeeping position the coalition routes around: doctrinal authorization, indulgence revenue, clerical mediation of scripture. Its countermeasures, from the Worms ban through catalogue-style prohibitions and prosecutions of printers, cost money and credibility while the bypass channel outruns interdiction. It cannot abandon its own claim to universal jurisdiction, so every routed-around transaction is a loss it must answer publicly.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, catholic_church_hierarchy, payer,
    institutional, generational, trapped, continental).

% Buy cheap quartos and broadsheets and gain direct access to scripture and polemic previously mediated by clergy. Participation is voluntary and reversible: a reader tired of controversy stops buying, changes genre, or attends differently. They fund the arrangement without governing it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, literate_urban_readers, beneficiary,
    moderate, biographical, mobile, regional).

% Licence presses, appoint censors, and decide which confession may print legally inside their territory. Confiscated church property, jurisdictional gains, and a compliant licensed press flow to them. Having staked legitimacy on a confession, reversal threatens their settlements with neighbors and subjects, so their administration of the trade is welded to choices already made.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, territorial_princes_and_city_councils, beneficiary,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, territorial_princes_and_city_councils, agenda_setter).

% Lose commission work as pamphlet economics undercut hand copying. Conversion to compositor work was slow and their guild position weak; they bear the displacement costs of a trade they neither chose nor govern.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, manuscript_scribes_and_scriptoria, payer,
    powerless, biographical, trapped, regional).

% Used the same pamphlet channel in 1525 to publish the Twelve Articles and agrarian grievances; after the war's suppression, both confessions' authorities barred them from print and burned their pamphlets. They demonstrated the channel's openness and were then closed out of it, the clearest constituency the arrangement cannot hear.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, peasant_pamphleteers, excluded,
    powerless, immediate, trapped, regional).

% Reconstruct print runs, ledgers, and confiscation records centuries later; they test the coalition's self-descriptions against production data and weigh rival explanations of why the arrangement moved as it did. No stake rides on the sixteenth-century balance sheet.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, historians_of_the_book, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, commercial_printers).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Connects dispersed reform-minded audiences with standardized vernacular doctrinal content at a speed manuscript reproduction could not reach: printers contribute capital, compositing skill, and distribution networks; reformers contribute content, public legitimacy, and guaranteed demand; municipal authorities contribute legal permission. No party could produce mass religious communication alone.
% TRANSFER_FUNCTION: Moves doctrinal content and legitimation from reformer theologians through printers to reading publics; moves cash from buyers to printers, since authors rarely held royalties; moves risk downward onto printers as confiscations and bans; moves authority-rents, including indulgence revenue, doctrinal gatekeeping, and clerical mediation, away from the Church hierarchy toward the coalition and the territorial rulers who license and tax what replaces it.
% ABSENT_VOICES: Peasant pamphleteers would object loudest: they proved the channel open in 1525 with the Twelve Articles and were then shut out of print by both confessions' authorities after the war's suppression. Women, largely outside Latin schooling and press patronage, are absent from the coalition's editorial decisions. Catholic controversialists appear only as targets to be answered, never as parties to be heard. Each sits outside the arrangement's decision surface: peasants post-suppression, women structurally, Catholics by coalition design.
% DISAPPEARANCE_RATIONALE: If the coalition arrangement vanished overnight, reform arguments circulate at manuscript speed and at source-interdictable scale; the Church's gatekeeping holds for decades longer; the pamphlet economy that funded and spread the movement never forms; territorial rulers lose the confiscated-property and jurisdiction windfalls that anchored confessional state-building. European religious geography rearranges around slower, controllable channels.
% FOUNDING_PROBLEM: Disseminate doctrinal critique faster than a unified Church authority could suppress it, given that manuscript reproduction was slow, costly, and easily interdicted at the copying source.
% FOUNDING_PROBLEM_CORROBORATION: The strongest corroboration comes from the injured party: the Church's own countermeasures, including the Worms ban, successive banning edicts, index-style catalogues, and printer prosecutions, are institutional testimony that the bypass problem was live and feared. Imperial police and city-council records corroborate independently. Modern book-history scholarship, built on production statistics and printer ledgers, attests from outside all benefiting parties that the specific gatekeeping problem ended with the confessional settlement, even as the generic problem of unsuppressible dissemination recurs in later media regimes.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.67, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction sits at 0.67 because the arrangement's value derived substantially from displacing the Church's gatekeeping rents and distributing them among coalition insiders, with printers additionally absorbing risk shifted down from reformers. Suppression at 0.66 is structural throughout: municipal licensing, confiscation, and confessional press controls, not internalized belief. Theater at 0.41 reflects a growing share of pious framing (service to the Gospel, edifying intent) laid over commercial and political motive as the original bypass function institutionalized. Accessibility_collapse is low-moderate (0.42): manuscript, oral, and image channels persisted alongside print, so alternatives narrowed but never collapsed. Resistance at 0.58 records the Church's counter-offensive, printer debt litigation, and post-1525 suppression of peasant print. All three temporal series share one nine-point grid so no metric is sampled against another's gaps. The extractiveness curve is cyclical rather than monotonic: output peaked in the early 1520s, but extraction peaked later, because each crisis (Worms, the Peasants' War, the Schmalkaldic War and Interim) raised enforcement stakes and let surviving insiders consolidate the displaced rents, while detente intervals compressed them. The oscillation functions partly as an extraction mechanism: crisis-driven consolidation is how market share and licensing privileges concentrated. Base properties are measured at interval end (t=38), the post-settlement consolidation phase. Identity-lock dynamics bind the reformer seat: publication under ban fused professional and ideological identity, making recantation the only exit and therefore unthinkable; break that frame and the seat shifts from locked beneficiary to mobile author. Manuscript scribes illustrate the coalition-power gap among those who bore costs: dispersed, skill-specific, and guildless, they mounted no collective response, unlike the institutional Church, whose resistance was formidable but aimed at a channel it could not interdict.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute divergent types from identical structure. From the reformer seat the arrangement is liberation infrastructure it directed; from the printer seat it is a risk-laden dependency in which content suppliers set terms and pushed losses downward; from the Church seat it is expropriation of a jurisdictional estate; from the reader seat it is cheap access voluntarily purchased; from the prince seat it is an instrument of sovereignty that arrived conveniently pre-built. Inter-institutionally, the Church and the princes hold comparable formal power yet opposite structural relationships: the Church is trapped by its own universal claim, while princes converted the same bypass into enforceable territorial advantage. Same-level differentiation appears between the two insider seats: reformers exit only through identity death (identity_locked), printers through ruinous capital conversion (constrained), which is why equal participation yields unequal exposure. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. Reformer theologians: agenda-setting beneficiaries with identity-locked exit, strongly subsidized on the benefit side, though their extraction of printer risk tempers the subsidy. Commercial printers: declared both beneficiary and victim; their derived d sits mid-range, with the net position left to the printer_net_position_ambiguity omega rather than resolved by assertion. Catholic Church hierarchy: full target, trapped exit, continental scope, the highest-d seat, amplified by scope-driven verification difficulty. Literate urban readers: near-beneficiary, mobile exit, the lowest realistic d among human seats. Territorial princes and city councils: beneficiary-administrators, positioned between pure beneficiary and agenda-setter. Manuscript scribes: collateral targets with trapped exit, high d despite receiving nothing directly, since the arrangement's operation destroys their market. Peasant pamphleteers are authored as excluded (R3: commentary-grade absence; their exclusion never drives classification overrides).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, unsuppressible dissemination against a unified gatekeeper, was resolved by the confessional settlement: by 1555 territorial churches ran their own licensed presses, and the bypass channel served new gatekeepers. The mandate outlived its function, so mandatrophy_resolved is declared. Classification discipline prevents both mislabels: reading the whole arrangement as pure extraction (snare) erases the genuine coordination that made mass vernacular religion communicable at all; reading it as pure coordination (rope) erases the Church's expropriated rents and the printers' uncompensated losses. Tangled rope holds both halves. The mismatch consumer will read founding_problem_status=dead against disappearance_verdict=world_rearranges and flag zombie persistence, which is the honest finding: the machinery persisted past its mandate into state-confessional service. Whether print's coordination role was always destined for retirement (a hidden sunset, per the infrastructure_scaffold_status omega) remains open inside this classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_epsilon_indexicality,
    'This constraint is one reading of the kernel technology_reformation_causality; would instantiating the technological_determinism or co_constitution reading yield a different epsilon and a different victim set over the same historical surface?',
    'Generate the sibling reading files and compare computed classifications; divergence in epsilon, victim sets, or type marks the structural location of the kernel dispute.',
    'If siblings converge on the same type and victim set, the kernel contest is merely interpretive; if they diverge structurally, the colloquial label ''print and the Reformation'' covers multiple distinct constraints and the family decomposition stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_epsilon_indexicality, conceptual, 'Committer-frame routing: reading-indexed epsilon over a shared historical referent.').

omega_variable(
    printer_net_position_ambiguity,
    'Were commercial printers net beneficiaries or net payers across the interval once confiscations, unpaid reformer debts, and boom margins are netted?',
    'Printer ledger reconstruction: bankruptcy and confiscation rates against margin data for reform-commissioned stock versus safe stock, using city-archive loss records and surviving account books.',
    'A net-beneficiary outcome dampens the printer seat toward subsidy and strengthens the mutual-gain half of the arrangement; a net-payer outcome pushes the printer seat toward full target and flavors the whole computation toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_net_position_ambiguity, empirical, 'Whether the printer seat''s dual position nets out positive or negative.').

omega_variable(
    bypass_value_share,
    'How much of the coalition''s measured value derived from authority-bypass specifically, as opposed to ordinary commercial expansion of the book trade?',
    'Compare margins and volumes on indulgence-critical and banned titles against contemporaneous safe genres (schoolbooks, almanacs, broadsheets) produced by the same shops.',
    'A high bypass share supports epsilon deriving from authority-bypass value as this reading claims; a low share collapses this story toward a generic print-market account with far lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bypass_value_share, empirical, 'Decomposing coalition value into bypass rent versus ordinary commerce.').

omega_variable(
    infrastructure_scaffold_status,
    'Was print''s coordination role within the coalition transitional support, retired once territorial churches built their own distribution and licensing apparatus, or permanent infrastructure?',
    'Trace whether post-1555 print governance replaced coalition coordination (consistorial and territorial control of presses) or merely rebranded it.',
    'A transitional finding would split this story: the coalition phase carrying a de facto sunset, the infrastructure phase becoming a separate constraint; failure to resolve leaves the sunset question open inside the current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(infrastructure_scaffold_status, conceptual, 'Whether the technology component carried a hidden sunset clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(tech_tr_t4, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(tech_tr_t8, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(tech_tr_t12, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(tech_tr_t16, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(tech_tr_t20, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(tech_tr_t25, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 25, 0.33).
narrative_ontology:measurement(tech_tr_t30, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 30, 0.37).
narrative_ontology:measurement(tech_tr_t38, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 38, 0.41).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(tech_be_t4, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(tech_be_t8, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(tech_be_t12, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(tech_be_t16, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(tech_be_t20, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(tech_be_t25, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(tech_be_t30, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(tech_be_t38, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 38, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(tech_su_t4, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 4, 0.47).
narrative_ontology:measurement(tech_su_t8, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 8, 0.53).
narrative_ontology:measurement(tech_su_t12, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(tech_su_t16, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 16, 0.58).
narrative_ontology:measurement(tech_su_t20, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(tech_su_t25, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement(tech_su_t30, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(tech_su_t38, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 38, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'printing press and the Reformation' decomposes into three epsilon-invariant readings of one kernel. This file is the beneficiary-agency member; the determinism member assigns causality to the press itself and dissolves coalition responsibility into technological necessity; the co-constitution member distributes agency bidirectionally across the human-technical assemblage. This member is upstream in evidentiary confidence (agent-level decision records, printer ledgers) and is cited within both sibling debates. The 'technology as scaffold' component noted in the expected structural delta is routed to the infrastructure_scaffold_status omega rather than forced into this story's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
