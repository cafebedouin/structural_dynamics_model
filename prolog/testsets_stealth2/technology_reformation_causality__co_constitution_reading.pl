% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Print-Reformation Co-Constitution Arrangement (Co-Constitution Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Between Gutenberg's bible (c. 1455) and the settled confessional print
 *   economies of 1600, print capacity and religious reform co-produced each
 *   other: presses multiplied because reform controversy created demand no
 *   scribal economy could meet, and reform took the shape it did because
 *   presses made cheap identical text, pamphlet speed, and vernacular
 *   scripture possible. This story instantiates the co_constitution_reading
 *   of the technology_reformation_causality kernel: the press enabled but did
 *   not determine, the reformers shaped what the press produced but lost
 *   steering control to market selection, and the arrangement's costs arise
 *   from the interaction of the two rather than from either component alone.
 *   The epsilon referent is the standing early-modern print-reform
 *   arrangement itself, assessed by this reading's own lights - not the
 *   inevitability the determinism sibling asserts or the pure-tool relation
 *   the agency sibling asserts. KEY AGENTS (by structural relationship): -
 *   commercial_printers: Split seat, winning cohort primary beneficiary
 *   (organized/constrained) - collects the boom; losing cohort appears
 *   separately below - reformist_publishing_entrepreneurs: Aligned
 *   beneficiary (organized/identity_locked) - windfall collectors fused with
 *   the cause - wittenberg_reformer_faculty: Agenda setter with payer
 *   exposure (institutional/identity_locked) - supplies and steers content,
 *   cannot own or fully control it - pirated_humanist_authors: Target
 *   (moderate/constrained) - bears uncompensated reprinting -
 *   insolvent_printer_households: Target (powerless/trapped) - bears ruin
 *   risk at the tail - vernacular_reading_public: Net beneficiary with
 *   diffuse costs (moderate/mobile) - subsidized access, smuggling premiums
 *   in banned territories - fiscally_tolerant_territorial_princes: Incidental
 *   beneficiary and selective enforcer (powerful/arbitrage) - collects
 *   revenue from a trade their edicts forbid - roman_curia_gatekeepers:
 *   Powerful target (institutional/identity_locked) - pays in authority and
 *   reach, not coin - scriptoria_and_itinerant_preachers: Displaced and
 *   voiceless (powerless/trapped) - the atrophied alternative channels -
 *   historiographers_of_print: Analytical observer (analytical/analytical) -
 *   sees the full coupling from outside. Family note: the colloquial question
 *   'did print cause the Reformation?' decomposes into three
 *   epsilon-invariant constraints linked by network.affects_constraints; this
 *   file is the co-constitution member.
 *
 * KEY AGENTS:
 *   - - commercial_printers: Split seat - winning cohort primary beneficiary (organized/constrained), losing cohort insolvent (powerless/trapped); the same population sorted by the coupling's risk structure
 *   - - reformist_publishing_entrepreneurs: Aligned beneficiary (organized/identity_locked) - houses built on reformist titles, exit means betraying the cause
 *   - - wittenberg_reformer_faculty: Agenda setter with payer exposure (institutional/identity_locked) - authors and vetters whose steering authority eroded as the market took over selection
 *   - - pirated_humanist_authors: Target (moderate/constrained) - uncompensated, uncorrectable reprinting of their works
 *   - - insolvent_printer_households: Target (powerless/trapped) - ruin risk concentrated on craft-specific sunk capital
 *   - - vernacular_reading_public: Net beneficiary with diffuse costs (moderate/mobile) - cheap access, smuggling premiums where bans bind
 *   - - fiscally_tolerant_territorial_princes: Incidental beneficiary and selective enforcer (powerful/arbitrage) - taxes the trade its edicts ban
 *   - - roman_curia_gatekeepers: Powerful target (institutional/identity_locked) - pays in lost gatekeeping authority; responds with counter-press rather than participation
 *   - - scriptoria_and_itinerant_preachers: Displaced and voiceless (powerless/trapped) - the manuscript-and-oral channels print economics cannibalized
 *   - - historiographers_of_print: Analytical observer (analytical/analytical) - reconstructs the whole coupling from production and adoption data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.58).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.5).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Print-Reformation Co-Constitution Arrangement (Co-Constitution Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '8c3d9747-7b08-4ae0-8263-14eff2e24300').
narrative_ontology:cs_kernel_codification('8c3d9747-7b08-4ae0-8263-14eff2e24300', distributed).
narrative_ontology:cs_authority_grounding('8c3d9747-7b08-4ae0-8263-14eff2e24300', expertise).
narrative_ontology:cs_interpretation_layer_present('8c3d9747-7b08-4ae0-8263-14eff2e24300').
narrative_ontology:cs_reading_relation('8c3d9747-7b08-4ae0-8263-14eff2e24300', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('8c3d9747-7b08-4ae0-8263-14eff2e24300', technology_reformation_causality__beneficiary_agency_reading, forecloses).
narrative_ontology:cs_axiom('8c3d9747-7b08-4ae0-8263-14eff2e24300', foundational, causality_is_bidirectional_in_print_reformation_coupling).
narrative_ontology:cs_axiom_status(causality_is_bidirectional_in_print_reformation_coupling, holdable).
narrative_ontology:cs_axiom_grounding('8c3d9747-7b08-4ae0-8263-14eff2e24300', causality_is_bidirectional_in_print_reformation_coupling, empirically_contingent).
narrative_ontology:cs_axiom('8c3d9747-7b08-4ae0-8263-14eff2e24300', foundational, affordance_without_determination).
narrative_ontology:cs_axiom_status(affordance_without_determination, holdable).
narrative_ontology:cs_axiom_grounding('8c3d9747-7b08-4ae0-8263-14eff2e24300', affordance_without_determination, empirically_contingent).
narrative_ontology:cs_axiom('8c3d9747-7b08-4ae0-8263-14eff2e24300', secondary, market_selection_outpaced_doctrinal_steering).
narrative_ontology:cs_axiom_status(market_selection_outpaced_doctrinal_steering, holdable).
narrative_ontology:cs_axiom_grounding('8c3d9747-7b08-4ae0-8263-14eff2e24300', market_selection_outpaced_doctrinal_steering, empirically_contingent).
narrative_ontology:cs_reference_frame('8c3d9747-7b08-4ae0-8263-14eff2e24300', bidirectional_co_evolution_framework).
narrative_ontology:cs_drift_state('8c3d9747-7b08-4ae0-8263-14eff2e24300', post_quantitative_revision_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('8c3d9747-7b08-4ae0-8263-14eff2e24300', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, commercial_printers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reformist_publishing_entrepreneurs).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_reading_public).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, fiscally_tolerant_territorial_princes).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, pirated_humanist_authors).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, insolvent_printer_households).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, roman_curia_gatekeepers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, scriptoria_and_itinerant_preachers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, commercial_printers).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, wittenberg_reformer_faculty).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, vernacular_reading_public).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, bidirectional_causation_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, affordance_without_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run presses in Wittenberg, Basel, Strasbourg, Antwerp, and Venice; choose titles, front paper and type, and sell through the fair circuits. The reform controversy multiplied demand several-fold and rewarded houses that gambled on polemic and scripture; the same dynamics bankrupted houses that overprinted when demand turned, and enforcement seizures destroyed stock outright. Leaving the trade meant abandoning sunk capital and craft networks.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, commercial_printers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, commercial_printers, payer).

% Printer-publishers aligned to the Wittenberg movement built their houses on reformist titles; their trade, confession, and social standing fused with the cause, so printing for it was not a portfolio choice. They collected the boom's largest windfalls and carried its largest reputational exposure.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformist_publishing_entrepreneurs, beneficiary,
    organized, biographical, identity_locked, continental).

% Supply the doctrine, vet translations, and lend the names that made titles sell; their correspondence shows constant effort to speed some prints, correct others, and suppress distorted editions. Authorship brought fame but not ownership - houses reprinted and altered their texts without consultation, and by mid-century the market, not the faculty, decided what circulated.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, wittenberg_reformer_faculty, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, wittenberg_reformer_faculty, payer).

% Scholars watched corrected editions spawn error-ridden reprints in other cities within weeks, with no payment and no correction channel; refusing print meant scholarly invisibility, so they published into the risk and lodged their complaints in prefaces.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, pirated_humanist_authors, payer,
    moderate, biographical, constrained, continental).

% Families of printers who fronted editions that enforcement seized or demand never met; debts, seized stock, and craft-specific capital left them no comparable employment, and they survive in the record as creditors' ledgers and city relief rolls rather than as negotiating parties.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, insolvent_printer_households, payer,
    powerless, biographical, trapped, regional).

% Buy pamphlets and folios at prices scribes could never match, gaining direct access to scripture and argument; in ban-enforcing territories they pay smuggling premiums and risk penalties, and everywhere they receive whatever the market selected, including garbled reprints.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, vernacular_reading_public, beneficiary,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, vernacular_reading_public, payer).

% Imperial and municipal bodies requiring pre-publication license, seizing unlicensed stock, and prosecuting offending houses; their writ runs unevenly across territories, and each tightening drives printing across a border rather than out of existence.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, imperial_licensing_offices, agenda_setter,
    institutional, generational, constrained, continental).

% Princes and magistrates who proclaim bans in principle while taxing, hosting, and protecting print shops in practice, collecting customs, fees, and prestige from a trade their edicts nominally forbid; their tolerance is selective and revocable, keeping every house dependent on continued favor.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, fiscally_tolerant_territorial_princes, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, fiscally_tolerant_territorial_princes, agenda_setter).

% The papal bureaucracy that lost its monopoly on circulating sacred and doctrinal text; it responds with bans, indexes, and a counter-press rather than adoption, because participating in open print would dissolve the gatekeeping office that constitutes it. Its costs are paid in authority and reach, not coin.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, roman_curia_gatekeepers, payer,
    institutional, civilizational, identity_locked, continental).

% Scribes, scriptoria, and traveling preachers whose manuscript and oral channels had carried dissent and devotion for centuries; print economics undercut their prices and speeds within a generation, and they had no seat in the councils where print privileges and licenses were negotiated.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, scriptoria_and_itinerant_preachers, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, scriptoria_and_itinerant_preachers, excluded).

% Analysts reconstructing production runs, edition counts, and city adoption data; they observe the whole coupling from outside and hold no position in the early modern economy it describes.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, historiographers_of_print, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__co_constitution_reading, commercial_printers).
narrative_ontology:fixing_cost_class(technology_reformation_causality__co_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of moving identical texts to dispersed vernacular audiences faster than authorities can respond: standardized typesetting, reprint networks, and fair circuits make one argument simultaneously present in dozens of cities.
% TRANSFER_FUNCTION: Moves coin from readers and patrons to printer-publishers; moves textual authority from clerical gatekeepers to whoever operates a press; moves reputational and doctrinal control from authors to the houses that reprint them without payment; and concentrates enforcement risk on the houses least able to bear it.
% ABSENT_VOICES: Scriptoria, scribes, and itinerant preachers displaced by print economics had the clearest standing to object and no seat anywhere; insolvent printer households appear only as ledger entries; women readers shaped the devotional market without any role in licensing or production decisions. The apparent unanimity of the surviving record is partly an artifact of who could sign petitions and who could only be seized.
% DISAPPEARANCE_RATIONALE: Without the press-reform coupling, Luther's 1520 treatises stay at manuscript scale and the movement fragments regionally as Hussitism did; the vernacular scripture market, the pamphlet news cycle, and the confessional print economies of Geneva, Amsterdam, and London never organize. Every later arrangement built on cheap identical text - scientific journals, newspapers, catechisms - loses its founding substrate.
% FOUNDING_PROBLEM: Dissent and reform died of transmission lag: reproducing a text by hand cost months per copy, so Wycliffe's and Hus's challenges never reached a fraction of the needed audience at the speed required to outrun suppression.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: Febvre and Martin's production data and Eisenstein's fixity analysis document the pre-1450 bottleneck and its dissolution; the Roman Curia's own emergency countermeasures (bans, indexes, licensing treaties with cities) attest that its opponents had solved transmission; Dittmar's city-level adoption data shows the capability diffusing independently of any single patron. No party claims the transmission-lag problem still exists.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.58 because the coupling genuinely subsidizes most seats - cheap identical text for readers, windfalls for winning houses, revenue for tolerant princes - while the interaction term concentrates real costs on identifiable payers: uncompensated authors, ruined houses, a gatekeeping office stripped of its function. Suppression is authored at 0.50 as a RAW STRUCTURAL PROPERTY, unscaled: bans, licensing, and seizures were persistent but chronically leaky (smuggled sheets, false title pages, border-hopping presses), a stable enforcement equilibrium rather than total closure; only extractiveness is scaled by the engine, via directionality and scope. Theater_ratio 0.38 records the reformer-steering layer's decay: vetting and imprimaturs were functional war-time control around 1525 and increasingly ceremonial once market selection, not doctrinal approval, decided what survived - the atrophied-alternatives component of this reading. Accessibility_collapse 0.45: manuscript and oral alternatives persisted throughout but were steadily outcompeted on price and speed, a partial rather than near-total collapse. Resistance 0.60: sustained counter-press, imperial bans, printer flight, and author complaints met the arrangement continuously. The three temporal series run on ONE shared seven-point grid (1450-1600) so every metric is authored at every examined time point; suppression_requirement is tracked because enforcement capacity genuinely moved - light licensing before 1517, ratchet to the 1550 peak (Worms lineage, Interim, index-listing), then territorial normalization after Augsburg - rather than sitting static. Fixing_cost is prohibitive on its own evidence: every rollback attempt (Worms ban, Augsburg Interim, Index enforcement) failed at escalating expense because the economy, the movement, and the states had all reorganized around the coupling. Coalition note: the weakest seats (insolvent households, displaced scriptoria) were geographically scattered and craft-fragmented, with effectively zero coalition capacity - their powerlessness is structural, not incidental.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same structure as four different worlds: from the winning printer's position the coupling is the commercial opportunity of the century; from the insolvent household's position it is a lottery lost with unredeployable craft capital; from the Curial position it is the confiscation of a twelve-century-old gatekeeping office; from the reading public's position it is a subsidy. The payer and beneficiary seats therefore compute different types from identical structural facts, driven by power, exit, and directionality rather than by anyone's self-description. The reformer faculty seat is internally divided the same way: as agenda setter it experiences authorship of the century's discourse; as payer it experiences watching distorted reprints of its own texts outrun its corrections.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (winning printers, reformist publishing entrepreneurs, the reading public, fiscally tolerant princes) derive low directionality - the arrangement subsidizes them. Victim declarations (pirated authors, insolvent households, the Curia, displaced manuscript-and-oral channels) derive high directionality. Two structural modifiers matter: the Curia's identity_locked exit pushes it toward the full-target end despite institutional power - it cannot participate without dissolving the office that constitutes it - while the reading public's mobility pulls it toward the beneficiary end despite diffuse indirect costs. The commercial_printer class sits near symmetric by construction: one seat contains both the boom's winners and its bankrupts, so class-level costs approximate class-level gains even though individuals landed at extremes. No directionality_overrides were needed: role plus exit data already separates every seat the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - dissent dying of transmission lag - is dead: solved decisively by the 1530s, corroborated from outside the beneficiary set. Yet the arrangement persists because successor functions (news cycles, scientific communication, confessional administration) re-founded it on new coordination problems; that is the co-constitution reading's own claim about the coupling. Classifying the arrangement as tangled_rope prevents both characteristic mislabels: the rope mislabel, which would honor the genuine coordination and hide the payers the interaction term concentrates costs on; and the snare mislabel, which would read the piracy, ruin, and gatekeeping loss as the arrangement's purpose and deny the enormous net benefit the coupling delivered to nearly every seat. The reformer-steering component decays toward theater as market selection takes over - a piton-flavored residue living inside a live tangled_rope - and the moderate theater_ratio records that decay honestly without letting it stand in for the whole structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexical_status,
    'This constraint is one reading (co_constitution) of the kernel technology_reformation_causality; which reading''s causal structure should govern classification of the print-Reformation arrangement?',
    'Kernel adjudication across the three sibling stories: if the technological_determinism_reading''s inevitability claim survives evidentiary review, this reading''s interaction-term epsilon collapses toward the determinism story''s profile; if the beneficiary_agency_reading''s exclusivity holds, epsilon relocates to the strategic-chooser seats and the structural coupling empties.',
    'Each sibling instantiates a different constraint with different epsilon and type: the determinism reading asserts a necessity claim with negligible extraction; the agency reading concentrates costs in choice rather than structure. Adopting either dissolves this story''s tangled_rope profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexical_status, conceptual, 'Committer structure: this story is the co-constitution reading of the technology_reformation_causality kernel; sibling readings would change epsilon and type.').

omega_variable(
    interaction_term_identifiability,
    'Can the extraction generated by the press-times-reformer interaction be separated from the contributions of the press alone or the reformers alone?',
    'Comparative baselines: pre-print dissent movements (Hussite, Waldensian transmission curves) supply the no-press counterfactual; high-print-no-break regions (northern Italy, Spain) supply the no-reform counterfactual; the residual attributable to neither component alone is the coupling''s own contribution.',
    'If the interaction residual is small, the arrangement trends toward pure coordination and the claimed type softens toward rope; if the interaction carries most of the measured extraction, the tangled_rope claim stands and the coupling itself is the extractive engine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interaction_term_identifiability, empirical, 'Whether epsilon-from-interaction is identifiable against component-only counterfactuals.').

omega_variable(
    reformer_steering_functional_residual,
    'How much of the reformers'' imprimatur and vetting activity remained functional content control versus ceremonial maintenance once market selection dominated title survival?',
    'Series on endorsement rates, the fate of rejected titles, and market performance of endorsed versus unendorsed editions, 1520-1600.',
    'Sets the theater_ratio level and weights the reformer seat''s atrophied-steering component; a large functional residual lowers theater and strengthens the steering half of the co-constitution claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_steering_functional_residual, empirical, 'Functional-versus-theatrical boundary of reformer print governance.').

omega_variable(
    curia_participation_counterfactual,
    'Was the Roman Curia''s refusal to participate in the open print commons structurally locked by its gatekeeping identity, or a reversible policy choice a different Curial strategy could have overturned?',
    'Curial archives on early responses to printing (Leo X''s licensing negotiations, Trent''s index deliberations): evidence of seriously weighed adoption paths would indicate choice; their absence or repeated dismissal would indicate lock.',
    'If participation was genuinely available, the Curia seat''s directionality drops toward symmetric and part of its measured cost becomes foregone benefit rather than imposed cost, lowering overall epsilon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(curia_participation_counterfactual, conceptual, 'Identity-lock versus strategic choice in the Curia''s exit from the print commons.').

omega_variable(
    privilege_compensation_baseline,
    'How much of the cost borne by first publishers from unauthorized reprinting was already offset by imperial and Venetian printing privileges, changing the net extraction attributed to the coupling?',
    'Match privilege registry entries against reprint incidence per title; estimate the compensated share of pirated editions.',
    'High effective compensation lowers epsilon toward the rope range; low compensation confirms the asymmetric extraction that anchors the tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privilege_compensation_baseline, empirical, 'Privilege-system compensation as a deduction from measured piracy extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__co_constitution_reading, theater_ratio, 1450, 0.1).
narrative_ontology:measurement_basis(tech_tr_t1450, observed).
narrative_ontology:measurement(tech_tr_t1475, technology_reformation_causality__co_constitution_reading, theater_ratio, 1475, 0.14).
narrative_ontology:measurement_basis(tech_tr_t1475, observed).
narrative_ontology:measurement(tech_tr_t1500, technology_reformation_causality__co_constitution_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement_basis(tech_tr_t1500, observed).
narrative_ontology:measurement(tech_tr_t1525, technology_reformation_causality__co_constitution_reading, theater_ratio, 1525, 0.35).
narrative_ontology:measurement_basis(tech_tr_t1525, observed).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__co_constitution_reading, theater_ratio, 1550, 0.42).
narrative_ontology:measurement_basis(tech_tr_t1550, observed).
narrative_ontology:measurement(tech_tr_t1575, technology_reformation_causality__co_constitution_reading, theater_ratio, 1575, 0.4).
narrative_ontology:measurement_basis(tech_tr_t1575, observed).
narrative_ontology:measurement(tech_tr_t1600, technology_reformation_causality__co_constitution_reading, theater_ratio, 1600, 0.38).
narrative_ontology:measurement_basis(tech_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1450, 0.3).
narrative_ontology:measurement_basis(tech_be_t1450, observed).
narrative_ontology:measurement(tech_be_t1475, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1475, 0.38).
narrative_ontology:measurement_basis(tech_be_t1475, observed).
narrative_ontology:measurement(tech_be_t1500, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1500, 0.45).
narrative_ontology:measurement_basis(tech_be_t1500, observed).
narrative_ontology:measurement(tech_be_t1525, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1525, 0.62).
narrative_ontology:measurement_basis(tech_be_t1525, observed).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1550, 0.66).
narrative_ontology:measurement_basis(tech_be_t1550, observed).
narrative_ontology:measurement(tech_be_t1575, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1575, 0.6).
narrative_ontology:measurement_basis(tech_be_t1575, observed).
narrative_ontology:measurement(tech_be_t1600, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1600, 0.58).
narrative_ontology:measurement_basis(tech_be_t1600, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1450, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1450, 0.15).
narrative_ontology:measurement_basis(tech_su_t1450, observed).
narrative_ontology:measurement(tech_su_t1475, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1475, 0.2).
narrative_ontology:measurement_basis(tech_su_t1475, observed).
narrative_ontology:measurement(tech_su_t1500, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1500, 0.28).
narrative_ontology:measurement_basis(tech_su_t1500, observed).
narrative_ontology:measurement(tech_su_t1525, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1525, 0.55).
narrative_ontology:measurement_basis(tech_su_t1525, observed).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1550, 0.62).
narrative_ontology:measurement_basis(tech_su_t1550, observed).
narrative_ontology:measurement(tech_su_t1575, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1575, 0.52).
narrative_ontology:measurement_basis(tech_su_t1575, observed).
narrative_ontology:measurement(tech_su_t1600, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1600, 0.5).
narrative_ontology:measurement_basis(tech_su_t1600, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, index_librorum_prohibitorum).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, statute_of_anne_copyright).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'print caused the Reformation' decomposes into three epsilon-invariant stories - technological_determinism_reading (necessity claim, mountain-profile, negligible extraction), beneficiary_agency_reading (tool relation, costs located in strategic choice), and this co_constitution_reading (interaction-term extraction, tangled_rope). The determinism claim circulates as settled background in popular accounts and thereby pressures this reading's legitimacy conditions (upstream influence); this reading's documented uncompensated-reprinting extraction is the causal ancestor of privilege systems and ultimately the Statute of Anne, hence the downstream edges to index_librorium_prohibitorum and statute_of_anne_copyright. Each member carries its own epsilon, beneficiaries, victims, and claimed type; none hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
