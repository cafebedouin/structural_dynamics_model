% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Press as Strategic Deployment Tool for Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint describes the printing press as a neutral capacity that
 *   reformation agents strategically deployed to amplify their message and
 *   profit. The reading asserts that human agency—printer decision-making
 *   about what to publish, reformer choice to author and authorize texts,
 *   merchant city governance that permitted printing—directed the
 *   technology's use toward reformation ends. The press did not cause the
 *   Reformation; reformers and printers caused the press to become the
 *   Reformation's primary distribution apparatus. This is ONE reading of the
 *   contested kernel 'press_reformation_causation'—a reading emphasizing
 *   agent causation and purposeful technology deployment, as opposed to
 *   technological determinism (press made reformation inevitable) or mutual
 *   shaping (technology and agency co-evolved).
 *
 * KEY AGENTS:
 *   - reformation_printers: Deliberate agents who selected reformation texts for publication based on ideology, market demand, and profit. They controlled print runs, distribution, and pricing.
 *   - reformation_theologians_preachers: Authorizers and producers of reformation content who strategically chose the printing medium to reach scale. They benefited from amplification.
 *   - catholic_ecclesiastical_establishment: Structural target that lost monopoly control over scriptural interpretation. They bore the cost of authority fragmentation.
 *   - merchant_cities_trade_hubs: Regional governors who decided whether to permit and profit from printing activities. Many actively encouraged reformation printing as economic and prestige activity.
 *   - papal_and_imperial_authorities: Attempted suppression through censorship and bans but could not undo the distributed capacity of printing networks.
 *   - literate_laity_vernacular_readers: Beneficiaries of direct textual access when printers strategically produced vernacular materials, but dependent on printer and reformer choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.62).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.45).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.62).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Press as Strategic Deployment Tool for Reformation").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causation__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, 'b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6').
narrative_ontology:cs_kernel_codification('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', distributed).
narrative_ontology:cs_authority_grounding('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', expertise).
narrative_ontology:cs_interpretation_layer_present('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6').
narrative_ontology:cs_reading_relation('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', foundational, human_agency_drives_technology_deployment).
narrative_ontology:cs_axiom_status(human_agency_drives_technology_deployment, holdable).
narrative_ontology:cs_axiom_grounding('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', human_agency_drives_technology_deployment, empirically_contingent).
narrative_ontology:cs_axiom('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', secondary, printing_press_is_neutral_capacity_not_autonomous_cause).
narrative_ontology:cs_axiom_status(printing_press_is_neutral_capacity_not_autonomous_cause, holdable).
narrative_ontology:cs_axiom_grounding('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', printing_press_is_neutral_capacity_not_autonomous_cause, deontological).
narrative_ontology:cs_reference_frame('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', neutral_printing_capacity).
narrative_ontology:cs_drift_state('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', post_reformation_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b89a21ef-6192-47fc-bdfe-5e6ee06ec5f6', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, reformation_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, reformation_theologians_preachers).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_ecclesiastical_establishment).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, competing_religious_factions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, merchant_cities_trade_hubs).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, literate_laity_vernacular_readers).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, papal_and_imperial_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control access to printing capacity and decide what gets printed. They strategically choose to publish reformation texts—pamphlets, vernacular Bibles, polemical works—because they identify market demand, ideological alignment, or profit opportunity. They set print runs, distribution networks, and pricing. Their income and reputation depend directly on the commercial success and circulation of reformation materials.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, reformation_printers, agenda_setter,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, reformation_printers, beneficiary).

% Deploy the printing press as their chosen instrument to reach beyond pulpit audiences to readers across territories and languages. They author or authorize texts that printers select and multiply. Their message reaches scale and persistence through the printer's strategic choices about what to produce and how to distribute it. They benefit from amplification without controlling the production apparatus.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, reformation_theologians_preachers, beneficiary,
    moderate, biographical, constrained, regional).

% Loses monopoly control over scriptural interpretation and religious authority when printers decide to publish vernacular Bibles and reformation critiques. They attempt counter-publication (producing their own printed works) and legal suppression (banning, burning, pursuing heresy charges), but cannot undo the distributed capacity of the printing network once activated. They bear the cost of authority fragmentation.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_ecclesiastical_establishment, payer,
    institutional, generational, constrained, continental).

% Experience proliferation of competing printed doctrines and polemics. Reformed, Lutheran, Anabaptist, and other factions all exploit printing capacity strategically, driving confessional fragmentation through published argument. Each faction bears the cost of doctrinal contestation now permanently recorded and circulated in print.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, competing_religious_factions, payer,
    organized, generational, constrained, regional).

% House printing operations and control distribution routes (Strasbourg, Basel, Wittenberg, Geneva, Amsterdam). They profit from printer tax, paper supply, ink manufacture, and binding. They decide whether to enforce or permit printing of reformation materials—many actively encourage it as economic activity and cultural prestige.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, merchant_cities_trade_hubs, beneficiary,
    powerful, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, merchant_cities_trade_hubs, agenda_setter).

% Attempt to suppress reformation printing through bans, censorship apparatus (Index of Prohibited Books), and legal pressure on printers. They cannot prevent the activation because printers in jurisdictions they don't control will print anyway. The suppression machinery itself becomes a theater: burning books accelerates fame; prohibitions create clandestine distribution.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, papal_and_imperial_authorities, payer,
    institutional, generational, trapped, continental).

% Gain access to religious texts in their own languages when printers strategically decide that vernacular production serves market demand or ideology. They benefit from direct scriptural engagement without clerical mediation—but the availability depends on printer and reformer choices, not on lay demand alone.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, literate_laity_vernacular_readers, beneficiary,
    powerless, biographical, mobile, regional).

% Manuscript copyists and scribal networks were historically the dominant reproduction technology. Once printers strategically activate mass printing, manuscript reproduction cannot compete on scale. The old technology is not destroyed but economically displaced—trapped in a declining production model.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, competing_print_technologies_manuscripts, excluded,
    moderate, biographical, trapped, regional).

% Examines the causal structure: did the press cause reformation or did reformers and printers cause the press to amplify reformation? This reading asserts the latter—agency precedes and directs technology use.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__strategic_deployment, reformation_printers).
narrative_ontology:fixing_cost_class(press_reformation_causation__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Printers and reformers coordinate to produce, distribute, and circulate religious and polemical texts at scale beyond manuscript capacity. The printing technology becomes the coordination apparatus itself—transforming individual theological argument into mass-circulated doctrine and contestation.
% TRANSFER_FUNCTION: Moves authority over religious interpretation from the centralized ecclesiastical establishment to a distributed network of printers, theologians, and reading communities. Printers capture economic surplus (profit per volume, sustained demand for printed materials). Reformers gain reach for their doctrines. Ecclesiastical authorities lose monopoly control. Lay readers gain direct textual access.
% ABSENT_VOICES: Manuscript copyists and scribal guilds—those whose economic and professional interests depended on hand-production reproduction. They would have argued for limits on printing or privilege for manuscript work but were economically displaced. Rural illiterate populations remain absent: printing serves literate urban and merchant communities, not the masses.
% DISAPPEARANCE_RATIONALE: If printers had refused to strategically activate reformation publishing—if they had maintained neutrality or printed only Catholic works—the Reformation would have propagated through pulpit, manuscript, and oral networks but at vastly reduced speed and scale. The fragmentation of religious authority would have been delayed or geographically contained. The disappearance of strategic printer-reformer deployment would mean the Reformation remains regional and episodic rather than transforming into a continental, multi-generational movement.
% FOUNDING_PROBLEM: How do religious dissenters communicate their critiques across territories and languages when ecclesiastical authorities control manuscript copying and pulpit access? How does a decentralized reform movement scale without centralized command structure?
% FOUNDING_PROBLEM_CORROBORATION: Reformation historians document that printers made deliberate decisions to publish reformation materials—John Froben in Basel, Hans Lufft in Wittenberg, Christopher Plantin in Antwerp all actively identified and produced reform texts as strategic choices, not passive responses. Trade records, printer contracts, and surviving edition data show publishers selecting what to print. Reformers themselves describe the printing press as their chosen weapon—Luther's 'printing press is the instrument of God' reflects agential framing. Ecclesiastical authorities' desperate attempts at suppression (papal censorship apparatus, imperial book bans) confirm they recognized printer agency as the problem to solve.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how far the constraint redistributes from traditional authorities (papal/ecclesiastical) to new beneficiaries (printers/reformers/lay readers). At 1520 (peak reformation activity), extractiveness is 0.58—substantial because the redistribution is decisive, yet not maximal because the ecclesiastical establishment retains institutional power and can attempt counter-publication. Theater ratio rises from 0.05 (1455: printing is merely a neutral technology) to 0.28 (1560: suppression machinery becomes performative—book burnings advertise forbidden texts; censorship lists amplify reform doctrine) then stabilizes. Suppression requirement rises sharply through 1520–1560 as authorities activate counter-measures, peaks around 1560 when confessional conflicts are most violent, then declines slightly as the new theological/institutional reality becomes normalized. The measurement grid is shared across metrics and time points—every metric is authored at every examined interval point (1455, 1490, 1520, 1560, 1600, 1650). Early points (1455, 1490) use 'projected' basis because direct measurement data is sparse; later points use 'observed' basis from print records, ecclesiastical correspondence, and publication data. The extractiveness decline from 1600–1650 reflects stabilization: the initial radical redistribution of authority has calcified into new institutional equilibrium (established Protestant churches, normalized vernacular Bible markets, regularized printing licensing), so the extractive force of the constraint—the degree to which it continues to redistribute power—moderates.
 *
 * PERSPECTIVAL GAP:
 *   From the printer's seat: the press is neutral capacity they activate strategically to capture market opportunity and ideological positioning—the constraint is rope, coordination mechanism serving their profit and values. From the ecclesiastical establishment's seat: the same strategic activation is an attack on their monopoly—the constraint is extractive, a snare that removes their control. From the lay reader's seat: the constraint enables direct access to scripture and religious argumentation, beneficial coordination. From the censor's seat: suppression machinery is necessary defense against heresy spreading. The engine computes these seat-divergent classifications from the authored structural data: power asymmetry, exit options, beneficiary/victim positioning. The perspectival gap is not a measurement failure—it is exactly what the framework measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformation printers and theologians are net beneficiaries: they capture economic surplus, institutional power, and ideological influence. The ecclesiastical establishment and competing factions are net targets: they lose authority monopoly and face costly counter-publication and suppression attempts. Merchant cities are secondary beneficiaries (tax, prestige, economic activity). Literate lay readers are beneficiaries of access, though dependent on printer choices. Directionality: printers and reformers sit at d near 0.1–0.2 (beneficiary end—the constraint subsidizes their goals and profits). Ecclesiastical authorities sit at d near 0.8–0.9 (target end—the constraint extracts authority and stability from them). Lay readers sit near d=0.5–0.6 (mixed: genuine benefit from access, but dependency on printer discretion and vulnerability to suppression). Papal authorities sit at d near 0.9 (maximum target: their authority is the object being redistributed). No directionality overrides are needed—the structural derivation is tight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: communication of reform doctrine across territorial boundaries remains a persistent need as long as theological contestation is live. The founding problem has NOT become ceremonial—strategic deployment of printing continues to matter for religious and political movements through 1650 and beyond (Reformation consolidation, Counter-Reformation, English Civil War pamphleteering). Theater ratio is moderate (0.28) and tracks enforcement intensity, not performance masking atrophy. The constraint persists because the coordination problem it solves (reaching scale audiences with doctrine) and the extraction it enables (profit, power redistribution) remain operationally real, not ceremonial. No mandatrophy condition is present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_determinism_boundary,
    'How much of the Reformation''s propagation was driven by printer and reformer strategic choice versus how much by the inherent capabilities of printing technology itself?',
    'Counterfactual historical analysis: (1) Survey instances where printing capacity existed but strategically was NOT deployed for reformation—did reform doctrines propagate at comparable rates through other channels? (2) Examine regions with printing capacity but merchant/authority restrictions on reformation printing—did reformation nonetheless spread at equivalent pace? (3) Model the historical Reformation''s geographic and temporal distribution under different printer decision scenarios.',
    'If printer agency was primary driver, this reading stands as rope (strategic deployment). If technological inevitability dominated, reclassify as mountain (determinism reading) or modify extraction claims. If mixed, the reading remains rope but with higher acknowledgment of technological affordance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(agency_vs_determinism_boundary, conceptual, 'Whether Reformation propagation was agent-driven or technology-inevitable.').

omega_variable(
    strategic_choice_vs_market_response,
    'Did printers and reformers strategically TARGET reformation distribution as a deliberate choice, or did they respond to market demand that emerged from theological contestation already underway?',
    'Printer correspondence, contract records, and edition data: can we find evidence of proactive commissioning of reformation works (strategic initiative) versus reactive publication to market demand? What fraction of early printed reformation works were author-commissioned versus printer-selected for perceived market?',
    'True strategic choice strengthens the agency framing and extraction claims. Market-response framing softens agency attribution and may redistribute some beneficiary status to lay readers whose demand shaped output.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_choice_vs_market_response, empirical, 'Whether printer deployment was proactive strategic choice or reactive market response.').

omega_variable(
    ecclesiastical_vs_printer_causation,
    'Did the suppression apparatus (Catholic Church and imperial authorities attempting censorship and book-burning) constitute the primary enforcement mechanism enabling extraction, or did the constraint persist primarily because printers in decentralized jurisdictions could simply ignore bans?',
    'Trace enforcement effectiveness: did regions with strong enforcement (strong Catholic majorities, centralized governance) show lower circulation of reformation materials? Did printers in regions with weak enforcement (free cities, Protestant-majority territories) operate under lower suppression pressure? Measure correlation between enforcement intensity and circulation rates.',
    'If enforcement is primary, suppression=0.45 may be understated; reclassify as more tangled_rope (coordination + substantial coercive overhead). If enforcement is secondary (printers simply ignored bans), then suppression represents theater more than functional constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecclesiastical_vs_printer_causation, empirical, 'Whether ecclesiastical suppression was primary enforcement or secondary to jurisdictional fragmentation.').

omega_variable(
    reading_committer_frame_kernel_contest,
    'Is the strategic_deployment reading a genuine alternative framework to technological_determinism and mutual_shaping, or is it a different level of analysis (focusing on proximate human choices while determinism and mutual-shaping address ultimate causes)?',
    'Philosophical clarification: can all three readings be simultaneously true if they operate at different causal levels (agent choice, technological affordance, feedback dynamics)? Or do they make incompatible claims about what constituted the PRIMARY cause? If the latter, empirical arbitration by counterfactual reconstruction; if the former, the readings are orthogonal rather than genuinely competing.',
    'If readings are orthogonal, reclassify sibling relations as ''influences'' rather than ''coexists_with''—each reading creates conditions that make the others operative at different analytical levels. If readings are genuinely competing, maintain ''coexists_with'' (different parties hold different readings) or identify a ''forecloses'' pair if one reading logically eliminates another.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_committer_frame_kernel_contest, conceptual, 'Whether kernel readings compete on the same causal axis or operate at different analytical levels.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1455, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1455, press_reformation_causation__strategic_deployment, theater_ratio, 1455, 0.05).
narrative_ontology:measurement_basis(pres_tr_t1455, projected).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causation__strategic_deployment, theater_ratio, 1490, 0.12).
narrative_ontology:measurement_basis(pres_tr_t1490, projected).
narrative_ontology:measurement(pres_tr_t1520, press_reformation_causation__strategic_deployment, theater_ratio, 1520, 0.22).
narrative_ontology:measurement_basis(pres_tr_t1520, observed).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causation__strategic_deployment, theater_ratio, 1560, 0.28).
narrative_ontology:measurement_basis(pres_tr_t1560, observed).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__strategic_deployment, theater_ratio, 1600, 0.32).
narrative_ontology:measurement_basis(pres_tr_t1600, observed).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causation__strategic_deployment, theater_ratio, 1650, 0.28).
narrative_ontology:measurement_basis(pres_tr_t1650, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1455, press_reformation_causation__strategic_deployment, base_extractiveness, 1455, 0.35).
narrative_ontology:measurement_basis(pres_be_t1455, projected).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causation__strategic_deployment, base_extractiveness, 1490, 0.45).
narrative_ontology:measurement_basis(pres_be_t1490, projected).
narrative_ontology:measurement(pres_be_t1520, press_reformation_causation__strategic_deployment, base_extractiveness, 1520, 0.58).
narrative_ontology:measurement_basis(pres_be_t1520, observed).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causation__strategic_deployment, base_extractiveness, 1560, 0.66).
narrative_ontology:measurement_basis(pres_be_t1560, observed).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__strategic_deployment, base_extractiveness, 1600, 0.62).
narrative_ontology:measurement_basis(pres_be_t1600, observed).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causation__strategic_deployment, base_extractiveness, 1650, 0.59).
narrative_ontology:measurement_basis(pres_be_t1650, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1455, press_reformation_causation__strategic_deployment, suppression_requirement, 1455, 0.15).
narrative_ontology:measurement_basis(pres_su_t1455, projected).
narrative_ontology:measurement(pres_su_t1490, press_reformation_causation__strategic_deployment, suppression_requirement, 1490, 0.25).
narrative_ontology:measurement_basis(pres_su_t1490, projected).
narrative_ontology:measurement(pres_su_t1520, press_reformation_causation__strategic_deployment, suppression_requirement, 1520, 0.48).
narrative_ontology:measurement_basis(pres_su_t1520, observed).
narrative_ontology:measurement(pres_su_t1560, press_reformation_causation__strategic_deployment, suppression_requirement, 1560, 0.52).
narrative_ontology:measurement_basis(pres_su_t1560, observed).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causation__strategic_deployment, suppression_requirement, 1600, 0.45).
narrative_ontology:measurement_basis(pres_su_t1600, observed).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causation__strategic_deployment, suppression_requirement, 1650, 0.42).
narrative_ontology:measurement_basis(pres_su_t1650, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, resource_allocation).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__strategic_deployment, 0.12).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The kernel 'press_reformation_causation' decomposes into three structurally distinct constraints, each representing a reading of contested causation. This story (strategic_deployment) emphasizes agent-driven direction of neutral technology. The sibling 'technological_determinism' story emphasizes inherent material inevitability of printing technology. The sibling 'mutual_shaping' story emphasizes co-evolution of technology and agency through feedback. The three readings have different ε values, different beneficiary/victim structures, and different types. They are linked via network.affects_constraints because the truth of one reading constrains the space of defensible positions for the others—but they remain live, co-held positions in historical scholarship rather than sequentially resolved claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
