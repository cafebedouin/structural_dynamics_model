% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Print Deployment of the Reformation (Reformer-Printer Weaponization Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Between roughly 1517 and 1567 (modeled here as interval 0-50), an
 *   alliance of reformers, master printers, and opportunistic princes
 *   converted an existing reproduction technology into an instrument:
 *   vernacular pamphlets, translated scripture, and polemical flugschriften
 *   were commissioned, priced, and timed to break the Church's hold on
 *   doctrine and revenue. This story instantiates ONE reading of the
 *   press_reformation_causality kernel — the strategic_deployment reading —
 *   and generates it as a clean, epsilon-invariant constraint: the standing
 *   arrangement under contest is the deployment complex itself, and epsilon
 *   is authored for that arrangement as this reading sees it (deliberate,
 *   targeted, agent-directed). The contest with sibling readings is NOT
 *   folded into the constraint; it is routed to the omegas and to
 *   commentary.kernel_context per the committer-frame rules. The claim and
 *   the metrics are independent authored facts: claimed_type states what I
 *   believe is structurally true of the arrangement (genuine coordination
 *   function plus asymmetric extraction, actively maintained), while the
 *   metrics describe its observed operation; the engine computes per-seat
 *   classifications from the structural data, and any divergence between
 *   claim and computed type is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - protestant_reformers: Primary beneficiary (organized/identity_locked) — authored the weaponized content; exit means recantation or death
 *   - early_modern_printers: Secondary beneficiary and operational agenda-setter (organized/constrained) — selected what was printed and collected the monetary rents
 *   - secular_princes: Opportunist beneficiary (powerful/arbitrage) — supplied protection and law, collected property and jurisdiction
 *   - literate_urban_readers: Diffuse beneficiary (moderate/mobile) — received cheap standardized argument, steered demand
 *   - catholic_church_hierarchy: Primary target (institutional/trapped) — lost revenue, interpretive monopoly, and territorial obedience; could not exit doctrine
 *   - pirated_authors: Secondary target (moderate/constrained) — bore uncompensated reprinting with offsetting publicity
 *   - anabaptist_radicals: Excluded voice (powerless/trapped) — printed against by the coalition, absent from its councils
 *   - historians_of_the_book: Analytical observer (analytical/analytical) — reconstructs the whole structure from archives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.66).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.62).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.66).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Print Deployment of the Reformation (Reformer-Printer Weaponization Reading)").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, '010fae0e-52dd-4009-842b-19ce79aa41c3').
narrative_ontology:cs_kernel_codification('010fae0e-52dd-4009-842b-19ce79aa41c3', distributed).
narrative_ontology:cs_authority_grounding('010fae0e-52dd-4009-842b-19ce79aa41c3', expertise).
narrative_ontology:cs_interpretation_layer_present('010fae0e-52dd-4009-842b-19ce79aa41c3').
narrative_ontology:cs_reading_relation('010fae0e-52dd-4009-842b-19ce79aa41c3', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('010fae0e-52dd-4009-842b-19ce79aa41c3', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('010fae0e-52dd-4009-842b-19ce79aa41c3', foundational, agent_choice_carries_causal_load).
narrative_ontology:cs_axiom_status(agent_choice_carries_causal_load, holdable).
narrative_ontology:cs_axiom_grounding('010fae0e-52dd-4009-842b-19ce79aa41c3', agent_choice_carries_causal_load, empirically_contingent).
narrative_ontology:cs_axiom('010fae0e-52dd-4009-842b-19ce79aa41c3', secondary, market_logic_shaped_doctrine).
narrative_ontology:cs_axiom_status(market_logic_shaped_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('010fae0e-52dd-4009-842b-19ce79aa41c3', market_logic_shaped_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('010fae0e-52dd-4009-842b-19ce79aa41c3', agent_centered_strategic_causation).
narrative_ontology:cs_drift_state('010fae0e-52dd-4009-842b-19ce79aa41c3', contemporary_historiography, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('010fae0e-52dd-4009-842b-19ce79aa41c3', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, early_modern_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, secular_princes).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, literate_urban_readers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, pirated_authors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% University-trained clergy and former monastics who wrote vernacular sermons, Bible translations, and polemics timed to printer output. Print let them address a public directly, bypassing episcopal licensing and disputation gatekeeping. Their livelihoods, physical safety, and self-conception fused with the movement the pamphlet network sustained; leaving it meant recantation, exile, or the stake.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, protestant_reformers, beneficiary,
    organized, generational, identity_locked, continental).

% Master printers and their investor syndicates who selected, financed, and rushed editions to market. They decided which tracts were set in type and in what quantity, hedged between confessional markets, relocated shops when city authorities turned hostile, and took the profits of repeated bestseller runs. Many also absorbed losses from confiscated stock and banned editions.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, early_modern_printers, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, early_modern_printers, agenda_setter).

% Territorial rulers who protected reforming printers, sponsored court preachers, and legislated church ordinances. They gained legal-religious cover for absorbing monastic property and tithes, and used confessional alignment as diplomatic currency. Their position let them switch patrons or hedge between Rome and the evangelical estates at comparatively low cost.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, secular_princes, beneficiary,
    powerful, generational, arbitrage, national).

% Townsmen, artisans, students, and lower clergy who bought pamphlets and household Bibles, joined reading circles, and carried arguments into guildhalls and taverns. They could choose among competing confessional offers and stop buying; their demand signals steered what printers risked next.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, literate_urban_readers, beneficiary,
    moderate, biographical, mobile, regional).

% The papal curia, bishops, and religious orders that lost the indulgence revenue stream, the interpretive monopoly, and in wide territories the obedience of clergy and laity. Their countermeasures — bans, indices, censorship offices, reformed orders — consumed enormous resources and recovered little of the lost ground. They could not abandon doctrine or office to escape the pressure.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% Humanist scholars, poets, and theologians whose works were reprinted without permission or payment across the European book fairs. Unauthorized editions spread their names widely while cutting into authorized sales and exposing them to charges for texts they had not cleared. Their remedies — privilege petitions, litigation — ran at manuscript speed against reprint speed.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, pirated_authors, payer,
    moderate, biographical, constrained, continental).

% Baptist and spiritualist reformers outside the magisterial coalition. Their own writings circulated in small fugitive editions; the coalition's presses and pulpits printed against them, and imperial and civic authorities executed many. They had no seat in the publishing decisions that defined acceptable reform.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, anabaptist_radicals, excluded,
    powerless, biographical, trapped, regional).

% Modern scholars of print culture who reconstruct production figures, printer ledgers, and diffusion patterns from archives. They take no side in the sixteenth-century allocation and can compare the whole arrangement across seats and centuries.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, historians_of_the_book, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the dispersed-dissent synchronization problem: cheap, identical, rapidly reprinted pamphlets let geographically scattered critics cite the same texts, answer the same objections, and recognize themselves as one movement within months — something episodic manuscript circulation and earshot preaching could not do. Secondarily it standardizes vernacular scripture and liturgy across territories.
% TRANSFER_FUNCTION: Moves interpretive authority and revenue: indulgence income and doctrinal gatekeeping away from the Roman hierarchy toward reformer-published vernacular texts; moves coin from urban readers to printers and their investor syndicates through bestseller economics; moves church lands and dues toward cooperating princes under reforming legal cover; moves attention from Latin clerical discourse to vernacular public argument.
% ABSENT_VOICES: Anabaptist and spiritualist radicals would object loudest: they were printed against by the very coalition this arrangement served, and their own presses were few and fugitive. The peasants of 1525 would object — their grievances were answered with Luther's printed counterattack. Catholic scholastics would object that their reply channels (disputation, episcopal correction) operated at manuscript speed against print speed. None of these sat in the coalition's publishing councils.
% DISAPPEARANCE_RATIONALE: If the deployment arrangement vanished overnight — printers declining reformist copy, reformers reverting to manuscript and pulpit — the movement fragments back into contained academic dissent on the Hussite pattern: local suppression succeeds because confiscation can outrun hand-copying; indulgence revenue and doctrinal monopoly persist; the princes lose the legal-religious cover for secularizing church property; the print trade loses its first mass-market bestseller engine and contracts back to liturgical and official jobbing.
% FOUNDING_PROBLEM: The arrangement was built to solve a double bottleneck: doctrinally, how to carry scholarly critique past episcopal gatekeeping to a literate public faster than authorities could confiscate it; commercially, how to fill presses profitably in a market whose official contracts (liturgica, university texts) were controlled by the Church.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Catholic controversialist Johannes Cochlaeus urged suppressing print shops precisely because Luther's theses outran episcopal reply; the 1521 Edict of Worms and the 1559 Index attest the bottleneck by attempting to close it; Venetian and French diplomatic dispatches note the speed of pamphlet spread. Modern book historians (Febvre and Martin, Eisenstein, Pettegree) corroborate the mechanism while disputing its causal weight — that dispute is the kernel contest recorded in the omegas.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.66 at interval end because the deployment deliberately targeted existing revenue and authority streams (indulgences, interpretive monopoly) and because printer returns were decoupled from author compensation — piracy was the norm, privileges the exception. Suppression is 0.62 because persistence required continuous active work: evading the Edict of Worms, disguising imprints, running the Frankfurt clandestine trade, and later building Protestant territories' own censorship once the coalition held power. Theater_ratio is 0.28 and rising slowly: the core activity stayed functional, but licensing formalities, safe dedications, and confessional boilerplate grew as a defensive share of output. Accessibility_collapse is 0.40 — scribal and pulpit channels persisted throughout but were outcompeted on speed and cost, not eliminated. Resistance is 0.70: the Counter-Reformation, the Index, imperial bans, and eventually war were sustained, organized counter-mobilization. The temporal series run on one shared grid (points 0,10,20,30,40,50) so every metric is authored at every examined point; extractiveness rises steeply to a 1530s plateau then eases slightly as the Church counter-mobilizes and markets saturate, while suppression_requirement ratchets monotonically upward as enforcement machinery matured on both confessional sides. Receipt surface: gains demonstrably split three ways — printer profits, reformer authority, princely property — so after checking every named seat, no single seat captures the extraction; gain_flow is therefore the affirmative 'diffuse', not a default. Fixing cost is 'prohibitive': the party with the strongest incentive to remove the arrangement (the Church) spent decades and ruinous resources attempting exactly that and failed, which is direct evidence on the cost class.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute differently, and the structural data is built to produce that divergence. From the catholic_church_hierarchy seat — institutional power, civilizational horizon, trapped exit — the arrangement operates as deliberate dispossession by a coalition wielding a superior reproduction technology: a snare-shaped experience borne by the party least able to leave. From the reformer and printer seats the same structure is a coordination tool they built, funded, and risked their necks for: rope-shaped. The reader seat sits near-symmetric, gaining access while paying indirectly through confessional strife. The historian seat sees an instrument whose valence depends entirely on position. The engine computes these per-seat classifications from power, exit, and declared position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. catholic_church_hierarchy: declared victim, trapped exit, institutional power — derived d sits near the full-target end, and effective extraction is amplified by the trap. protestant_reformers: declared beneficiaries with identity_locked exit — the lock keeps them at the subsidized end indefinitely; their fusion with the movement means the subsidy cannot be cashed out by leaving. early_modern_printers: beneficiaries with constrained exit and agenda-setting leverage — low d, but their gatekeeping position lets them steer which doctrines ride the channel. secular_princes: beneficiaries with arbitrage-grade exit — nearest the full-beneficiary end; they could hedge between confessions, which is exactly what several did. literate_urban_readers: beneficiaries with mobile exit — nearest the beneficiary end, paying only diffuse indirect costs. pirated_authors are declared victims but their situation is genuinely ambivalent (unauthorized reprinting both harmed their sales and spread their reputations); the directionality_override surface keys on power atoms, which pirated_authors share with literate_urban_readers, so an override would misfire across seats — the ambivalence is routed to the piracy_net_harm_attribution omega instead of forced into a scalar.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — carrying dissent past episcopal gatekeeping faster than confiscation could run, and filling presses profitably outside Church-controlled contracts — was substantially solved by the mid-1520s: the coalition had broken the bottleneck, and no later pamphlet ever again had to achieve what the 95 Theses and the 1522 September Testament achieved. What persisted after the victory was the machine: confessional propaganda departments, printer rent-seeking on an established bestseller format, and princely administration of seized church property. That is a mandate outliving its function, and the story declares mandatrophy_resolved accordingly; the founding_problem_status x disappearance_verdict pair (dead x world_rearranges) records the zombie signature for the mismatch consumer. The classification prevents mislabeling in both directions: calling the arrangement a pure snare erases the real coordination achievement (dispersed dissent synchronized at print speed — a genuine collective-action solution that manuscripts could not deliver); calling it a pure rope erases the asymmetric extraction (a trapped institutional target, uncompensated authors, and rents decoupled from service). The tangled-rope claim holds both facts in one structure, and the temporal series shows the drift from breakthrough instrument to standing extraction-and-propaganda apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the strategic_deployment reading of the press_reformation_causality kernel; what would change structurally if the technological_determinism sibling reading were adopted instead?',
    'Cross-reading comparison within the kernel family: reclassify the same historical arrangement under the determinism reading, where enabling capacity rather than agent choice carries the causal load.',
    'Under determinism, the beneficiary/victim structure shifts from agent coalitions to the technology itself as an autonomous force; persistence reads as inevitability rather than maintained deployment, dissolving the enforcement story and changing the computed type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position marker: this story is one reading of a contested causal kernel, with sibling readings authored as separate constraints.').

omega_variable(
    co_constitution_feedback_delta,
    'How would the co_constitution sibling reading restructure this constraint''s beneficiary and enforcement data?',
    'Model the print-economy/religious-controversy feedback loop as the unit of analysis — printer profits funding controversy, controversy driving demand, demand reshaping doctrine — rather than unidirectional strategic use of a tool.',
    'Co-constitution distributes agency across the loop and blurs the beneficiary/target boundary (the Church itself became a mass print user after 1540), likely softening the asymmetry that drives the tangled-rope computation toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_constitution_feedback_delta, conceptual, 'Sibling-reading structural delta: feedback-loop framing versus agent-directed deployment framing.').

omega_variable(
    counterfactual_diffusion_necessity,
    'Was strategic print deployment causally necessary for the Reformation''s scale, or would maturing manuscript and preaching networks have achieved comparable diffusion?',
    'Compare diffusion half-lives of pre-print heterodox movements (Waldensians, Wycliffites, Hussites) against print-era Protestant spread, controlling for political protection; use production and edition-count data from the book-history literature.',
    'If manuscript networks converge on comparable reach, the coordination-function component shrinks and the arrangement reads closer to pure rent-seeking; if print is decisive, the rope component is confirmed and the tangled-rope claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_diffusion_necessity, empirical, 'Whether the coordination function attributed to the deployment was decisive or incidental.').

omega_variable(
    piracy_net_harm_attribution,
    'Did unauthorized reprinting impose net harm on the authors pirated, or did the publicity outweigh lost sales in the early modern book market?',
    'Author-level comparison of earnings and career trajectories for pirated versus exclusively published writers, including royalty arrangements some reformer-authors negotiated against prevailing pirate norms.',
    'If net benefit dominates, pirated_authors exits the victim set and the extraction profile concentrates on the Church hierarchy, raising measured asymmetry; if net harm dominates, the dual-victim structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piracy_net_harm_attribution, empirical, 'Victim-set composition uncertainty around unauthorized reprinting.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prsd_tr_t0, press_reformation_causality__strategic_deployment, theater_ratio, 0, 0.08).
narrative_ontology:measurement(prsd_tr_t10, press_reformation_causality__strategic_deployment, theater_ratio, 10, 0.12).
narrative_ontology:measurement(prsd_tr_t20, press_reformation_causality__strategic_deployment, theater_ratio, 20, 0.16).
narrative_ontology:measurement(prsd_tr_t30, press_reformation_causality__strategic_deployment, theater_ratio, 30, 0.21).
narrative_ontology:measurement(prsd_tr_t40, press_reformation_causality__strategic_deployment, theater_ratio, 40, 0.25).
narrative_ontology:measurement(prsd_tr_t50, press_reformation_causality__strategic_deployment, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(prsd_be_t0, press_reformation_causality__strategic_deployment, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(prsd_be_t10, press_reformation_causality__strategic_deployment, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(prsd_be_t20, press_reformation_causality__strategic_deployment, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(prsd_be_t30, press_reformation_causality__strategic_deployment, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(prsd_be_t40, press_reformation_causality__strategic_deployment, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(prsd_be_t50, press_reformation_causality__strategic_deployment, base_extractiveness, 50, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(prsd_su_t0, press_reformation_causality__strategic_deployment, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(prsd_su_t10, press_reformation_causality__strategic_deployment, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(prsd_su_t20, press_reformation_causality__strategic_deployment, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(prsd_su_t30, press_reformation_causality__strategic_deployment, suppression_requirement, 30, 0.54).
narrative_ontology:measurement(prsd_su_t40, press_reformation_causality__strategic_deployment, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(prsd_su_t50, press_reformation_causality__strategic_deployment, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% Constraint family: press_reformation_causality decomposes into three readings — strategic_deployment (this file), technological_determinism, and co_constitution. The colloquial question 'did the printing press cause the Reformation?' conflates three structurally distinct claims with different epsilon referents: agent-directed deployment (deliberate, targeted extraction; tangled-rope shape), autonomous enabling capacity (no agent-side extraction; inevitability claim), and feedback-loop co-production (distributed agency; rope-leaning). Each is authored as a separate story with its own epsilon, beneficiaries, and metrics; the enabling-capacity claim sits upstream since both other readings presuppose the capacity existed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
