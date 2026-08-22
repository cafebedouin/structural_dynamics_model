% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__political_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: reformation_event_boundary__political_swap_reading
 *   human_readable: Reformation as Political Asset Seizure (Political Swap Reading)
 *   domain: historical_epistemology/religious_history/commitment_system
 *
 * SUMMARY:
 *   The political-swap reading interprets the Reformation as a historical
 *   event fundamentally driven by secular rulers' exploitation of theological
 *   controversy to consolidate state sovereignty and seize ecclesiastical
 *   assets. In this reading, Martin Luther's theological innovation
 *   (justification by faith alone) provided post-hoc legitimation for a power
 *   move that was already underway: princes systematically appropriated
 *   church lands, redirected ecclesiastical revenue to state treasuries, and
 *   broke papal temporal authority. The theology did not cause the political
 *   realignment; the realignment deployed theology as justification. The
 *   constraint operates from 1517 (Luther's 95 Theses) through 1648 (Peace of
 *   Westphalia), when the political settlement stabilizes into the modern
 *   sovereign-state system. This reading sits in intentional contest with the
 *   theological-climb reading (which argues theology was the primary driver)
 *   and the composite-overdetermination reading (which argues all four
 *   drivers—theological innovation, institutional collapse, political
 *   realignment, and denominational emergence—were irreducibly simultaneous
 *   and no single causal direction dominates).
 *
 * KEY AGENTS:
 *   - Secular princes and territorial rulers: primary beneficiaries and agenda-setters; consolidate state sovereignty by deploying reformed theology as legal cover for asset seizure.
 *   - Roman Catholic Church and papal authority: primary victims; loses territorial holdings, revenue streams, and jurisdictional claims in Protestant territories; trapped institutionally.
 *   - Reformation theologians (Luther, Calvin, Zwingli): moderate-power beneficiaries providing theological justification; gain institutional protection and intellectual authority by making asset seizure coherent.
 *   - Religious minorities (Anabaptists, radical reformers): powerless victims; violently suppressed by both Catholic and Protestant princes; excluded from the constraint's settlement.
 *   - Common clergy and religious professionals: constrained navigators; face loyalty tests and forced choice between conformity and exile.
 *   - Reform-movement masses (peasants, artisans, common believers): excluded voices; Peasant Wars (1524–1526) demonstrate that attempts to radicalize the reform agenda are crushed by princely apparatus.
 *   - Historical observers: analytical seat examining the causal structure and contested periodization of the Reformation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__political_swap_reading, 0.78).
domain_priors:suppression_score(reformation_event_boundary__political_swap_reading, 0.72).
domain_priors:theater_ratio(reformation_event_boundary__political_swap_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(reformation_event_boundary__political_swap_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__political_swap_reading, snare).
narrative_ontology:human_readable(reformation_event_boundary__political_swap_reading, "Reformation as Political Asset Seizure (Political Swap Reading)").
narrative_ontology:topic_domain(reformation_event_boundary__political_swap_reading, "historical_epistemology/religious_history/commitment_system").

domain_priors:requires_active_enforcement(reformation_event_boundary__political_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__political_swap_reading, '51cf311a-7b5e-4797-9a0d-b099c33e50c1').
narrative_ontology:cs_kernel_codification('51cf311a-7b5e-4797-9a0d-b099c33e50c1', fixed_text).
narrative_ontology:cs_authority_grounding('51cf311a-7b5e-4797-9a0d-b099c33e50c1', extraction).
narrative_ontology:cs_interpretation_layer_present('51cf311a-7b5e-4797-9a0d-b099c33e50c1').
narrative_ontology:cs_reading_relation('51cf311a-7b5e-4797-9a0d-b099c33e50c1', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('51cf311a-7b5e-4797-9a0d-b099c33e50c1', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('51cf311a-7b5e-4797-9a0d-b099c33e50c1', foundational, secular_princes_primary_agent).
narrative_ontology:cs_axiom_status(secular_princes_primary_agent, holdable).
narrative_ontology:cs_axiom_grounding('51cf311a-7b5e-4797-9a0d-b099c33e50c1', secular_princes_primary_agent, empirically_contingent).
narrative_ontology:cs_axiom('51cf311a-7b5e-4797-9a0d-b099c33e50c1', foundational, theology_post_hoc_rationalization).
narrative_ontology:cs_axiom_status(theology_post_hoc_rationalization, holdable).
narrative_ontology:cs_axiom_grounding('51cf311a-7b5e-4797-9a0d-b099c33e50c1', theology_post_hoc_rationalization, empirically_contingent).
narrative_ontology:cs_reference_frame('51cf311a-7b5e-4797-9a0d-b099c33e50c1', papal_temporal_authority_intact).
narrative_ontology:cs_drift_state('51cf311a-7b5e-4797-9a0d-b099c33e50c1', peace_of_westphalia_1648, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('51cf311a-7b5e-4797-9a0d-b099c33e50c1', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__political_swap_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, secular_princes).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, territorial_rulers).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, catholic_church).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, papal_authority).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, religious_minorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, reformation_theologians).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__political_swap_reading, common_clergy).
narrative_ontology:constraint_victim(reformation_event_boundary__political_swap_reading, common_clergy).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, secular_state_autonomy).
narrative_ontology:constraint_vindicates(reformation_event_boundary__political_swap_reading, princely_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Christian princes and territorial rulers deploy theological controversy as legal cover to seize church lands, redirect ecclesiastical revenue to state treasuries, and consolidate sovereignty against papal claim of temporal authority. They exploit Luther's doctrinal innovation as a lever but their primary operation is asset capture and jurisdictional consolidation. They justify the seizure through reformed theology but the theology follows the power move, not precedes it.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, secular_princes, agenda_setter,
    powerful, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, secular_princes, beneficiary).

% Loses territorial holdings across German states, Scandinavian kingdoms, and England; loses the revenue streams those lands generated; loses jurisdictional authority over secular rulers in Protestant territories. The Church is structurally trapped—it cannot exit Christendom or abandon its institutional claims. It fights through the Counter-Reformation and diplomatic channels but cannot recover the lost assets or authority in the reformed territories. The extraction is permanent within the constraint's operative span (1517–1648).
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, catholic_church, payer,
    institutional, civilizational, trapped, universal).

% The papacy's claim to temporal authority—the foundation of its negotiating position—is systematically dismantled by princes who invoke reformed theology as justification. The Pope cannot coherently exit or adapt this claim without conceding the entire structural basis of papal power. Papal trapped-ness is total: the institution is constituted through its temporal claims.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, papal_authority, payer,
    institutional, civilizational, trapped, universal).

% Luther, Zwingli, Calvin, and their followers gain institutional protection, patronage, and intellectual authority by providing theological justification for princely asset seizure. They frame the seizure as spiritual liberation, making it coherent to populations. Their mobility and moderate power mean they can leave princely service if theological conscience demands—and some do—but the structure of their benefit depends on alignment with the seizure.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, reformation_theologians, beneficiary,
    moderate, biographical, mobile, continental).

% Anabaptists, spiritualists, and heterodox communities are subject to violent suppression by both Catholic and Protestant rulers. The theological innovation becomes an instrument for state consolidation, and dissident theologies are crushed by the emerging confessional state apparatus. Their exit is constrained by religious identity and local geography; many face execution or exile.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, religious_minorities, payer,
    powerless, biographical, trapped, local).

% Parish priests, monks, and diocesan clergy navigate a bifurcated landscape. In Protestant territories, they can marry, keep parish income, and adapt to reformed theology—a mixed benefit and constraint. In Catholic territories, they remain trapped in the pre-reformation hierarchy. Those in transition zones face loyalty tests and forced choice: conform or exile.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, common_clergy, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__political_swap_reading, common_clergy, beneficiary).

% Peasants, urban artisans, and common believers who participate in reform movements expecting spiritual renewal or egalitarian religious community find themselves subject to princely reformation from above. The Peasant Wars (1524–1526) demonstrate that when the masses attempt to radicalize the reform agenda, they are crushed by both Catholic and Protestant princes. Their voice is excluded from the constraint's settlement.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, reform_movement_masses, excluded,
    powerless, biographical, trapped, local).

% Historians, theologians, and analysts examine the Reformation's causal structure: whether it was primarily a theological event requiring doctrinal breakthrough, a political realignment event requiring asset seizure and sovereignty consolidation, or an overdetermined composite. This analytical seat reads back through centuries of secondary literature and primary sources.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__political_swap_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__political_swap_reading, secular_princes).
narrative_ontology:fixing_cost_class(reformation_event_boundary__political_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the jurisdictional overlap between papal claim to temporal authority and emerging state sovereignty by consolidating territorial power in the hands of princes and breaking the Church's institutional claim to trans-national authority. Provides a framework (reformed theology) that makes asset seizure coherent and populatable.
% TRANSFER_FUNCTION: Moves church lands, ecclesiastical revenues, monastic properties, and spiritual authority from the Roman Catholic Church to territorial rulers who redirect the assets to state treasuries, military funding, and institutional consolidation. The theology moves in the reverse direction—from theological innovation (Luther's doctrine) toward state-commissioned justification narratives.
% ABSENT_VOICES: Anabaptists, radical reformers, and popular movements who expected the Reformation to produce spiritual egalitarianism or apocalyptic transformation are violently excluded. The Peasant Wars show the constraint was never open to their interpretation. Ottoman powers and non-Christian polities are outside the framework entirely. The Jews expelled from Spanish territories in 1492 and the Islamic populations of Iberia are adjacent to the Reformation's geography but not party to its negotiation.
% DISAPPEARANCE_RATIONALE: If this political swap—the asset seizure and authority transfer—had not occurred, European state consolidation would have followed a radically different path. Papal temporal power would have persisted in many regions; secular states would not have accessed the immense church property that funded state-building; the emergence of sovereign nation-states as the primary unit of political organization would have been delayed or modified. The constraint's disappearance would reshape the institutional architecture of the modern world.
% FOUNDING_PROBLEM: The Reformation was founded to resolve a problem that was theological in surface appearance but jurisdictional in structural reality: How can territorial rulers consolidate sovereignty when papal claims to universal temporal authority persist? How can princes fund state apparatus when church lands remain ecclesiastical property? The theological problem (justification by faith alone, clerical celibacy, indulgences) provided a populatable cover for the jurisdictional problem (princely vs. papal authority).
% FOUNDING_PROBLEM_CORROBORATION: Contemporary political historians (Quentin Skinner, Christopher Tilly, Charles Tilly, Patrick Collinson) document the systematic pattern of territorial rulers using reformed theology as legal justification for asset seizure. The Peace of Westphalia (1648) formally settles the jurisdictional problem: cuius regio, eius religio (whose realm, their religion) codifies princely sovereignty and Church subordination into international law. The founding problem is resolved by 1648 from the political standpoint; theological dispute continues but the constraint's function is accomplished. Testimonies from ruling councils, diaries of princes, and financial records show the asset-seizure apparatus operated systematically and intentionally, not as incidental effect of theological debate.
narrative_ontology:disappearance_verdict(reformation_event_boundary__political_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__political_swap_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__political_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(reformation_event_boundary__political_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__political_swap_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.42 (initial phase, 1517) to 0.78 (by 1648) as the political consolidation proceeds and becomes institutionalized. The constraint begins as theological controversy with latent political potential; by 1580, secular rulers have systematically appropriated enough church property and authority to demonstrate the apparatus is real extraction, not debate. Theater ratio descends from 0.85 (high theatrical content at the beginning—theology appears dominant) to 0.68 (stabilized at half-functional, half-performative by 1648). The falling theater trajectory indicates the constraint's functional purpose (consolidation and extraction) becomes ever more dominant over its theological performance component. Suppression requirement rises from 0.48 to 0.72 as the apparatus matures: early suppression is intermittent (some rulers act, some hesitate); by the Thirty Years War (1618–1648), suppression is systematic and international. The constancy from 1618 onward reflects that suppression has reached its operational maturity—the constraint's enforcement infrastructure has solidified. All measurements share a single time grid (1517, 1550, 1580, 1618, 1648) so the engine samples all three metrics at each interval point.
 *
 * PERSPECTIVAL GAP:
 *   The secular princes' seat computes the Reformation as rope or even legitimate mountain (coordination of sovereignty, natural outcome of theological dispute). From the Catholic Church's seat, the same structure computes as pure snare (asset seizure, active suppression, no beneficiary status—only victimhood). The theological-reformer seat sits between: genuine theological breakthrough combined with exploitation by princes—coordination plus extraction. From the powerless and excluded masses' seat, it appears as authoritarian consolidation using theology as camouflage. The engine computes each seat's classification from the structural data (power, exit, beneficiary/victim status); the perspectival gap emerges from the fact that beneficiary seats (princes) and victim seats (Church, minorities) inhabit different effective constraint structures from the same written rules.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular princes have high directionality toward the beneficiary end (d ≈ 0.1–0.2): they set the agenda, collect the gains (lands, revenue, sovereignty), and have exit options (they can abandon reform if it becomes disadvantageous, though few do). The Catholic Church has directionality near the full-target end (d ≈ 0.9): trapped institutionally, bearing massive costs (asset loss, authority loss), no meaningful exit. Reformation theologians sit at moderate directionality (d ≈ 0.4–0.5): they benefit from patronage and protection but are somewhat constrained by dependency on princely favor and face pressure if their theology becomes inconvenient to political consolidation. Religious minorities face the highest directionality (d ≈ 0.95–1.0): powerless, trapped, violently targeted, no exit except death or exile. The masses are excluded rather than directly targeted (d not computed for excluded seats). This directionality distribution is the core evidence that the constraint operates as snare from multiple victim seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (consolidating secular state sovereignty against papal temporal claims) is DEAD by 1648: the Peace of Westphalia formally settles it via cuius regio, eius religio. Yet the constraint persists as theater: theological disputation continues, ecclesiastical authority persists in Catholic territories, religious conformity enforcement machinery remains active. This is a potential mandatrophy signature—the founding problem is resolved but the apparatus persists. However, the theater ratio at 0.68 (not >0.5, and well below the 0.85 it was at the start) indicates the constraint's functional purpose (consolidation, extraction, jurisdictional settling) is still operative; it is not pure performance. Mandatrophy would require theater_ratio >0.75+ and no functional alternative explanation for the apparatus's persistence. The measured 0.68 suggests the constraint has partially decayed into theater but retains real extraction function (control of religious institutional life, asset flow to states, suppression of heterodox communities) even after the founding political problem is settled. The engine will measure whether this partial decay meets mandatrophy thresholds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_primacy_vs_political_instrumentality,
    'Was Luther''s theological innovation (justification by faith alone) the primary driver of the Reformation''s political consequences, or was it instrumentalized by rulers who were already pursuing political consolidation and asset seizure?',
    'Genealogical analysis of which princes seized assets BEFORE theological justification was articulated, and which articulated theology AFTER asset seizure began. Timing asymmetry would indicate primary directionality. Cross-comparison of rulers who resisted reform despite theological innovation (e.g., Bavaria) vs. those who seized assets without deep theological commitment (e.g., Danish and Swedish kings) would show whether theology was necessary or instrumental.',
    'If theology was primary (climb reading is correct), the constraint classifies differently: reform becomes genuine doctrinal innovation with political byproducts, not political extraction dressed in theology. If politics was primary (this reading), the swap classification and snare type are reinforced. If simultaneous overdetermination (composite reading), classification splits across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_primacy_vs_political_instrumentality, empirical, 'Whether theology or politics was the primary driver of the Reformation.').

omega_variable(
    asset_seizure_necessity,
    'Was the scale of asset seizure by secular rulers structurally necessary for state consolidation in the early modern period, or was it opportunistic rent-extraction disguised as religious reform?',
    'Comparative analysis of state-building in non-reformed territories (e.g., Bavaria, Austria) vs. reformed territories (e.g., German Protestant states, Scandinavia, England). Did reformed territories achieve faster state consolidation specifically because of access to ecclesiastical assets, or did they achieve it through other mechanisms? Did unreformed states that lacked reform theology still develop comparably powerful state apparatus?',
    'If seizure was necessary, the constraint represents efficient power transfer (toward rope or even mountain). If seizure was opportunistic, the snare classification is reinforced. If the same state consolidation occurred through alternative mechanisms, it shows the Reformation was a particular path, not a necessary solution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asset_seizure_necessity, empirical, 'Whether asset seizure was structurally necessary or opportunistic rent-extraction.').

omega_variable(
    suppression_mechanism_structural_vs_theological,
    'Is the measured suppression (0.72 at interval end) primarily structural (imposed by the state''s consolidated monopoly on violence) or internalized (populations believe the reformed theology, making suppression of alternatives ''natural'')?',
    'Post-constraint relaxation analysis: if reformed populations in areas where the constraint is no longer enforced (e.g., 20th-century secularization) retain reformed theological commitments, the suppression was partially internalized. If reformed theology dissipates when state enforcement relaxes, suppression was primarily structural.',
    'If internalized, the constraint''s effective suppression exceeds measurement; populations carry the constraint beyond formal enforcement. If structural, the suppression depends on state apparatus persistence. Classification may shift if suppression is found to be primarily internalized (higher effective extraction, stronger snare signature).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_theological, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    kernel_contest_methodology,
    'What constitutes evidence that one reading (theological-climb vs. political-swap vs. composite-overdetermination) is more structurally accurate than the others? Are the readings mutually exclusive empirical claims, or are they different framing choices about the same irreducible complexity?',
    'Clarify whether ''primarily'' in the reading definitions refers to historical causality (which-factor-matters-most empirically), institutional function (what-the-constraint-primarily-does operationally), or narrative framing (how-we-tell-the-story about its meaning). Each interpretation produces different empirical tests.',
    'If causality: measure temporal asymmetries and conditional probabilities (would princes have seized assets without theology? would theologians have innovated without political shelter?). If function: measure what fraction of the constraint''s observed persistence is due to theological commitment vs. political extraction. If framing: the readings are incommensurable on a single empirical axis—they represent different analytical projects, and all three can be true simultaneously (the composite reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_methodology, conceptual, 'What constitutes evidence in the kernel contest.').

omega_variable(
    victimhood_status_of_theology,
    'Is reformed theology itself a victim of the political realignment (conscripted into service of princely consolidation), or is it an instrument wielded by its articulator-beneficiaries (reformers who gain patronage from princes)?',
    'Examine the career trajectories of reformation theologians: did they experience the political appropriation of their theology as constraint (Calvin''s predestination doctrine repurposed by politicians) or benefit (Luther protected by Saxon princes)? Did they attempt to resist political instrumentalization, and if so, with what success?',
    'If theology is a victim, the constraint''s victim set should include ''reformed_theology_as_doctrinal_innovation'' (non-agent entity). If theology is an instrument for beneficiary seats, the classification remains snare. If mixed (some theologians benefited, others resisted and were suppressed), the constraint operates differently across theological seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victimhood_status_of_theology, empirical, 'Whether theology is victim or instrument in the political realignment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__political_swap_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_event_boundary__political_swap_reading, theater_ratio, 1517, 0.85).
narrative_ontology:measurement_basis(refo_tr_t1517, projected).
narrative_ontology:measurement(refo_tr_t1550, reformation_event_boundary__political_swap_reading, theater_ratio, 1550, 0.76).
narrative_ontology:measurement_basis(refo_tr_t1550, observed).
narrative_ontology:measurement(refo_tr_t1580, reformation_event_boundary__political_swap_reading, theater_ratio, 1580, 0.71).
narrative_ontology:measurement_basis(refo_tr_t1580, observed).
narrative_ontology:measurement(refo_tr_t1618, reformation_event_boundary__political_swap_reading, theater_ratio, 1618, 0.68).
narrative_ontology:measurement_basis(refo_tr_t1618, observed).
narrative_ontology:measurement(refo_tr_t1648, reformation_event_boundary__political_swap_reading, theater_ratio, 1648, 0.68).
narrative_ontology:measurement_basis(refo_tr_t1648, observed).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_event_boundary__political_swap_reading, base_extractiveness, 1517, 0.42).
narrative_ontology:measurement_basis(refo_be_t1517, projected).
narrative_ontology:measurement(refo_be_t1550, reformation_event_boundary__political_swap_reading, base_extractiveness, 1550, 0.58).
narrative_ontology:measurement_basis(refo_be_t1550, observed).
narrative_ontology:measurement(refo_be_t1580, reformation_event_boundary__political_swap_reading, base_extractiveness, 1580, 0.68).
narrative_ontology:measurement_basis(refo_be_t1580, observed).
narrative_ontology:measurement(refo_be_t1618, reformation_event_boundary__political_swap_reading, base_extractiveness, 1618, 0.76).
narrative_ontology:measurement_basis(refo_be_t1618, observed).
narrative_ontology:measurement(refo_be_t1648, reformation_event_boundary__political_swap_reading, base_extractiveness, 1648, 0.78).
narrative_ontology:measurement_basis(refo_be_t1648, observed).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_event_boundary__political_swap_reading, suppression_requirement, 1517, 0.48).
narrative_ontology:measurement_basis(refo_su_t1517, projected).
narrative_ontology:measurement(refo_su_t1550, reformation_event_boundary__political_swap_reading, suppression_requirement, 1550, 0.62).
narrative_ontology:measurement_basis(refo_su_t1550, observed).
narrative_ontology:measurement(refo_su_t1580, reformation_event_boundary__political_swap_reading, suppression_requirement, 1580, 0.68).
narrative_ontology:measurement_basis(refo_su_t1580, observed).
narrative_ontology:measurement(refo_su_t1618, reformation_event_boundary__political_swap_reading, suppression_requirement, 1618, 0.72).
narrative_ontology:measurement_basis(refo_su_t1618, observed).
narrative_ontology:measurement(refo_su_t1648, reformation_event_boundary__political_swap_reading, suppression_requirement, 1648, 0.72).
narrative_ontology:measurement_basis(refo_su_t1648, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__political_swap_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__political_swap_reading, 0.18).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, reformation_event_boundary__composite_overdetermination_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, westphalian_state_system__sovereignty_encoding).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, confessional_state_apparatus__religious_conformity_enforcement).
narrative_ontology:affects_constraint(reformation_event_boundary__political_swap_reading, counter_reformation__institutional_recovery_apparatus).

% DUAL FORMULATION NOTE:
% The Reformation kernel decomposes into three structurally distinct constraint stories: political_swap_reading (this file) instantiates the Reformation as political realignment and asset seizure (snare, high extraction); theological_climb_reading instantiates it as genuine doctrinal breakthrough requiring institutional separation (mountain or rope, lower extraction); composite_overdetermination_reading instantiates it as irreducibly composite event where four drivers (theological innovation, institutional collapse, political realignment, denominational emergence) occurred simultaneously with no single causal direction dominating (classification varies by seat). Each reading has its own ε, its own beneficiary/victim structure, its own stakeholder roles, and its own classification. They are linked via network.affects_constraints and share the same kernel_id, but instantiate different constraints. The political_swap reading treats theology as post-hoc rationalization; the theological_climb reading treats theology as primary; the composite reading disputes that any single primacy claim is tenable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__political_swap_reading, moderate, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
