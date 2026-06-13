% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Technology-Reformation Co-Constitution Mechanism
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The co-constitution reading argues that the Reformation and printing
 *   press co-evolved through mutual feedback, not through one causing the
 *   other. Reformers did not invent the printing press, but they
 *   strategically adopted it, sponsored its deployment for vernacular
 *   scripture, and shaped what got printed. Printers responded to reformation
 *   demand and developed print capacity accordingly. The printing press was
 *   not predetermined to drive the Reformation—medieval printers printed
 *   mostly indulgences, liturgical texts, and legal documents for the first
 *   30 years. The Reformation as a historical event emerged from the
 *   interaction of reformer intention, printer business logic, lay literacy
 *   growth, and the technical affordances of moveable type. This reading
 *   contests both technological determinism (printing alone caused the
 *   Reformation) and pure beneficiary agency (reformers used printing as a
 *   tool they fully controlled). Instead, technology and social actors
 *   co-constituted each other: technology enabled new forms of coordination;
 *   social actors shaped what the technology was used for; the resulting
 *   coordination was irreducible to either alone.
 *
 * KEY AGENTS:
 *   - reform_movement_agents: Organized theologians, clergy, civic magistrates, educated merchants seeking to distribute vernacular scripture and challenge Church interpretive monopoly (power: organized, exit: mobile, spatial: continental)
 *   - printing_technology_operators: Printers and print workshops responding to market demand for religious texts, developing capacity and distribution networks (power: moderate, exit: constrained, spatial: regional)
 *   - church_ecclesiastical_authority: Lost interpretive monopoly through the co-constitution mechanism but actively tried to suppress and compete with printing (power: institutional, exit: trapped, spatial: continental)
 *   - manuscript_scribal_economy: Displaced by printing's superior economics but not through purely technological superiority—through the strategic choice to print reformation texts (power: moderate, exit: constrained, spatial: regional)
 *   - lay_readers_vernacular_literate: Gained access to scripture without ecclesiastical mediation, but access was unequal and required reformer/printer coordination (power: powerless, exit: mobile, spatial: continental)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.38).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.22).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Technology-Reformation Co-Constitution Mechanism").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__co_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, '08c7888e-8ac0-42ae-b663-52e5beeaf2cf').
narrative_ontology:cs_kernel_codification('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', distributed).
narrative_ontology:cs_authority_grounding('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', distributed).
narrative_ontology:cs_reading_relation('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', technology_reformation_causality__technological_determinism_reading, influences).
narrative_ontology:cs_reading_relation('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_axiom('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', foundational, bidirectional_causality_between_technology_and_agency).
narrative_ontology:cs_axiom_status(bidirectional_causality_between_technology_and_agency, holdable).
narrative_ontology:cs_axiom_grounding('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', bidirectional_causality_between_technology_and_agency, instrumental).
narrative_ontology:cs_axiom('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', foundational, neither_actor_alone_is_sufficient).
narrative_ontology:cs_axiom_status(neither_actor_alone_is_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', neither_actor_alone_is_sufficient, deontological).
narrative_ontology:cs_reference_frame('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', independent_technology_and_agency).
narrative_ontology:cs_drift_state('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', post_reformation_consolidation_1550, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('08c7888e-8ac0-42ae-b663-52e5beeaf2cf', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reform_movement_agents).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printing_technology_operators).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, church_ecclesiastical_authority).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, manuscript_scribal_economy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, lay_readers_vernacular_literate).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, printing_technology_operators).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, bidirectional_causality_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reformers—theologians, local clergy, civic magistrates, educated merchants—sought to distribute scripture directly to lay readers in vernacular languages, bypassing ecclesiastical gatekeepers. They did not invent the printing press, but they strategically adopted it, sponsored its deployment, shaped what got printed, and mobilized it to amplify their theological claims. Their ability to do so depended on printing technology's existence, but the technology's utility for their agenda was not inevitable—it required deliberate choice to print vernacular scripture at scale, to recruit printers, to organize distribution networks, and to defend the practice against Church suppression.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reform_movement_agents, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, reform_movement_agents, beneficiary).

% Printers and print workshops gained markets and patronage from printing religious texts and controversial pamphlets. They were economic agents responding to demand and seeking profit, not autonomous causes of the Reformation. But the medium itself—moveable type, mechanical reproduction, distribution speed—enabled forms of persuasion and coordination that manuscript culture could not. Their business model co-evolved with reformation demand: as reformers needed texts, printers built capacity and developed distribution; as printing capability grew, new textual practices became possible. The constraint operates through this interaction.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printing_technology_operators, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, printing_technology_operators, payer).

% The Catholic Church had held authority over doctrinal interpretation and scripture access through its monopoly on literacy, manuscript production, and textual authority. The co-constitution mechanism eroded this authority—not because printing made it mathematically impossible to suppress (the Church tried and partly succeeded), but because the interaction of reformer intention + printing capacity + distribution networks + lay literacy growth created an asymmetry the Church could not fully close. The Church bore the cost of losing exclusive interpretive authority as lay readers could now compare their own vernacular texts to the Latin Vulgate and ecclesiastical teaching.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, church_ecclesiastical_authority, payer,
    institutional, civilizational, trapped, continental).

% Manuscript scribes and the scribal production economy were displaced by printing's superior speed and cost structure. The constraint operated partly through technological superiority (printing was faster and cheaper per copy), but the transition was not purely technological—it required reformers to choose printed texts as their distribution medium, printers to invest in capacity, and patrons to prefer printed over manuscript for religious texts. The scribal economy bore the cost of technological displacement, but the constraint's persistence depended on all three parties' choices.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, manuscript_scribal_economy, payer,
    moderate, biographical, constrained, regional).

% Newly literate or semi-literate lay readers in vernacular languages gained access to religious texts—scripture, devotional works, polemical tracts—that had been restricted to clergy and educated elites. This was a genuine coordination gain: they could now read scripture without ecclesiastical mediation. But the availability of vernacular printing did not automatically translate to access—it required reformers to fund printing, printers to produce texts, and distribution networks to get them into lay hands. The constraint enabled their access but did not determine it.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, lay_readers_vernacular_literate, beneficiary,
    powerless, biographical, mobile, continental).

% The Catholic Church's eventual response—the Counter-Reformation, the Index of Prohibited Books, the printing of Catholic vernacular texts—acknowledged the constraint's operation: the Church could not simply suppress printing (it was too useful), but had to engage it, compete for control of the printed text, and deploy printing for its own theological purposes. This response was absent from the constraint's initial formation but became crucial to its evolution. Their voice was excluded from the mechanism's founding but would reshape its operation.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, counter_reformation_church, excluded,
    institutional, generational, trapped, continental).

% Historians and analysts examine the mechanisms by which technology and intentional human actors co-constitute historical outcomes. They assess whether causality is unidirectional (technology causes history, or human agency causes history) or bidirectional (technology and agency mutually shape each other through interaction). Their analytical frame reveals the structure of the constraint.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, historical_observer_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__co_constitution_reading, reform_movement_agents).
narrative_ontology:fixing_cost_class(technology_reformation_causality__co_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The co-constitution mechanism solves a fundamental coordination problem: how can reformers disseminate theological alternatives to a monopoly interpretation (the Church's authority) when they lack institutional power? The mechanism works through the interaction of reformer intention + printing capacity + distribution networks. Printing technology enabled mass reproduction; reformers' strategic choice to use it for vernacular scripture; printers' business interest in serving that demand; lay literacy growth. No single actor caused this coordination—it emerged from the structural compatibility of these elements. The coordination problem was: disseminate vernacular theology widely enough to challenge ecclesiastical monopoly on interpretation. The solution was: co-evolve printing capacity with reformation demand.
% TRANSFER_FUNCTION: The constraint transfers interpretive authority from the Church (via its manuscript and latinate monopoly) to lay readers and reformers (via their access to vernacular printed texts). It also transfers economic value from the scribal economy to the printing economy. The transfer is not extraction in the snare sense—it is asymmetric (the Church loses more than the scribal economy; lay readers gain more than printers), but it is the output of coordinated action by all parties, none of whom fully intended the full outcome.
% ABSENT_VOICES: The voices excluded from the co-constitution mechanism at its founding were the Church's counter-reformers, who would later engage the printed text and deploy printing for Catholic purposes. Their absence meant the initial mechanism was read as one-directional (reformers using printing against Church authority) when it later became bidirectional (both Church and reformers competing through print). Indigenous readers whose languages were not chosen for vernacular printing, rural populations with no access to printed texts, women excluded from clerical education and often from lay reading groups—these populations would have contested the framing that printing enabled 'lay' access, since access was actually unequal by geography, gender, literacy level, and language.
% DISAPPEARANCE_RATIONALE: If the co-constitution mechanism disappeared—i.e., if printing had never been invented or if reformers had not adopted it—the religious landscape would have rearranged fundamentally. Some form of religious challenge to Church authority would likely have emerged (pre-Reformation heresies show the Church was contestable even before printing), but the scale, speed, and geographic reach of the Reformation would have differed radically. Manuscript-based reform movements were slower, more geographically fragmented, more vulnerable to suppression. Whether the Reformation as a historical event would have occurred at all is contested: technological determinists say no (printing was necessary); beneficiary-agency readers say yes (reformer will would have found another way); co-constitution readers say the question itself is malformed—the Reformation as we know it was the product of the interaction, not of either actor alone.
% FOUNDING_PROBLEM: The founding problem was theological and institutional: the Church claimed interpretive authority over scripture based on Latin scholarship, clerical training, and ecclesiastical tradition. Reformers believed lay readers should access scripture directly in their own language and form their own judgment. Before printing, this was aspirational—a few manuscript Bibles existed in vernacular, but distribution was tiny. The problem was: how to scale vernacular scripture access against Church opposition and resource constraints?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is no longer live in the historical sense (the Reformation happened, the Church's monopoly on interpretation was broken, vernacular scripture is now globally distributed), but the mechanism that solved it persists in the historical record. Early reformers and printers explicitly attested to the problem and their intention to solve it (Luther's writings on scripture availability, printer's prefaces, magisterial statements). Church authorities attested to the threat they perceived. But corroboration from outside the benefiting parties comes from historians and observers who recognize the co-constitution structure: Elizabeth Eisenstein's work on the printing press and the Renaissance; historians of the Reformation who document both reformer strategy and printer business logic; sociologists and historians of technology who study how technologies and social movements co-evolve. The founding problem is dead; the founding mechanism's operation remains a matter of interpretive contention.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).
:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises steeply from 1440 to 1510 (0.12 to 0.38) as printing capacity expands, reformer-printer coordination deepens, and vernacular texts saturate the market. It plateaus after 1510 as the Counter-Reformation begins and the Church enters the printing competition itself—the asymmetry stabilizes. Suppression mirrors this arc: minimal when printing is scarce (1440), rising as the Church recognizes the threat and tries to ban books (1490s onward), plateauing as the Church accepts it must engage printing rather than suppress it entirely. Theater ratio is low throughout (0.05 to 0.18) because the coordination is genuine: reformers really do intend to distribute scripture; printers really do profit from it; lay readers really do gain access. The theater that emerges (increasing from 1490 onward) is the Church's performative hostility to printing while eventually using printing for its own texts. The constraint is Tangled Rope because it coordinates (reformers + printers + lay readers solve a distribution problem) AND asymmetrically extracts (Church loses authority; scribal economy is displaced) AND requires active enforcement (the Church must actively suppress vernacular printing to slow it, but cannot prevent it entirely).
 *
 * PERSPECTIVAL GAP:
 *   Reformers see the constraint as coordinating their theological movement; the Church sees it as threatening their authority; printers see it as profitable opportunity; lay readers see it as access they desperately want but cannot fully control; the Counter-Reformation Church eventually sees it as a medium they must master to compete. Each seat computes a different local type from the same structural arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are beneficiaries (they gain authority and market share) with moderate mobility (they can redirect their efforts if printing becomes suppressed, though it is costly). The Church is a clear victim (loses interpretive monopoly, bears suppression costs) with no exit (it is trapped as the institution defending the old order). The scribal economy is a victim (displaced) with constrained exit (scribes could retrain but face retraining costs). Lay readers are beneficiaries (gain access) but powerless and somewhat mobile (they can seek texts through networks but cannot organize supply themselves). This creates the asymmetry: concentrated beneficiaries (organized reformers, capital-holding printers) versus diffuse victims (Church losing monopoly, scattered scribes, widely distributed lay readers). The directionality computations are: reformers d ≈ 0.2 (low d, beneficiaries), Church d ≈ 0.8 (high d, victims), printers d ≈ 0.35 (moderate benefit, constrained by capital and patron dependence), lay readers d ≈ 0.4 (slight benefit, powerless). The asymmetry is not extreme (no d at 1.0 or 0.0) because all parties are locked into the interaction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy because the founding problem (distribute vernacular scripture widely) remains live through the 1550 endpoint and beyond. The Church does not accept the reform agenda, but it does accept that printing exists and must be engaged. The mechanism persists not out of inertia but out of continued contention: both the Church and reformers continue to use printing to advance their theological positions. The constraint is not a zombie—it is an active field of competition. A piton reading (atrophied alternatives, mostly theater) does not fit because the coordination function (mass text distribution) remains real and non-theatrical, and the extraction (Church authority loss) is actively resisted, not performed. The claim (Tangled Rope) matches the measured structure: genuine coordination + asymmetric extraction + active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_reformation_without_printing,
    'Would the Reformation have occurred—in any form—if printing had not been invented, or if it had been invented but not adopted for religious texts?',
    'Comparative history: examine pre-Reformation reform movements (Wycliffe, Hus) and assess how far they spread without printing; study contemporary religious movements in non-literate societies; analyze the theological content of the Reformation to assess whether it was dependent on printed scripture or whether it would have emerged through other mechanisms (oral networks, manuscript distribution, institutional reform movements).',
    'If the Reformation would have occurred substantially as it did without printing, the co-constitution reading dissolves into beneficiary-agency and printing becomes a secondary accelerant rather than a co-cause. If the Reformation would not have occurred without printing at scale, the co-constitution reading strengthens and technological determinism gains ground. If a different religious movement would have occurred without printing, the reading shifts to: printing enabled specifically reformation outcomes, not religious change in general.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_reformation_without_printing, conceptual, 'Whether the Reformation was contingent on printing or would have emerged through other mechanisms.').

omega_variable(
    reformer_intent_vs_printing_capability,
    'How much of the Reformation''s geographic reach, speed, and textual form was shaped by reformers'' strategic choices to use printing, versus by printing''s own technical affordances and cost structures?',
    'Textual analysis of early printed religious texts: examine what reformers chose to print (vernacular scripture vs. polemics vs. devotional texts) and why; compare printed texts to manuscript precursors to assess what printing enabled that manuscript could not; analyze printer''s colophons and correspondence to identify printer agency versus reformer direction; study suppression attempts to assess what the Church could and could not control.',
    'If reformers made deliberate choices that shaped printing''s application (they did), the co-constitution reading is strengthened: printing was necessary but not determining. If printing''s technical affordances (speed, cost, distribution potential) drove reformers'' choices more than reformer theology drove printing choices, technological determinism gains ground.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformer_intent_vs_printing_capability, empirical, 'The balance of strategic intent versus technical enablement in the co-constitution mechanism.').

omega_variable(
    church_adaptation_trajectory,
    'At what point did the Church transition from trying to suppress printing to adopting it for Catholic purposes, and what does this trajectory reveal about the constraint''s nature?',
    'Historical analysis of Church policy on printing: examine dates of suppression edicts (Index of Prohibited Books, bans on vernacular translation), compare to dates of Church-sponsored printing of Catholic texts; analyze the rhetoric of Church authorities over time to track whether they viewed printing as a tool they could control or as a threat they must manage.',
    'If the Church rapidly adopted printing after initial suppression (it did, beginning mid-16th century), this supports co-constitution: the Church''s adaptation shows it recognized printing as a medium it must engage, not eliminate. If the Church''s adoption of printing for its own purposes significantly slowed reformation-friendly printing, the extraction component strengthens: the Church used control of suppression to recapture interpretive authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(church_adaptation_trajectory, empirical, 'Whether the Church''s adaptation of printing supports or undermines the co-constitution reading.').

omega_variable(
    lay_reader_agency_in_textual_reception,
    'How much did lay readers'' reception and use of printed texts shape what reformers and printers chose to produce, versus passively receiving what reformers decided to print?',
    'Textual scholarship on annotations in surviving printed Bibles and devotional texts; analysis of what reformers knew about lay reader demand and how it shaped printing decisions; study of how lay readers commented on, shared, and distributed texts in their own networks; examination of reformer complaints about lay misinterpretation to assess how lay readers actively constructed meaning.',
    'If lay readers significantly shaped what got printed through their demand and reception, the co-constitution mechanism includes a third feedback loop (lay reader → printer/reformer → printing choice), making it more complex but also more genuinely coordinative. If lay readers were passive consumers of reformer-determined content, the mechanism simplifies to reformer-printer coordination with lay readers as beneficiaries but not agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_reader_agency_in_textual_reception, empirical, 'The extent of lay reader agency in shaping the constraint''s operation.').

omega_variable(
    printing_technology_determinism_boundary,
    'What specific technical features of moveable-type printing (as opposed to manuscript or other reproductive technologies) were necessary and sufficient for the Reformation''s scale?',
    'Comparative technology history: analyze whether woodblock printing, single-sheet pamphlets, or other pre-moveable-type reproductive technologies could have achieved similar distribution; assess the cost structure of different technologies and their effect on print run sizes; examine what reformers specifically exploited about moveable type''s affordances versus what would have been possible with other technologies.',
    'If moveable-type printing''s specific affordances (reusable type, large print runs, low marginal cost per copy) were genuinely necessary for the Reformation''s scale, the technological co-constitution is real. If simpler reproductive technologies could have achieved similar effects, printing''s role is less determinative and more contingent on reformer choice to use the particular technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printing_technology_determinism_boundary, empirical, 'Which technical features of moveable-type printing drove the co-constitution mechanism.').

omega_variable(
    kernel_reading_foreclosure,
    'Do the three kernel readings (technological_determinism, beneficiary_agency, co_constitution) represent genuinely distinct causal claims, or are they different framings of the same underlying mechanism that could coexist?',
    'Logical analysis: map the core premises of each reading to check whether they logically exclude each other or merely emphasize different causal pathways. Test whether a single framework could accommodate all three (e.g., ''printing was necessary but not sufficient; reformers were intentional but not omnipotent; both co-evolved''—is this a middle ground or three incommensurable claims?).',
    'If the readings are genuinely incommensurable (one rules out the others), the co_constitution reading forecloses the determinism reading. If they are compatible (all three true under different descriptions of the same phenomenon), they coexist_with each other. If co-constitution creates structural pressure on the others without ruling them out, it influences them. This determines the reading_relations topology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'The logical relationship between sibling readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1440, technology_reformation_causality__co_constitution_reading, theater_ratio, 1440, 0.05).
narrative_ontology:measurement_basis(tech_tr_t1440, observed).
narrative_ontology:measurement(tech_tr_t1470, technology_reformation_causality__co_constitution_reading, theater_ratio, 1470, 0.08).
narrative_ontology:measurement_basis(tech_tr_t1470, observed).
narrative_ontology:measurement(tech_tr_t1490, technology_reformation_causality__co_constitution_reading, theater_ratio, 1490, 0.12).
narrative_ontology:measurement_basis(tech_tr_t1490, observed).
narrative_ontology:measurement(tech_tr_t1510, technology_reformation_causality__co_constitution_reading, theater_ratio, 1510, 0.16).
narrative_ontology:measurement_basis(tech_tr_t1510, observed).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__co_constitution_reading, theater_ratio, 1530, 0.18).
narrative_ontology:measurement_basis(tech_tr_t1530, observed).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__co_constitution_reading, theater_ratio, 1550, 0.18).
narrative_ontology:measurement_basis(tech_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t1440, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1440, 0.12).
narrative_ontology:measurement_basis(tech_be_t1440, observed).
narrative_ontology:measurement(tech_be_t1470, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1470, 0.22).
narrative_ontology:measurement_basis(tech_be_t1470, observed).
narrative_ontology:measurement(tech_be_t1490, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1490, 0.32).
narrative_ontology:measurement_basis(tech_be_t1490, observed).
narrative_ontology:measurement(tech_be_t1510, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1510, 0.38).
narrative_ontology:measurement_basis(tech_be_t1510, observed).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1530, 0.38).
narrative_ontology:measurement_basis(tech_be_t1530, observed).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1550, 0.38).
narrative_ontology:measurement_basis(tech_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1440, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1440, 0.05).
narrative_ontology:measurement_basis(tech_su_t1440, observed).
narrative_ontology:measurement(tech_su_t1470, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1470, 0.12).
narrative_ontology:measurement_basis(tech_su_t1470, observed).
narrative_ontology:measurement(tech_su_t1490, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1490, 0.18).
narrative_ontology:measurement_basis(tech_su_t1490, observed).
narrative_ontology:measurement(tech_su_t1510, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1510, 0.22).
narrative_ontology:measurement_basis(tech_su_t1510, observed).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1530, 0.22).
narrative_ontology:measurement_basis(tech_su_t1530, observed).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__co_constitution_reading, suppression_requirement, 1550, 0.22).
narrative_ontology:measurement_basis(tech_su_t1550, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__co_constitution_reading, 0.18).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, printing_press_as_distributed_technology).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, reformation_theology_and_agency).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, ecclesiastical_authority_over_scripture).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel technology_reformation_causality. The technological_determinism_reading treats printing as a mountain (exogenous cause); the beneficiary_agency_reading treats it as a rope (reformer tool, no extraction); the co_constitution_reading (this story) treats it as a tangled_rope (coordinating + asymmetrically extractive). All three readings are linked via network.affects_constraints to enable cross-reading analysis of causality and contingency. The three readings decompose a single natural-language kernel ('what caused the Reformation?') into three ε-invariant constraints with different structural and metric profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, institutional, 0.85).
constraint_indexing:directionality_override(technology_reformation_causality__co_constitution_reading, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
