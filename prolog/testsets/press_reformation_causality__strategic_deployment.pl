% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Deployment of Print Technology Against Church Authority
 *   domain: history/technology/religious
 *
 * SUMMARY:
 *   Between 1450 and 1550, reformers and printers in Northern Europe
 *   strategically coordinated the deployment of printing technology to break
 *   the Catholic Church's monopoly on scriptural interpretation and religious
 *   authority. This constraint is NOT the passive diffusion of a neutral
 *   technology; it is an active, coordinated WEAPONIZATION: reformers
 *   selected which texts to print, in which languages and editions, with
 *   which polemical apparatus; printers positioned themselves to profit from
 *   the fastest-moving, most controversial content; both groups adapted
 *   continuously to Church suppression attempts. The constraint extracted
 *   epistemic authority and institutional legitimacy from the Church and
 *   redistributed it to reform networks and the printing industry. It was
 *   defended by the reformer-printer alliance through active suppression of
 *   alternatives (Catholic printing, rival reform movements, institutional
 *   counter-action) — not by appeal to natural law or neutral technology, but
 *   through competitive advantage, economic incentive alignment, and
 *   strategic coordination. This reading instantiates ONE interpretation of
 *   the contested 'press_reformation_causality' kernel: the
 *   strategic_deployment reading, which emphasizes intentional human agency
 *   and coordinated action over technological determinism or
 *   co-constitutional feedback. The sibling readings
 *   (technological_determinism, co_constitution) offer different causal
 *   framings; this one asserts that reformers and printers CHOSE how to
 *   deploy print, and that choice was the causal driver.
 *
 * KEY AGENTS:
 *   - reformer_leadership: Organized networks of Protestant theological and institutional leaders (Luther, Calvin, Zwingli, their allies) who deliberately strategized the use of printing to disseminate vernacular scripture and doctrinal critiques; beneficiary of the constraint through access to mass distribution and monopoly-breaking, and agenda-setter through choice of texts and arguments.
 *   - printer_operators: Printers in reform-aligned cities who profited from the market for controversial, fast-moving religious texts and made editorial choices about what to publish; dual beneficiary and agenda-setter through competitive positioning and market adaptation.
 *   - church_institutional_authority: The Roman Catholic Church's institutional apparatus, facing erosion of its monopoly on scriptural interpretation and theological authority through coordinated textual attack; victim bearing suppression costs and losing enforcement capacity.
 *   - literate_lay_audiences: Educated merchants, scholars, minor nobility, and clergy who gained access to vernacular scripture and alternative theological frameworks; beneficiaries of the constraint's monopoly-breaking but dependent on the reformer-printer alliance's publication decisions.
 *   - illiterate_majority: Excluded from the textual economy entirely; experienced the Reformation as institutional disruption imposed through oral preaching, visual polemic, and religious war, not as access to the printed texts.
 *   - rival_printer_networks: Printers in Catholic-aligned or neutral cities displaced by the reformer-printer alliance's market capture and suppressed by both Church and reform authorities; victims of the constraint's competitive advantage.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.68).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.72).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, snare).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Deployment of Print Technology Against Church Authority").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history/technology/religious").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'c99659c7-a200-4866-a059-7bf288930cc3').
narrative_ontology:cs_kernel_codification('c99659c7-a200-4866-a059-7bf288930cc3', fixed_text).
narrative_ontology:cs_authority_grounding('c99659c7-a200-4866-a059-7bf288930cc3', lineage).
narrative_ontology:cs_interpretation_layer_present('c99659c7-a200-4866-a059-7bf288930cc3').
narrative_ontology:cs_reading_relation('c99659c7-a200-4866-a059-7bf288930cc3', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('c99659c7-a200-4866-a059-7bf288930cc3', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('c99659c7-a200-4866-a059-7bf288930cc3', foundational, human_agency_determines_technological_deployment).
narrative_ontology:cs_axiom_status(human_agency_determines_technological_deployment, holdable).
narrative_ontology:cs_axiom_grounding('c99659c7-a200-4866-a059-7bf288930cc3', human_agency_determines_technological_deployment, empirically_contingent).
narrative_ontology:cs_axiom('c99659c7-a200-4866-a059-7bf288930cc3', secondary, strategic_alignment_extraction_mechanism).
narrative_ontology:cs_axiom_status(strategic_alignment_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c99659c7-a200-4866-a059-7bf288930cc3', strategic_alignment_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('c99659c7-a200-4866-a059-7bf288930cc3', dispersed_reform_movements_without_print_coordination).
narrative_ontology:cs_drift_state('c99659c7-a200-4866-a059-7bf288930cc3', consolidation_of_reformer_printer_alliance_1520_1550, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c99659c7-a200-4866-a059-7bf288930cc3', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reformer_leadership).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printer_operators).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, church_institutional_authority).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_orthodox_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, literate_lay_audiences).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, competing_printer_networks).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, human_agency_technology_shaping).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, economic_interest_religious_change).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Protestant reformers (Luther, Calvin, Zwingli, their circles) actively sought printing technology as a weapon to disseminate vernacular scripture, doctrinal attacks on Church authority, and competing theological claims. They understood print's capacity to multiply texts, bypass clerical gatekeeping, and reach literate lay audiences. They made deliberate choices about which texts to print, in which languages, with which polemical apparatus, and distributed them strategically into networks of sympathetic merchants, nobles, and university students. The constraint's persistence depended on their sustained deployment strategy and the constant reinvention of arguments against counter-reformation suppression.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformer_leadership, agenda_setter,
    organized, generational, constrained, continental).

% Printers in Basel, Strasbourg, Geneva, Wittenberg, and other Protestant-aligned cities profited directly from the reform controversy: they printed the fastest-moving, most controversial texts, built market advantage by competing on speed and accessibility, and positioned themselves as indispensable to the dissemination of reformist thought. Some printers were personally ideologically aligned; others were opportunistic. Collectively, they made editorial and production choices that amplified certain arguments over others and created feedback loops: controversial editions sold faster, incentivizing more provocative content, which drew Church suppression attempts, which made the printing capacity itself politically strategically valuable.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printer_operators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, printer_operators, agenda_setter).

% The institutional Roman Catholic Church faced erosion of its monopoly on scriptural interpretation, ecclesiastical authority, and spiritual legitimacy through the cascade of printed challenges. It attempted counter-suppression (banning books, threatening printers, offering inducements to silence), but the technology's multiplication capacity meant that suppressing one edition merely created market demand for the next. The Church bore the costs of defending orthodoxy against coordinated, continuous textual attack and lost institutional coherence as local authorities faced local printing networks with local economic and factional interests in reform.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, church_institutional_authority, payer,
    institutional, civilizational, trapped, universal).

% The interpretive monopoly itself — the condition that only authorized Church authorities could authoritatively speak on scripture and doctrine — was systematically dismantled by the weaponized deployment of printing. Once the constraint (the monopoly enforcement) was breached, alternative readings became thinkable and defendable. The extraction here is structural: the monopoly extracted obedience and conformity; its breakdown extracted legitimacy and coherence from the institution.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_orthodox_monopoly, payer,
    institutional, civilizational, trapped, universal).

% Educated merchants, artisans, minor nobles, and university-educated clergy gained access to vernacular scripture, doctrinal alternatives, and intellectual weapons to contest ecclesiastical authority. This access was real and unprecedented. The constraint enabled them — but also strategically shaped what they could read, in what order, with what rhetorical framing. They benefited from the dismantling of the monopoly but remained dependent on the reformer-printer alliance's decisions about what texts to produce and circulate.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, literate_lay_audiences, beneficiary,
    organized, generational, constrained, regional).

% The vast illiterate majority could not read the proliferating texts. They experienced the Reformation as oral sermons, visual polemic (woodcuts, iconoclasm), and violent institutional struggle, mediated through literate intermediaries — priests, magistrates, landlords — whose authority continued to be gatekeepers. The printing revolution for them was invisible until it materialized as religious war and institutional reorganization imposed from above.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, peasant_and_illiterate_majority, excluded,
    powerless, biographical, trapped, local).

% Printers not aligned with reform movements were constrained by the market capture of the most lucrative and fastest-moving content. They either adapted to market demand for reform texts, exited the high-value segment, or faced regional suppression from authorities who controlled their operating licenses. The weaponization of print by the reformer-printer alliance extracted value from the wider printing industry by concentrating the highest-demand market.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, competing_printer_networks, payer,
    organized, biographical, constrained, regional).

% Examines the causal role of strategic human agency in selecting, deploying, and defending the use of printing technology. Reads sources for evidence of deliberate tactical choices by reformers and printers, competitive positioning, rhetorical strategy, and economic incentive-response cycles. This reading emphasizes the intentionality and coordination embedded in the constraint.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, historical_analyst, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, reformer_leadership).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates reformist ideological production and distribution: it solves the problem of how decentralized theological critique can be mass-produced, circulated rapidly, and defended against institutional suppression. It creates a feedback loop: controversial arguments generate market demand for printing; printing amplifies controversy; amplified controversy increases the value of the printing capacity itself. Reformers and printers aligned their incentives (ideological and economic) to solve this collective-action problem.
% TRANSFER_FUNCTION: Moves epistemic authority and institutional legitimacy from the Catholic Church's monopoly gatekeepers to a distributed coalition of reformers, printers, and literate lay readers. Also moves economic value (profits, market share) to printers and printer-aligned reformer networks. The transfer is enabled by the strategic weaponization of print: reformers choose which arguments to print; printers choose which texts to distribute; both benefit from the Church's loss of monopoly enforcement capacity.
% ABSENT_VOICES: The illiterate majority has no voice in this constraint's design or operation — they experienced it as institutional disruption imposed from above, not as access granted. Rival printer networks and Catholic printer shops experienced market displacement and suppression. The Vatican's printing operations and Counter-Reformation printers attempted to compete but faced the reformer-printer alliance's coordinated first-mover advantage and ideological momentum. Their suppression is not incidental — it is structural to the constraint's persistence.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of print by reformers and printers had not occurred — if printing technology had remained neutral in religious conflict or aligned primarily with Catholic institutional control — the Reformation would have unfolded entirely through slower manuscript channels, oral preaching, and institutional politics. The religious map of Europe would be substantially different: either the Reformation would have fragmented into regional, non-coordinated reform movements without transnational doctrinal coherence, or it would have been suppressed by concerted institutional counter-action. The constraint is world-shaping: it enabled the specific form the Reformation took (rapid, textually coordinated, ideologically decentralized, transnational).
% FOUNDING_PROBLEM: How can dispersed reform movements lacking institutional authority coordinate their theological critiques, defend against Church suppression, reach audiences beyond the reach of oral preaching, and establish alternative interpretive frameworks that persist across generations and regions? The printing press solved this coordination problem by multiplying texts, making suppression costly, enabling rapid response and adaptation, and creating economic incentives for independent operators to invest in dissemination.
% FOUNDING_PROBLEM_CORROBORATION: Reformer writings (Luther's justifications for printing strategy, Calvin's correspondence about text distribution, Zwingli's polemical program) and printer records (publication patterns, business correspondence, evidence of deliberate choices about which manuscripts to set in type) attest that the founding problem was real and that the printing strategy was deliberately designed to solve it. Historians of the Reformation (Pettegree, Eisenstein in their structural analysis modes, not their determinist claims) document the strategic coordination between reformer networks and printing hubs. Catholic institutional complaints about printed heresy and counter-reformation printing investments confirm the weaponization was recognized and actively resisted. No corroboration comes from the illiterate majority or displaced Catholic printers — they experienced the constraint as imposed change, not as the solution to a problem they had named.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).

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
 *   Extractiveness rises sharply from 1450 (0.08, pre-reformation baseline) to 1550 (0.68), tracking the constraint's consolidation: as printing became entrenched as the dominant medium for religious polemic and as the reformer-printer alliance's coordination deepened, the extraction of Church authority accelerated. By 1530, the Church had lost substantial control over theological discourse — the extraction was near-complete. Suppression_requirement tracks enforcement intensity: it rises steeply as the Church attempted counter-suppression (banning, prosecution, rival printing) and as the reformer-printer alliance defended its dominant position against Catholic and rival reform competitors. Theater_ratio (0.41 at end) reflects the constraint's character: the coordination story is genuine (the reformers did solve a real collective-action problem, the printers did operate under market incentives), but an increasing share of suppression activity defends the alliance's competitive advantage and political dominance rather than genuine security or theological truth. The measurements share a single time grid (1450, 1470, 1490, 1510, 1530, 1550), so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The reformer-leadership and printer-operator seats should compute as snare beneficiaries (low d, high χ subsidy), while the church_institutional_authority seat computes as victim (high d, high χ extraction). The literate_lay_audiences sit near symmetric (genuine benefit from monopoly-breaking, but dependent on alliance's publication choices). The illiterate_majority and rival_printers sit as collateral victims (trapped, constrained exit, bearing diffuse costs of the alliance's dominance). The analytical observer seat measures the coordination and weaponization as distinct phenomena: real collective-action problem solved (coordination function), but the solution designed to extract and consolidate power (snare architecture).
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers benefit directly from the constraint: they gain distribution reach, market advantage, and institutional power previously monopolized by the Church. Their exit options are constrained but not destroyed — they are organized and powerful enough to negotiate with secular authorities, form regional coalitions, and adapt to suppression. Directionality for this coalition is low-to-moderate (beneficiary range, d ~0.15-0.35). The Church institutional authority bears extraction costs (lost monopoly, required suppression expenditure, coherence erosion) and is trapped (the civilizational time horizon and universal scope mean exit is unthinkable — the Church's institutional identity is bound to defending orthodoxy). Directionality for the Church is high (victim range, d ~0.75-0.95). Literate lay audiences benefit from access but remain dependent on the alliance's choices (constrained exit, moderate power). Directionality moderate (d ~0.45-0.55). Illiterate majority and rival printers face suppression and displacement without meaningful choice (trapped, powerless-to-moderate). Directionality high (d ~0.75-0.95). The per-seat divergence is large and structurally rooted in differential power, exit options, and positioning relative to the monopoly-breaking mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — enabling coordinated theological critique against institutional monopoly — remains live throughout the interval: the reformers keep producing arguments, the printers keep producing editions, the Church keeps attempting suppression, and the alliance keeps adapting. There is no mandatrophy signal from the founding_problem_status. However, the theater_ratio's rise (0.05 to 0.41) suggests that as the constraint matures, an increasing proportion of its enforcement machinery defends competitive advantage and political dominance rather than the original coordination function. This is Goodhart drift: the success of the first wave (breaking the monopoly) creates new coordination problems (managing fragmented reform movements, defending against counter-reformation, preventing backsliding). The suppression machinery that once targeted heresy now targets alternative reform movements, Catholic printers, and rival doctrinal schools. The constraint persists, but the functional problem it addresses has partly shifted from 'how do we coordinate critique' to 'how do we maintain dominance.' The classification remains snare (extraction is primary, coordination is instrumental to extraction), but the theater component reveals the drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_emergence,
    'Did reformers and printers deliberately coordinate a strategy to deploy printing as a weapon against Church authority, or did the weaponization emerge from independent self-interested actions (each actor pursuing profit/doctrine) that coincidentally aligned?',
    'Textual analysis of reformer correspondence, printer business records, and polemical writings for explicit strategic language, planning, communication of intent. Cross-reference with economic data on printer market positioning, book production patterns, and distribution networks to distinguish coordinated strategy from emergent alignment.',
    'If coordination was deliberate, the constraint is a designed snare (intentional extraction wrapped in coordination framing). If emergent, the classification shifts toward rope or tangled_rope (actors solving problems they faced independently, not architecting an extraction mechanism). The measurement of ''intentionality'' is the causal driver of type classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_emergence, empirical, 'Whether the strategic deployment was coordinated by design or emerged from independent incentive alignment.').

omega_variable(
    technological_autonomy_boundary,
    'Could the printing technology itself have shaped the Reformation''s course independent of human strategic choices? Put differently: if reformers and printers had made different choices (printed different texts, slower, in Latin only), would the Reformation have unfolded differently?',
    'Counterfactual analysis grounded in comparative history: examine regions and time periods where printing was available but not strategically deployed (Catholic printing, non-Reformation printing), and assess whether religious change patterns differ. Look for ''technological ceiling'' evidence (would more printing have changed outcomes further, or would other factors have capped change?).',
    'If the technology''s properties alone would have driven Reformation success (high ceiling, low actor-dependence), the classification shifts toward technological_determinism and away from strategic_deployment. If actor choices were the binding constraint on outcomes (technology only enabled what actors chose to do), the snare classification holds. If technology and choices co-determine outcomes (neither alone sufficient), co_constitution gains ground.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_autonomy_boundary, conceptual, 'The boundary between technological affordances and strategic human choice in determining outcomes.').

omega_variable(
    economic_vs_ideological_motive,
    'Did reformers deploy printing primarily for ideological/theological reasons (breaking Church monopoly on doctrine) or economic reasons (enriching printers, disrupting Catholic institutional rents)? Or are these inseparable?',
    'Distinguish ideological writings (reformer polemics against Church authority) from economic evidence (printer investment, market expansion, profit-seeking). Test whether printers who lacked ideological commitment to reform still participated if profits were available, and whether reformers prioritized texts with low commercial appeal if they had high theological importance.',
    'If primarily ideological, the constraint''s beneficiary is doctrinal; if primarily economic, the beneficiary is the printer-capitalist class. If inseparable (economic and ideological incentives aligned such that profitable texts were also theologically strategic), the constraint operates as designed snare (extracting both Church authority and monopoly rent through a single mechanism). Disentangling motives clarifies whose interests the constraint primarily serves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_vs_ideological_motive, empirical, 'Whether the strategic deployment was driven primarily by ideological or economic interest.').

omega_variable(
    suppression_as_structural_feature,
    'Is the active suppression of Catholic printing, rival reformers, and illiterate-majority access a structural feature of the constraint necessary to maintain its extractive function, or a contingent defense response to resistance?',
    'Compare scenarios where suppression was relaxed (certain regions, certain time periods) with scenarios where it was tight. Assess whether the constraint''s coordination function (enabling reformist theological production) requires suppression, or whether suppression is only necessary to maintain monopoly advantage. Test with counterfactual: if all parties had equal printing access, would the coordination problem be solved or simply distributed?',
    'If suppression is structural and necessary, the constraint is a designed snare with extraction built in. If suppression is contingent (could be removed without breaking coordination), the classification might shift toward tangled_rope or rope-with-abuses. This omega addresses whether the snare classification depends on the measured suppression or on extractiveness alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_as_structural_feature, empirical, 'Whether suppression is built into the constraint or a contingent response to resistance.').

omega_variable(
    reading_kernel_contest,
    'Is this reading (strategic_deployment) the correct causal framing for the press-Reformation relationship, or does one of the sibling readings (technological_determinism, co_constitution) better capture the historical dynamics?',
    'Examine the corpus of Reformation historiography for which framing dominates in specialist literature. Test predictions generated by each reading against empirical evidence: does strategic_deployment predict that changing actor choices would change outcomes (should be verifiable); does technological_determinism predict that availability alone drives success (should be falsified by examples of available-but-not-used printing); does co_constitution predict feedback loops strengthening over time (testable from publication patterns and institutional response cycles).',
    'If technological_determinism is correct, this constraint mis-classifies: press technology should be mountain or rope (natural law or neutral coordination), not snare. If co_constitution is correct, the snare classification should be weakened toward tangled_rope (neither extraction nor coordination fully determines outcomes; both are co-produced). If strategic_deployment holds, snare classification is correct and the reading should be preferred in future corpus work. This is a preference omega addressing the committer-frame contest itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_contest, preference, 'Which reading of the press-Reformation causality kernel is most defensible given available evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__strategic_deployment, theater_ratio, 1450, 0.05).
narrative_ontology:measurement_basis(pres_tr_t1450, observed).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causality__strategic_deployment, theater_ratio, 1470, 0.12).
narrative_ontology:measurement_basis(pres_tr_t1470, observed).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causality__strategic_deployment, theater_ratio, 1490, 0.24).
narrative_ontology:measurement_basis(pres_tr_t1490, observed).
narrative_ontology:measurement(pres_tr_t1510, press_reformation_causality__strategic_deployment, theater_ratio, 1510, 0.32).
narrative_ontology:measurement_basis(pres_tr_t1510, observed).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causality__strategic_deployment, theater_ratio, 1530, 0.38).
narrative_ontology:measurement_basis(pres_tr_t1530, observed).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causality__strategic_deployment, theater_ratio, 1550, 0.41).
narrative_ontology:measurement_basis(pres_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__strategic_deployment, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement_basis(pres_be_t1450, observed).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causality__strategic_deployment, base_extractiveness, 1470, 0.22).
narrative_ontology:measurement_basis(pres_be_t1470, observed).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causality__strategic_deployment, base_extractiveness, 1490, 0.45).
narrative_ontology:measurement_basis(pres_be_t1490, observed).
narrative_ontology:measurement(pres_be_t1510, press_reformation_causality__strategic_deployment, base_extractiveness, 1510, 0.58).
narrative_ontology:measurement_basis(pres_be_t1510, observed).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causality__strategic_deployment, base_extractiveness, 1530, 0.65).
narrative_ontology:measurement_basis(pres_be_t1530, observed).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causality__strategic_deployment, base_extractiveness, 1550, 0.68).
narrative_ontology:measurement_basis(pres_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__strategic_deployment, suppression_requirement, 1450, 0.15).
narrative_ontology:measurement_basis(pres_su_t1450, observed).
narrative_ontology:measurement(pres_su_t1470, press_reformation_causality__strategic_deployment, suppression_requirement, 1470, 0.32).
narrative_ontology:measurement_basis(pres_su_t1470, observed).
narrative_ontology:measurement(pres_su_t1490, press_reformation_causality__strategic_deployment, suppression_requirement, 1490, 0.48).
narrative_ontology:measurement_basis(pres_su_t1490, observed).
narrative_ontology:measurement(pres_su_t1510, press_reformation_causality__strategic_deployment, suppression_requirement, 1510, 0.62).
narrative_ontology:measurement_basis(pres_su_t1510, observed).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causality__strategic_deployment, suppression_requirement, 1530, 0.68).
narrative_ontology:measurement_basis(pres_su_t1530, observed).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causality__strategic_deployment, suppression_requirement, 1550, 0.72).
narrative_ontology:measurement_basis(pres_su_t1550, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, identity_coordination).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__strategic_deployment, 0.12).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% The press_reformation_causality kernel decomposes into three distinct constraints, each instantiating a different causal reading of the printing press's role in the Reformation. Strategic_deployment (this story) emphasizes intentional human agency and coordinated deployment; technological_determinism isolates printing as an autonomous enabling technology; co_constitution frames technology and agency as mutually co-producing outcomes. The three readings share the same empirical domain (1450-1550 Northern Europe, printing-enabled religious change) but diverge fundamentally on causal mechanism. The ε values differ: strategic_deployment has substantial extractiveness (0.68 terminal) because the deployment was deliberate extraction of Church authority; technological_determinism has lower extractiveness because the press is treated as natural tool, not designed snare; co_constitution sits between, with shared responsibility. Each story carries its own classification. Strategic_deployment forecloses technological_determinism's core premise (autonomous technology) but coexists_with co_constitution's feedback mechanisms (which operate within a strategic framework). Network edges link all three; downstream consumers must engage all three readings to comprehend the kernel's full contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
