% ============================================================================
% CONSTRAINT STORY: printing_press_diffusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_printing_press_diffusion, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: printing_press_diffusion
 *   human_readable: Printing Press Diffusion and Religious Authority Restructuring (1450-1550)
 *   domain: historical_sociology/religious_studies/political_economy
 *
 * SUMMARY:
 *   The diffusion of printing press technology across Europe (1450-1500)
 *   simultaneously transformed religious authority, labor relations,
 *   information access, and state capacity. This constraint exhibits
 *   overdetermination: multiple independent causal chains (technological
 *   diffusion, economic disruption, doctrinal innovation, territorial
 *   consolidation) converge without reducible common substrate. The
 *   analytical problem: is this a single kernel with multiple readings
 *   (religious authority restructured by technology), or multiple constraints
 *   that happen to activate simultaneously? The Reformation conventionally
 *   bundles these as a single historical event, but structural analysis
 *   suggests decomposition into at least three distinct constraints with
 *   different ε values: (1) printing_press_diffusion (technological/economic
 *   extraction, this story), (2) protestant_doctrinal_innovation
 *   (intellectual contestation), (3) church_authority_restructuring
 *   (institutional renegotiation). The printing press itself is not
 *   inherently religious — it functions as technological constraint enabling
 *   rapid text distribution, which then carries whatever doctrinal content is
 *   chosen for printing. The Reformation's success depended on printers'
 *   economic interest in vernacular religious texts, not on the technology
 *   endorsing any theology. This story focuses on the printing press
 *   constraint: who benefits from reproduction monopoly, who bears costs of
 *   scribal displacement, what suppression mechanisms operate, and how the
 *   distribution of printed texts reshapes authority structures. Theater
 *   ratio declines sharply over the 50-year interval as printing becomes
 *   decentralized and normalized — the performative element (exotic
 *   technology, miraculous reproduction) is replaced by routine production.
 *   Base extractiveness rises then plateaus as the monopoly phase (high
 *   extraction, 0.42) gives way to competitive printing (extraction
 *   stabilizes at lower-cost equilibrium, 0.51). This trajectory suggests
 *   neither pure extraction (snare) nor pure coordination (rope) but a hybrid
 *   that begins extractive, becomes coordinative as diffusion proceeds.
 *
 * KEY AGENTS:
 *   - Printer Merchants: Primary beneficiary (institutional/arbitrage) — capture monopoly rents on technical knowledge, capital access, and reproduction rights during diffusion phase
 *   - Scribal Copyists: Primary victim (powerless/trapped) — labor class displaced without alternative; regional copying markets collapse within 20-30 years
 *   - Institutional Church: Secondary victim and attempted suppressor (institutional/arbitrage) — loses monopoly on knowledge distribution; attempts censorship and book burning; paradoxically benefits from printing its own texts
 *   - Parish Priests: Moderately affected (moderate/constrained) — benefit from standardized liturgical texts; constrained by inability to control doctrinal competition
 *   - Reformation Movements: Organized beneficiary (organized/constrained) — gain massive distribution advantage for vernacular theology; remain constrained by censorship and Church opposition
 *   - Territorial Princes: Powerful temporary users (powerful/mobile) — use printing for decree standardization and loyalty propaganda; lose exclusive advantage as printing decentralizes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional responses (Church censorship, monopoly preservation) as inevitable technological limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(printing_press_diffusion, 0.52).
domain_priors:suppression_score(printing_press_diffusion, 0.48).
domain_priors:theater_ratio(printing_press_diffusion, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(printing_press_diffusion, extractiveness, 0.52).
narrative_ontology:constraint_metric(printing_press_diffusion, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(printing_press_diffusion, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(printing_press_diffusion, tangled_rope).
narrative_ontology:human_readable(printing_press_diffusion, "Printing Press Diffusion and Religious Authority Restructuring (1450-1550)").
narrative_ontology:topic_domain(printing_press_diffusion, "historical_sociology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(printing_press_diffusion).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(printing_press_diffusion, distributed).
narrative_ontology:cs_authority_grounding(printing_press_diffusion, lineage).
narrative_ontology:cs_reading_relation(printing_press_diffusion, printing_press_diffusion_institutional_church_reading, coexists_with).
narrative_ontology:cs_reading_relation(printing_press_diffusion, printing_press_diffusion_reformation_reading, coexists_with).
narrative_ontology:cs_axiom(printing_press_diffusion, foundational, knowledge_authority_decentralizable).
narrative_ontology:cs_axiom_status(knowledge_authority_decentralizable, holdable).
narrative_ontology:cs_axiom(printing_press_diffusion, foundational, labor_value_mechanical_reproducible).
narrative_ontology:cs_axiom_status(labor_value_mechanical_reproducible, holdable).
narrative_ontology:cs_reference_frame(printing_press_diffusion, manuscript_monopoly_authority).
narrative_ontology:cs_drift_state(printing_press_diffusion, post_printing_saturation_1550, gap(authority_erosion, severe, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(printing_press_diffusion, printer_merchants).
narrative_ontology:constraint_beneficiary(printing_press_diffusion, protestant_reformation_movements).
narrative_ontology:constraint_beneficiary(printing_press_diffusion, vernacular_literacy_demand).
narrative_ontology:constraint_victim(printing_press_diffusion, institutional_church_monopoly).
narrative_ontology:constraint_victim(printing_press_diffusion, scribal_labor_class).
narrative_ontology:constraint_victim(printing_press_diffusion, manuscript_copying_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SCRIBAL COPYIST (SNARE) — Trapped in a labor class whose economic basis is systematically undermined by mechanical reproduction. No alternative career pathway; regional market for manuscript copying collapses within a generation. Suppression is economic desperation. Extraction flows away entirely — the copyist bears costs while coordinating with nothing.
constraint_indexing:constraint_classification(printing_press_diffusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PARISH PRIEST (TANGLED ROPE) — Genuine coordination function: printed liturgical texts standardize practice, reduce copying errors, enable distribution of reformed doctrine. But also constrained by suppression — the priest cannot control the flood of vernacular religious texts that bypass ecclesiastical authority. Mixed: benefits from standardized texts, pays costs of doctrinal competition. Suppression comes from inability to control interpretation.
constraint_indexing:constraint_classification(printing_press_diffusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRINTER MERCHANT (ROPE) — Net beneficiary. Printing technology enables coordination of book production and distribution. Early printers exploit information arbitrage: control of technical knowledge, exclusive access to capital, monopoly on reproduction. But the constraint is fundamentally coordinative — without the printer, no distributed texts. Suppression minimal relative to extraction — the printer faces low barriers to exit and high profit margins.
constraint_indexing:constraint_classification(printing_press_diffusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: REFORMATION MOVEMENT (TANGLED ROPE) — Organized actors with constrained options. The printing press enables coordination of doctrinal messaging, pamphlet distribution, and rapid response to institutional church positions. But also constrained by suppression: censorship, book burning, ecclesiastical bans. The reformation both benefits from printing's coordination function and bears the costs of attempted suppression. Genuine hybrid: coordination is essential to the movement's success; extraction occurs through book prices and control of which texts get printed.
constraint_indexing:constraint_classification(printing_press_diffusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: INSTITUTIONAL CHURCH / NATURAL LAW VIEW (MOUNTAIN) — From the Church's perspective seeking to naturalize its position, the printing press presents as a force of nature — an inexorable technological shift that no authority could control. Communication speed increases inevitably; diffusion of texts follows natural laws of information spread. The Church's authority over knowledge distribution appears as an immutable structural property now breached. This mountain classification is a false summit: the Church benefits from printing for its own doctrine yet claims the technology is beyond control. The beneficiary is naturalizing a constraint it partially controls.
constraint_indexing:constraint_classification(printing_press_diffusion, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: TERRITORIAL PRINCE (SCAFFOLD) — Powerful agents using the printing press as a temporary support structure for state consolidation. Printed decrees standardize law across territory; printed vernacular texts build national identity; printed loyalty propaganda legitimates rule. The scaffold has a sunset: once the print infrastructure is built and literacy spreads, the prince's monopoly on printing (temporary advantage) dissolves. This perspective sees extractiveness as declining over the biographical horizon as printing becomes decentralized and competition increases.
constraint_indexing:constraint_classification(printing_press_diffusion, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(printing_press_diffusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(printing_press_diffusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(printing_press_diffusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(printing_press_diffusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(printing_press_diffusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. Early printing is extractive — printers hold monopoly on technical knowledge, require massive capital investment (printing press costs equivalent to annual income for skilled labor), and control supply of reproduced texts. Scribal copyists bear full cost of displacement. But extractiveness is not extreme (snare level >0.66) because the technology generates genuine value — texts become cheaper, faster to produce, more standardized, and more widely distributed than manuscripts could achieve. The extraction is not pure rent-seeking but partly returns to innovation risk. Measured at peak extraction (t=15 years, value 0.42 → midway point in interval), then stabilizing as competition increases and technology spreads. Suppression (0.48): Moderate. Multiple suppression mechanisms: (1) Capital barriers — printing press cost restricts entry; (2) Guild monopolies — printers' guilds limit competition; (3) Censorship — Church and state attempts to restrict what gets printed; (4) Literacy barriers — texts benefit readers but constrain benefits to illiterate majority. But suppression is not total — printers successfully circumvent censorship through mobility and clandestine production; alternative literacy pathways exist; guild barriers erode. Theater ratio (0.35 at end, declining from 0.55): Initially high because printing presents as miraculous, technical expertise appears essential, mystique surrounds the technology. As printing normalizes and competition increases, performative element declines — printing becomes routine labor, not exotic craft. The low theater at end reflects that printing is now recognized as a straightforward mechanical process, not magical reproduction. Claimed type: Tangled Rope. The constraint exhibits both genuine coordination function (enabling standardized, distributed texts that solve real problems: quality control in liturgical texts, rapid doctrinal dissemination, national decree distribution) AND asymmetric extraction (printer monopoly rents, scribal displacement, capital concentration). Requires active enforcement (guild regulation, censorship, capital control) to maintain. Both beneficiary and victim classes identifiable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint generates maximum perspectival divergence. The printer merchant sees primarily coordination (Rope) — the technology solves the problem of book production and distribution. They experience themselves as low-extraction beneficiary. The scribal copyist sees pure extraction (Snare) — economic devastation with no benefit. The parish priest sees mixed coordination and constraint (Tangled Rope) — standardized texts are genuinely useful; loss of doctrinal control is genuinely harmful. The reformation movement sees constrained opportunity (Tangled Rope) — printing enables their success but censorship and costs constrain; both benefits and costs are significant. The territorial prince sees temporary support (Scaffold) — printing is valuable for consolidating state power through decree and loyalty propaganda, but the advantage erodes as printing decentralizes. The Church sees an inexorable force (Mountain) — the naturalizing perspective of a beneficiary-turned-victim trying to reframe loss of control as inevitable technological shift. The analytical observer recognizes the mountain as false summit — the Church's resistance to printing was a strategic choice, not a law of nature, and the Church's later adoption of printing for its own purposes reveals that the technology was malleable to institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position: beneficiary status with exit options (arbitrage vs constrained) produces low d (negative f(d), experienced as low/beneficial extraction); victim status with exit options (trapped vs mobile) produces high d (high f(d), experienced as severe extraction); mixed status produces mid-range d. Printer merchants: beneficiary + arbitrage → d ≈ 0.05-0.15, f(d) ≈ -0.12 to 0.02, experienced as essentially non-extractive or slightly extractive to the printer. Scribal copyists: victim + trapped → d ≈ 0.95, f(d) ≈ 1.42, maximum experienced extraction — the copyist has no exit and loses everything. Reformation movements: victim initially (suppressed by Church) + constrained (censorship barriers) → d ≈ 0.70, f(d) ≈ 1.10; moderate-high experienced extraction despite organizational capacity, because suppression is real. Territorial princes: ambiguous (beneficiary in immediate consolidation + mobile with exit options) → d ≈ 0.40-0.50, f(d) ≈ 0.40-0.65; experienced as moderate extraction as monopoly erodes. Parish priests: mixed (both benefit and constrained) + constrained exit → d ≈ 0.55, f(d) ≈ 0.75; moderate experienced extraction. Institutional Church: beneficiary (eventually adopts printing) + arbitrage (can choose to print or censor) → but constrained by suppression (cannot prevent others from printing) → d ≈ 0.45, f(d) ≈ 0.55; moderate experienced extraction despite strategic agency, because the core loss of monopoly is irreversible.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint avoids mandatrophy (classification ambiguity) by clarifying that all six types are defensible from different structural positions, and the perspectival gap IS the analytical content. The Tangled Rope classification at the analytical level is correct for the constraint as a whole because it exhibits both genuine coordination (solving book production problems) and asymmetric extraction (monopoly rents, capital concentration, labor displacement). The snare, rope, and mountain perspectives capture real structural experiences without error — they are not misclassifications but truthful positions. The mandatrophy would arise if we tried to compress these into a single 'true' type. The resolution is perspectival pluralism: the constraint instantiates all six types simultaneously from different positions, and the engine's job is to report the presheaf over the observation site, not to collapse it into a single canonical type. The Tangled Rope at the analytical/civilizational level is the meta-classification that holds together the multiple micro-classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_substrate_unity,
    'Does the Reformation instantiate a single kernel (religious authority) with multiple readings, or multiple independent causal chains (economic disruption, doctrinal innovation, technological diffusion) that happen to converge?',
    'Counterfactual analysis: remove printing press, what happens to Reformation? Remove doctrinal ferment, what happens to printing diffusion? Structural dependency tests between causal chains.',
    'If single kernel with readings: one constraint story with alternative perspectives. If independent chains: separate constraint stories (printing_press_diffusion, protestant_doctrinal_innovation, church_authority_restructuring) linked via network.affects_constraints. The ε values would differ sharply between readings/stories.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causal_substrate_unity, conceptual, 'Whether Reformation is kernel-readable or multi-causal convergence').

omega_variable(
    extraction_vs_creative_destruction,
    'Does the printing press function as extractive (printer captures surplus from disappearing scribal labor) or as creative destruction (new economic value in printing exceeds lost value in copying)?',
    'Quantitative analysis: total labor income in copying vs printing; total value of texts produced; regional economic data from transition zones. Adjustment for quality, error reduction, and access expansion benefits.',
    'If net extractive: snare or tangled_rope appropriate; base_extractiveness stays ~0.52. If net creative: reclassify as rope; base_extractiveness drops to ~0.30. If heterogeneous by region: decompose into regional stories with different ε values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_creative_destruction, empirical, 'Whether printing represents extraction or net creative value').

omega_variable(
    suppression_mechanism_internalization,
    'Does suppression of the printing press operate primarily through structural barriers (technical monopoly, capital requirements, guilds) or through internalized institutional authority (printers self-censoring to maintain Church favor)?',
    'Historical analysis of censorship efforts vs actual printing volumes; distribution of suppressors (state vs church vs guild); comparative suppression effectiveness across regions with different institutional structures.',
    'If structural: suppression is external; scenarios with alternative technology or decentralized capital would bypass it. If internalized: suppression persists even with technical alternatives because the authority claim remains intact. Suggests different exit options for key agents (trapped vs constrained vs identity_locked).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Suppression mechanism: structural vs internalized authority').

omega_variable(
    printing_necessity_for_reformation,
    'Was the printing press necessary for the Reformation to succeed, or merely accelerating? Could doctrinal reform have spread through manuscript networks and oral transmission, with printing as amplification rather than precondition?',
    'Comparative historical analysis: pre-printing doctrinal movements (Wycliffe, Hus); spread patterns of reform in regions with delayed printing access; counterfactual modeling of oral + manuscript diffusion rates.',
    'If necessary: printing press is a keystone constraint upstream of all reformation outcomes. If accelerating: printing is one causal factor among several with comparable weight. Affects whether printing_press_diffusion and protestant_reformation are structurally independent or causally coupled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printing_necessity_for_reformation, empirical, 'Whether printing was necessary or accelerating for Reformation success').

omega_variable(
    false_summit_church_authority,
    'Is the Institutional Church''s mountain perspective (printing as inexorable force of nature) a genuine natural law or a false summit naturalizing what is actually a negotiated institutional arrangement the Church partially controls?',
    'Analysis of Church response options: Did the Church genuinely have zero alternatives, or did it choose not to adopt printing for its own doctrine earlier? What would have happened if the Church had monopolized printing technology rather than resisting it?',
    'If genuine mountain: Church authority erosion is structural and irreversible. If false summit: Church beneficiaries are using ''natural law'' framing to escape accountability for institutional strategy choices. Affects whether mountain classification holds under scrutiny.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_church_authority, conceptual, 'Church perspective: genuine natural law or false summit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(printing_press_diffusion, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ppd_tr_t0, printing_press_diffusion, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ppd_tr_t15, printing_press_diffusion, theater_ratio, 15, 0.42).
narrative_ontology:measurement(ppd_tr_t30, printing_press_diffusion, theater_ratio, 30, 0.35).
narrative_ontology:measurement(ppd_tr_t50, printing_press_diffusion, theater_ratio, 50, 0.33).

% Extraction over time
narrative_ontology:measurement(ppd_be_t0, printing_press_diffusion, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(ppd_be_t15, printing_press_diffusion, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(ppd_be_t30, printing_press_diffusion, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(ppd_be_t50, printing_press_diffusion, base_extractiveness, 50, 0.51).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(printing_press_diffusion, resource_allocation).
narrative_ontology:affects_constraint(printing_press_diffusion, protestant_doctrinal_innovation).
narrative_ontology:affects_constraint(printing_press_diffusion, church_authority_restructuring).
narrative_ontology:affects_constraint(printing_press_diffusion, vernacular_literacy_expansion).

% DUAL FORMULATION NOTE:
% Printing press diffusion is one constraint in a family of three linked constraints that constitute the Reformation as a composite event. The printing press is upstream (enables the other two) but is not itself religious — it is a technological/economic constraint that carries any content loaded into it. The protestant_doctrinal_innovation constraint focuses on the intellectual contestation of Church doctrine independent of distribution mechanism. The church_authority_restructuring constraint focuses on the institutional renegotiation of authority claims independent of technology. These three constraints together form the overdetermined causal structure of the Reformation; each could be analyzed independently, and their intersection explains the historical pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(printing_press_diffusion, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
