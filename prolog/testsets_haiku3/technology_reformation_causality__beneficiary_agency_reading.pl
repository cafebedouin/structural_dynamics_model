% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformation Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates a beneficiary-agency reading of the
 *   contested kernel 'technology_reformation_causality.' The reading asserts
 *   that reformers and printers formed a strategic coalition to bypass Church
 *   authority over textual interpretation. Printing was the tool deployed
 *   toward that goal; it did not determine the Reformation but enabled
 *   reformers' deliberate strategy. The coalition extracted authority and
 *   economic value from the ecclesiastical hierarchy and manuscript networks.
 *   This reading foregrounds human agency and coalition formation; it
 *   contests the technological-determinism reading (which attributes causal
 *   inevitability to the press itself) and sits in tension with the
 *   co-constitution reading (which emphasizes mutual shaping rather than
 *   strategic deployment). The authored metrics (high extractiveness, high
 *   suppression, moderate theater) describe an actively maintained
 *   enforcement structure whose function has partly atrophied into
 *   justification — the measurement series shows rising theater_ratio as the
 *   enforcement became less about suppressing competing texts and more about
 *   defending intellectual property and market position.
 *
 * KEY AGENTS:
 *   - Reform-movement leadership (organized, agenda-setter): set strategy for using printing; identified printers as allies; calibrated texts for mass production
 *   - Independent printers (powerful, beneficiary/payer): chose to engage with reform texts; extracted profit; bore legal risk from censorship
 *   - Ecclesiastical authority (institutional, payer): lost monopoly on textual interpretation; bore suppression costs building censorship apparatus
 *   - Manuscript-scribal networks (moderate, payer): displaced by printing; lost income and status
 *   - Lay readership (powerless, beneficiary): gained access to vernacular scripture outside Church gatekeeping
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.62).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.71).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformation Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, 'f6c586e7-af04-4328-9219-a1c49330bf7c').
narrative_ontology:cs_kernel_codification('f6c586e7-af04-4328-9219-a1c49330bf7c', distributed).
narrative_ontology:cs_authority_grounding('f6c586e7-af04-4328-9219-a1c49330bf7c', distributed).
narrative_ontology:cs_reading_relation('f6c586e7-af04-4328-9219-a1c49330bf7c', technology_reformation_causality__technological_determinism_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6c586e7-af04-4328-9219-a1c49330bf7c', technology_reformation_causality__co_constitution_reading, influences).
narrative_ontology:cs_axiom('f6c586e7-af04-4328-9219-a1c49330bf7c', foundational, human_agency_determines_technology_deployment).
narrative_ontology:cs_axiom_status(human_agency_determines_technology_deployment, holdable).
narrative_ontology:cs_axiom_grounding('f6c586e7-af04-4328-9219-a1c49330bf7c', human_agency_determines_technology_deployment, deontological).
narrative_ontology:cs_axiom('f6c586e7-af04-4328-9219-a1c49330bf7c', foundational, strategic_coalition_formation_bypasses_incumbent_authority).
narrative_ontology:cs_axiom_status(strategic_coalition_formation_bypasses_incumbent_authority, holdable).
narrative_ontology:cs_axiom_grounding('f6c586e7-af04-4328-9219-a1c49330bf7c', strategic_coalition_formation_bypasses_incumbent_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('f6c586e7-af04-4328-9219-a1c49330bf7c', ecclesiastical_monopoly_on_textual_authority).
narrative_ontology:cs_drift_state('f6c586e7-af04-4328-9219-a1c49330bf7c', post_reformation_stabilization_1550, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f6c586e7-af04-4328-9219-a1c49330bf7c', '2026-06-13T14:32:18Z').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reform_movement_leadership).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, independent_printers).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, ecclesiastical_authority).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, manuscript_scribal_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, lay_readership).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, independent_printers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reformers (Luther, Calvin, Zwingli, their networks) identified printing as a strategic tool to distribute vernacular scripture and polemics beyond Church censorship. They actively sought out printers, funded editions, wrote texts calibrated for mass production, and built distribution networks. They set the agenda: what gets printed, in what language, for what audience. They collect the primary gain from bypassing ecclesiastical authority over textual interpretation.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, reform_movement_leadership, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, reform_movement_leadership, beneficiary).

% Printers (especially Protestant-sympathetic or politically positioned printers in Basel, Strasbourg, Geneva, Wittenberg) profit enormously from the volume and margin on reform texts. They extract value from the reformers' guaranteed demand and the political protection some cities offered. They also bear legal and reputational risk from Church interdiction and inquisitorial action, though this varies by jurisdiction and political jurisdiction. They are not passive technology operators; they choose which texts, which languages, which editions to produce based on market perception of demand and risk.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, independent_printers, beneficiary,
    powerful, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, independent_printers, payer).

% The Church (bishops, cardinals, papal authority, religious orders) held a monopoly on authorized textual interpretation through control of manuscript production, scribal networks, and doctrinal authority. Printing undermined that monopoly by enabling mass production of competing interpretations outside Church channels. The Church paid in lost authority over textual circulation and interpretive gatekeeping. It responded with censorship infrastructure (indexes, inquisitions, book-burning) but could not prevent the coalition from operating.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, ecclesiastical_authority, payer,
    institutional, civilizational, constrained, continental).

% The scribal tradition (monastery-based and urban scribal workshops, professional copyists) depended on Church patronage and a controlled, high-margin manuscript market. Printing displaced this business model for many texts, especially vernacular scripture and polemics. Scribes bore the cost of technological displacement; many transitioned to printing, became proofreaders, or lost income and status. They were not agents of the coalition but structural casualties of the bypass mechanism.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, manuscript_scribal_networks, payer,
    moderate, biographical, constrained, regional).

% The Church built and deployed censorship infrastructure (Index librorum prohibitorum, inquisitorial courts, licenses, imprimaturs) as enforcement machinery to counter the coalition. This apparatus did not prevent the coalition from operating; it raised costs and created risk stratification by jurisdiction. The enforcement was real but ultimately inadequate to contain the breach in the monopoly.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, ecclesiastical_censorship_apparatus, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__beneficiary_agency_reading, ecclesiastical_censorship_apparatus).

% Ordinary people (non-clerical lay readers, women, vernacular-speaking populations, merchants, artisans) gained access to scripture in vernacular, polemical tracts, and competing theological claims — texts that were either forbidden or economically unavailable in manuscript form. The coalition's strategy created a new textual commons for lay readers. Lay readership is a beneficiary, though not an agenda-setter; the coalition did not organize around lay preferences but benefited lay readers as a downstream effect and strategic rationale of the authority bypass.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, lay_readership, beneficiary,
    powerless, generational, mobile, continental).

% Kings, princes, city councils (especially in the German territories, Switzerland, Netherlands) exercised varying degrees of tolerance or active support for the reformer-printer coalition. Some offered safe haven (Geneva sheltered printers), some levied taxes on printed books, some allowed Reformation texts while opposing others, some shifted positions over decades. Political authorities observed and selectively enabled the constraint; their power shaped the regional feasibility of the bypass but did not determine the coalition's formation or core strategy.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, secular_political_authorities, observer,
    institutional, generational, analytical, regional).

% Printers sympathetic to counter-reformation theology and Church authority were economically disadvantaged by the reorientation of market demand toward reform texts in Protestant regions. They lacked access to the reformer-printer coalition's networks and strategic coordination. Their exclusion from the coalition meant they competed on price and availability against organized Protestant supply. Some survived by serving Catholic markets; others lost income.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, catholic_counter_reformation_printers, excluded,
    powerful, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, reform_movement_leadership).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reformers and printers coordinated to circumvent the Church's monopoly on authorized textual interpretation by manufacturing and distributing vernacular scripture and polemic outside ecclesiastical channels. Printing enabled speed and scale; coalition strategy deployed it deliberately toward this goal. The coordination solved the problem of getting heterodox theology to populations the Church restricted from it.
% TRANSFER_FUNCTION: Authority over textual interpretation and scriptural meaning transferred from the ecclesiastical hierarchy to a distributed coalition of reformers and printers operating outside Church permission structures. Economic value in the printed-text market transferred from manuscript producers and Church-controlled scribal networks to Protestant-sympathetic printers and reformer-adjacent publishers. Interpretive legitimacy transferred from Church-authorized exegesis to vernacular Bibles and reformer commentary. Lay readers gained access that the Church had restricted.
% ABSENT_VOICES: Subordinate scribes and manuscript workshops had no seat in the coalition's decisions and bore displacement costs without input; the Church's theological counter-reformation writers and defenders were structurally excluded from the new printing markets in Protestant regions; some Catholic printers were economically disadvantaged by the reorientation of demand toward reform texts; populations in regions where political authorities sided with the Church had restricted access to reform texts and no voice in the coalition.
% DISAPPEARANCE_RATIONALE: If the reformer-printer coalition and its strategic coordination disappeared — if reformers had not identified printing as a tool or if printers had declined to partner with them — the Reformation would have operated at manuscript speed and scale, confined to Latin-reading elites and face-to-face networks. The Church's monopoly on textual authority would have degraded more slowly (or possibly held). The distribution and speed of doctrinal challenge would have been fundamentally different. Authority structures would remain concentrated in clerical and institutional hands. The constraint's disappearance would have arrested the mechanism by which authority was successfully bypassed.
% FOUNDING_PROBLEM: The Church held an enforced monopoly on authorized scripture interpretation, channeled through manuscript production, Latin gatekeeping, and institutional hierarchy. Reformers sought to challenge this monopoly by distributing competing scriptural interpretation to lay populations in vernacular. Printing offered a technical means to achieve that distribution at unprecedented scale and speed.
% FOUNDING_PROBLEM_CORROBORATION: Reformer correspondence, printer contracts, and inquisitorial records all document this problem-and-solution explicitly: Luther's letters to printers about edition strategy; printing-shop records of reform-text volume and pricing; Church censorship indexes naming printed works and their distribution networks. Secular historians (Elizabeth Eisenstein on print culture, Andrew Pettegree on reform publishing, Roger Chartier on reading practices, Robert Darnton on book history) outside the benefiting parties attest the strategic coordination and the foundational problem of ecclesiastical monopoly and reformer challenge to it. None of these historians are invested in the reformer-printer coalition's success; they document it as historical fact.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.62 at interval end) because the coalition deliberately extracted authority and economic surplus from the incumbent ecclesiastical monopoly. This is not incidental; the constraint exists because reformers chose to use printing as a bypass mechanism. Suppression is higher (0.71) because the Church built and deployed censorship apparatus specifically to counter the coalition — enforcement was real and escalating. Theater rises over the interval (0.08 to 0.28) because over time the original function (bypassing ecclesiastical authority over live theological disputes) stabilized into institutional defense (protecting Protestant printing markets and intellectual property), while performative work (justifying the monopoly against Catholic counter-reformation printing) grew. Accessibility collapse is moderate (0.48) because alternatives to printing existed (manuscript circulation, oral preaching) but printing was so much more efficient that the alternatives collapsed as viable for mass distribution. Resistance is substantial (0.59) because the Church mounted genuine resistance (censorship, inquisition, book-burning) and Catholic printers competed in the market — the constraint was contested. The claim-metric gap is intentional: the constraint is CLAIMED as tangled_rope (mutual extraction in the coalition, asymmetric impact on Church/scribes) while the metrics describe the structural enforcement required to hold that coalition together and maintain its bypass function against Church opposition.
 *
 * PERSPECTIVAL GAP:
 *   The reformer-printer coalition seats and the ecclesiastical-authority seat should compute to different types. From the coalition's position, the constraint appears as rope (or barely tangled) — mutual benefit, shared interest in textual production, coordinated distribution. From the Church's position, the same structure operates as a snare: extraction of authority, suppressed alternatives (Church-authorized texts), enforced loss of interpretive gatekeeping. From the printer's individual position (power=powerful, exit=mobile), the constraint may read as rope (genuine market opportunity, some exit via Catholic printing). From the manuscript-scribe position (power=moderate, exit=constrained), it reads as snare (displacement, no viable alternative income). The engine computes per-seat divergence; the beneficiary-agency reading does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform-movement leadership: near full beneficiary (d ≈ 0.2). They set the agenda, define the strategy, and benefit from bypassed authority. They bear some risk (prosecution, excommunication) but have high time-horizon and institutional backing in many jurisdictions; exit is mobile. Independent printers: near symmetric (d ≈ 0.4–0.5). They benefit from the market opportunity and extract profit, but also bear significant legal risk and market uncertainty; they have arbitrage exit (Catholic printing, secular work) but most lose income if reform texts collapse. Ecclesiastical authority: near full target (d ≈ 0.95). They bear the cost of lost monopoly and must actively suppress; they have constrained exit (cannot abandon the textual domain without losing authority). Manuscript-scribal networks: near full target (d ≈ 0.9). They lose income and status; they have constrained exit (most cannot transition to printing, some cannot compete). Lay readership: symmetric to slightly beneficiary (d ≈ 0.3–0.4). They gain access but bear some costs (heresy risk, changed spiritual authority structures); they have mobile exit (can ignore texts, can read Catholic alternatives where available). The structural asymmetry — coalition members benefit, incumbents and displaced workers pay — is the core of the tangled_rope claim: genuine coordination function (getting texts produced and distributed) with asymmetric extraction (benefits to reformers/printers, losses to Church/scribes).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Church monopoly on textual interpretation restricts lay access to scripture) is LIVE — the constraint persists because the problem persists. However, by 1550, the constraint's function has begun to shift: from active authority-bypass (solving a live problem) toward institutional defense (protecting Protestant printing markets against Catholic competition and preserving profits). The theater_ratio rises (0.08→0.28) as justification work increases relative to functional work. This is the edge of mandatrophy: the constraint still solves its founding problem, but is increasingly maintained by ideology and institutional inertia rather than active necessity. The rising theater signals that by 1600, absent sustained theological ferment and market competition, the constraint might degrade to piton status (maintained theatrically, primary function atrophied). The engine should flag this as a mandatrophy-warning case: live problem, but functional transition underway.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_determinism_boundary,
    'Would the Reformation have taken place at similar speed and scale without printing, given reformer intent and theological ferment, or is printing causally necessary rather than instrumentally deployed?',
    'Counterfactual historical analysis comparing pre-printing reform movements (Wycliffe, Hus) to post-printing scale and speed; comparison with non-printing cultures'' religious dissent trajectories; textual analysis of reformer correspondence to distinguish strategic deployment from technological inevitability framing.',
    'If reformers deployed printing strategically and comparable theological ferment would have continued at slower speed, this reading holds and technology_reformation_causality__technological_determinism_reading is coexists_with. If printing was genuinely necessary (no pre-printing dissent reached similar scale), the boundary between the readings shifts and influences relationship may apply instead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_determinism_boundary, conceptual, 'Whether printing is causally necessary or strategically instrumental to Reformation scale/speed.').

omega_variable(
    printer_agency_vs_market_response,
    'Were printers active agents choosing to partner with reformers because of shared conviction or market opportunity, or did they respond to reformer demand as passive suppliers?',
    'Analysis of printer correspondence, marginal annotations, printing-shop acquisition of specific fonts/cases, edition choices in non-reform markets, printer patronage networks, and printer self-positioning relative to reform and counter-reform.',
    'If printers were active strategic partners (shared conviction, chose edition strategy, shaped textual form), this reading''s tangled_rope claim of mutual extraction holds. If printers were demand-following suppliers, the relationship may be better modeled as scaffold (temporary supplier to transient demand) or rope (coordination with passive suppliers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_agency_vs_market_response, empirical, 'Degree of printer agency in the reformer-printer coalition.').

omega_variable(
    beneficiary_extraction_symmetry,
    'Is the extraction between reformers and printers symmetric (both extract value, both bear enforcement risk), or does one party extract more while the other bears disproportionate costs?',
    'Economic analysis of profit margins, volume flow, and legal risk by party; printer-reformer contract terms where preserved; comparison of reformer imprisonment/prosecution rates to printer imprisonment rates across jurisdictions; analysis of how censorship loss was distributed.',
    'If symmetric, the tangled_rope classification holds with mutual extraction. If asymmetric (printers bear more legal risk while reformers capture more interpretive authority, for example), the constraint may be better modeled as rope (printers as coordinated suppliers) or snare (extraction concentrated on one party).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_extraction_symmetry, empirical, 'Whether reformer-printer extraction is symmetric or asymmetric.').

omega_variable(
    reading_vs_technological_determinism_foreclosure,
    'Does this beneficiary-agency reading logically foreclose the technological-determinism reading, or are both framings coherent within different epistemic commitments?',
    'Examination of the two readings'' core premises: agency reading asserts reformers deployed printing strategically; determinism reading asserts printing made Reformation inevitable. These are not logical contradictions IF ''strategic deployment'' and ''inevitable consequence'' operate at different causal levels (agent intention vs. structural outcome). If they are genuinely contradictory (no unified framework could hold both), the relation is forecloses; if they can coexist (one describes intention, one describes outcome), the relation is coexists_with.',
    'This omega is the decision hinge for cs_structure.reading_relations: if forecloses, then technological_determinism_reading must be marked as foreclosed by this reading''s axioms; if coexists_with, both readings remain live positions in a common discourse about causality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_technological_determinism_foreclosure, conceptual, 'Logical relationship between beneficiary-agency reading and technological-determinism reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1450, 0.08).
narrative_ontology:measurement(tech_tr_t1480, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1480, 0.12).
narrative_ontology:measurement(tech_tr_t1510, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1510, 0.18).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1530, 0.25).
narrative_ontology:measurement(tech_tr_t1550, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1550, 0.28).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement(tech_be_t1480, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1480, 0.28).
narrative_ontology:measurement(tech_be_t1510, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1510, 0.48).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1530, 0.58).
narrative_ontology:measurement(tech_be_t1550, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1550, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1450, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1450, 0.35).
narrative_ontology:measurement(tech_su_t1480, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1480, 0.48).
narrative_ontology:measurement(tech_su_t1510, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1510, 0.62).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1530, 0.68).
narrative_ontology:measurement(tech_su_t1550, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1550, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__beneficiary_agency_reading, 0.12).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of a contested kernel decomposed into three constraint stories per the ε-invariance principle (OQ-258). Each reading has a distinct ε, beneficiary structure, and type. The beneficiary-agency reading emphasizes coalition intentionality and strategic deployment of technology; the technological-determinism reading emphasizes technological causality; the co-constitution reading emphasizes mutual shaping. All three are linked via network.affects_constraints and share commentary.kernel_context documentation of their sibling relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__beneficiary_agency_reading, powerless, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
