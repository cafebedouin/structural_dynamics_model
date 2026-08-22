% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Strategic Coalition Bypassing Church Authority
 *   domain: history/technology/religious_authority
 *
 * SUMMARY:
 *   This reading centers on the deliberate strategic agency of the
 *   reformer-printer coalition in deploying printing technology to bypass
 *   Church authority. Reformers (Luther, Zwingli, Calvin) and printing
 *   entrepreneurs formed a mutual extraction arrangement: reformers needed
 *   printer capacity and logistical networks; printers needed ideologically
 *   committed authors and guaranteed markets. Together, they extracted the
 *   Church's monopoly on theological interpretation and clerical gatekeeping.
 *   The constraint is NOT that printing *caused* the Reformation
 *   (technological determinism) or that technology and social movements
 *   *co-evolved* (co-constitution reading). Rather, it is that beneficiary
 *   agents—reformers seeking authority-bypass and printers seeking
 *   profit—*strategically deployed* printing technology as the instrument.
 *   The technology was crucial but was never independent: its deployment was
 *   directed, selective, and contingent on reformer-printer alignment. This
 *   reading instantiates a tangled_rope: genuine coordination function (both
 *   parties benefited from alignment) combined with asymmetric extraction
 *   (Church, clergy, and Church-aligned printers bore the cost). The
 *   measurement series track the constraint's intensification: as reformist
 *   printing accelerated and geographic reach expanded (1440–1560), the
 *   extractiveness and suppression both rose. The theater ratio remained
 *   moderate, indicating that the constraint's function remained
 *   substantially real (bypassing gatekeeping) even as performative elements
 *   increased (Counter-Reformation propagandistic printing by Church,
 *   polemical exaggeration by both sides).
 *
 * KEY AGENTS:
 *   - Reformer Movement (Luther, Zwingli, Calvin networks): organized beneficiary/agenda-setter; strong power but constrained by Church opposition; generational time horizon; continental scope; sought strategic control of information distribution and theological authority
 *   - Printing Entrepreneurs (Gutenberg, Basel/Strasbourg workshops, Wittenberg operators): powerful beneficiary/agenda-setter; high exit options (could print for other markets); biographical time horizon; captured profit from reformist demand; negotiated exclusivity and pricing leverage over reformist content
 *   - Church Monopoly Holders (Rome, bishops, papal authority): institutional payer; trapped exit (gatekeeping was their structural function); generational time horizon; lost interpretive authority and education monopoly to vernacular readers and reformist communities
 *   - Latin Clergy & Scribes (monastic and cathedral clergy): moderate-power payer; identity-locked exit (clerical identity was their entire formation); biographical time horizon; lost status rent on manuscript scarcity and gatekeeping knowledge
 *   - Analytical Historian: observer seat; examines the causal structure and whether agency or technological inevitability determines outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.68).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.72).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Strategic Coalition Bypassing Church Authority").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history/technology/religious_authority").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, '0ed394a0-b417-4e0d-a937-612f8e0a1afb').
narrative_ontology:cs_kernel_codification('0ed394a0-b417-4e0d-a937-612f8e0a1afb', distributed).
narrative_ontology:cs_authority_grounding('0ed394a0-b417-4e0d-a937-612f8e0a1afb', practice).
narrative_ontology:cs_reading_relation('0ed394a0-b417-4e0d-a937-612f8e0a1afb', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_reading_relation('0ed394a0-b417-4e0d-a937-612f8e0a1afb', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_axiom('0ed394a0-b417-4e0d-a937-612f8e0a1afb', foundational, reformer_printer_strategic_agency).
narrative_ontology:cs_axiom_status(reformer_printer_strategic_agency, holdable).
narrative_ontology:cs_axiom_grounding('0ed394a0-b417-4e0d-a937-612f8e0a1afb', reformer_printer_strategic_agency, empirically_contingent).
narrative_ontology:cs_axiom('0ed394a0-b417-4e0d-a937-612f8e0a1afb', foundational, technology_as_deployed_tool_not_autonomous_force).
narrative_ontology:cs_axiom_status(technology_as_deployed_tool_not_autonomous_force, holdable).
narrative_ontology:cs_axiom_grounding('0ed394a0-b417-4e0d-a937-612f8e0a1afb', technology_as_deployed_tool_not_autonomous_force, empirically_contingent).
narrative_ontology:cs_reference_frame('0ed394a0-b417-4e0d-a937-612f8e0a1afb', church_theological_gatekeeping_authority).
narrative_ontology:cs_drift_state('0ed394a0-b417-4e0d-a937-612f8e0a1afb', post_reformation_consolidation, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('0ed394a0-b417-4e0d-a937-612f8e0a1afb', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reformer_movement).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, printing_entrepreneurs).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, church_monopoly_holders).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, latin_clergy_gatekeepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, print_readers_vernacular).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, rome_papal_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sought to bypass Church gatekeepers who controlled manuscript distribution and theological interpretation. Reformers (Luther, Zwingli, Calvin and their networks) deliberately identified printing as the strategic tool to reach vernacular audiences and circumvent Rome's authority over text. They authored content, negotiated with printers, and directed distribution toward high-impact audiences. They benefited from expanded reach but depended on printer compliance with reformist ideology—a mutual extraction bond.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, reformer_movement, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, reformer_movement, agenda_setter).

% Printers (Gutenberg's successors, Strasbourg and Basel workshops, Wittenberg operators) captured enormous profit from reformist demand. They negotiated with reformers for exclusive or priority access to ideologically charged texts, built business models around Protestant supply, and extracted rents from the coalition—controlling paper sourcing, press capacity, and distribution networks. They had exit options (printing for Church, nobility, or commercial customers) but the reformist market was the highest-margin opportunity.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, printing_entrepreneurs, beneficiary,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, printing_entrepreneurs, agenda_setter).

% Rome and local bishops had controlled theology and religious education through manuscript monopoly and clerical gatekeeping. Reformist printing stripped their authority to regulate doctrine—vernacular Bibles and polemical treatises circulated past their censorship. They bore the cost of losing interpretive monopoly but could not exit the field; their institutional survival depended on recovering control or adapting teaching authority to the new information environment.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, church_monopoly_holders, payer,
    institutional, generational, trapped, continental).

% Local clergy and monastic scriptoria lost economic and intellectual status. Printing destroyed the scarcity rent on hand-copied manuscript work; their identity as exclusive knowledge-holders eroded as vernacular readers could access texts directly. Their exit was theoretically possible (secular work, migration) but their entire formation was clerical identity—the constraint extracted their professional monopoly and cognitive authority.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, latin_clergy_gatekeepers, payer,
    moderate, biographical, identity_locked, continental).

% Ordinary readers—merchants, craft workers, literate women—gained access to religious texts and reformist arguments they could understand. They did not negotiate with printers or reformers; access to print was a collateral benefit of the coalition's extraction from Church authority. They benefited from the architectural outcome (open access) without bearing direct costs or exercising agency in the coalition.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, print_readers_vernacular, beneficiary,
    powerless, biographical, constrained, regional).

% The papal hierarchy lost continental interpretive authority and faced existential organizational challenge. They could not prevent reformist printing through censorship or legal prohibition alone (printers existed in multiple jurisdictions); their exit options were strategically absent. They tried counter-reformation, censorship lists, and doctrinal response—all defenses against the already-bypassed authority structure. The constraint extracted their historical monopoly on Christian doctrine.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, rome_papal_authority, payer,
    institutional, generational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, rome_papal_authority, excluded).

% Examines the causal structure: whether the printing press *made* the Reformation inevitable (technological determinism), whether reformers and press co-evolved, or whether the reformer-printer coalition *strategically deployed* print to bypass Church authority. This reading centers agency: the technology was the tool; the beneficiary agents (reformers and printers) directed its use toward extracting Church monopoly.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, analytical_historian, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__beneficiary_agency_reading, printing_entrepreneurs).
narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reformers and printers aligned on vernacular scripture and polemical distribution—both needed the other's competence. Reformers needed printing capacity and logistical reach; printers needed ideologically committed authors and guaranteed markets. This coordination solved the problem of circumventing Church gatekeeping at scale; without printer cooperation, reformist writings would have remained local and ephemeral.
% TRANSFER_FUNCTION: Extracted Church's monopoly on theological interpretation and religious education. The constraint moved authority over doctrine from Rome and bishops to vernacular readers and local Protestant communities. It also moved profit from scribal/monastic work to printing entrepreneurs, and it moved labor (compositors, pressmen, binders) from manuscript workshops to print houses. The transfer was asymmetric: reformers and printers both gained; Church and clergy lost.
% ABSENT_VOICES: Rome's perspective is structurally excluded—it describes the outcome as heretical dissemination, not strategic coalition. Indigenous readers of manuscripts (monks, scribes, elite Latin readers) are not in the negotiating room; they experience the constraint as the dissolution of their epistemic authority. The perspective of small-scale printers or those who printed for Church rather than reformers is also absent—the constraint centers on the profitable reformist coalition.
% DISAPPEARANCE_RATIONALE: If the reformer-printer coalition had not formed and remained separate agents, the Reformation would have proceeded more slowly and been far more geographically limited—a theological reform movement without the information-distribution architecture. Church authority would have recovered faster. The religious map of Europe would have been dramatically different: Spain, Italy, and large parts of France might have remained more uniformly Catholic. The emergence of print as a profit center for vernacular publishing depended on reformist demand.
% FOUNDING_PROBLEM: Reformers faced institutional gatekeeping: Rome and local bishops controlled education, theology, and script circulation. Printing technology existed (Gutenberg c. 1440) but was not yet deployed for religious dissemination. The founding problem was how to bypass this gatekeeping at continental scale without armies or legal authority.
% FOUNDING_PROBLEM_CORROBORATION: Luther's deployment of printing, the strategic printing of the 95 Theses, reformers' explicit partnerships with Basel and Strasbourg printers, and contemporary Church accounts of reformist 'propaganda' all document the coalition's intentional strategic use of print. Independent historians of the Reformation (including those skeptical of technological determinism) corroborate that reformers *deliberately chose* printing as an instrument—it was not an accident of technology. Printers' business records show they prioritized reformist contracts for profit, not ideology. The corroboration comes from primary source evidence outside the reformer movement itself: printer contracts, Church responses, and circulation records.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   EXTRACTIVENESS (0.68 at interval end): The constraint extracts Church's authority monopoly—a high-value extraction because interpretive control was foundational to Church power and revenue (tithe justification, penance economics, education fees). The extraction was not small and dispersed but concentrated: the coalition directly diverted theology from Rome to local vernacular readers. However, the extraction was not *total* (Church retained significant authority, especially where reform was suppressed)—hence 0.68 rather than 0.85+. The measurement series shows rising trajectory (0.12 → 0.68) because early printing (1440–1460) was limited to wealthy markets and elite readers; by 1500, vernacular printing was mass-distributed; by 1560, two generations had learned theology outside Church channels. SUPPRESSION (0.72 at interval end): The constraint required active enforcement by the reformer-printer coalition to survive: they had to resist Church censorship, protect printing locations, negotiate with local rulers, and maintain supply chains despite papal prohibition. Suppression was bidirectional—the coalition suppressed Church counter-reformation printing, and Rome suppressed reformist printing where it could. By 1560, suppression was high because the system was defended by force (religious wars, territorial Protestant states), not by participant choice. THEATER_RATIO (0.41 at interval end): Moderate—the constraint's core function (bypassing authority) remained real, but performative elements increased over time. Early reformist printing (1440s–1480s) focused on theological content; by 1530–1560, polemical and propagandistic exaggeration increased. Counter-Reformation printing added more theater (the Index, anti-heretical propaganda). Yet the underlying extraction of gatekeeping authority remained functional: readers continued to bypass Church channels, and clergy continued to lose epistemic authority. Theater did not approach piton-level (0.5+) because the constraint's primary function persisted. ACCESSIBILITY_COLLAPSE (0.52): Alternatives did not collapse completely. Oral preaching, manuscript networks, and institutional teaching remained available to Church authorities and those who rejected print. But vernacular print dramatically raised the cost of maintaining Church monopoly—alternatives existed but were increasingly expensive and labor-intensive. Readers could *theoretically* ignore printed Bibles and listen to priests, but the psychological/economic cost of rejecting free, accessible text was high. RESISTANCE (0.58): Substantial—both Church and ruling authorities mounted real resistance: censorship, book burning, imprisonment of heretics, legal prohibition of printing heretical works. This was not performative: people died for printing and reading reformist materials. However, resistance was not *overwhelming*—reformist printing persisted and proliferated despite suppression, indicating the coalition had structural advantages (multiple printing centers, ruler support in some jurisdictions, profit motive driving innovation) that overcame resistance.
 *
 * PERSPECTIVAL GAP:
 *   REFORMER PERSPECTIVE: Strategic agents deliberately deploying technology to achieve authority-bypass. Printing is a tool they commanded and directed; success depended on their theological clarity, political networking, and will. PRINTER PERSPECTIVE: Profit-maximizing entrepreneurs identifying a high-margin market (reformist demand); technology is neutral and deployable to any demand; reformist content is lucrative but not ideologically binding. CHURCH PERSPECTIVE: This constraint appears as *heretical dissemination enabled by technology*—Rome does not see strategic agency but sees the technology as the causal driver (printing made heresy inevitable and uncontrollable). CLERGY PERSPECTIVE: The constraint extracts their authority and labor; they experience it as institutional collapse driven by external force (the printing press). ANALYTICAL PERSPECTIVE: The constraint is a structured coalition (tangled rope) with mixed motivation—some reformers were ideologically pure, some printers were purely mercenary, and the coalition was never perfectly aligned (some reformers resented printer pricing; some printers feared religious controversy). The engine computes per-seat classification from the structural data. The reformer-as-agenda-setter will compute the constraint differently from the printer-as-agenda-setter because their exit options, power, and beneficiary status differ. This is the expected divergence—it is precisely what the classification system exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   REFORMERS: d ≈ 0.25–0.35 (mild beneficiary lean). They benefit significantly from printer cooperation and from Church authority extraction, but they remain constrained by Church opposition and dependent on printer compliance. They cannot unilaterally dictate terms to printers; they face organizational costs (networking, manuscript preparation, ideological alignment) that are nontrivial. They have constrained exit—they *could* retreat to local oral preaching, but that abandons the scale-and-speed advantage that justified the coalition in the first place. PRINTERS: d ≈ 0.15–0.25 (stronger beneficiary lean). They capture the most concentrated profit from the coalition and have high exit options (alternative markets: law, medicine, commercial printing, Church work). Their structural position is strongest; they can negotiate from strength with reformers and are least threatened by Church suppression (multiple jurisdictions, no religious commitment binding them). CHURCH & CLERGY: d ≈ 0.80–0.95 (strong targets). They lose authority, revenue, status, and labor rent. They have trapped or identity-locked exit—their institutional function IS gatekeeping; abandoning it means institutional suicide. They bear the full extraction. VERNACULAR READERS: d ≈ 0.40–0.50 (mild targets). They gain access but do not participate in the coalition; they bear no direct enforcement cost. Their benefits are real (readable scripture, reduced dependence on clergy) but they do not benefit from the *extraction* mechanism itself—they benefit from the outcome. NO OVERRIDES NEEDED because the structural data (beneficiary/victim declarations + exit options + power atoms) derive reasonable d values. Reformers and printers are listed as beneficiaries; Church and clergy as victims. This maps correctly to the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   FOUNDING PROBLEM: Church gatekeeping on theology and education was a persistent institutional arrangement, not a transient problem. FOUNDING_PROBLEM_STATUS: Live until at least 1530, contested by 1560 (Counter-Reformation reformers claimed they *also* were restoring authentic authority; Church argued it never lost legitimate teaching office). DISAPPEARANCE_VERDICT: world_rearranges (if the reformer-printer coalition dissolved, religious Europe reorganizes—slower reform, more regional variation, Church authority recovers in many areas). MANDATROPHY CHECK: The constraint's founding problem was live through the interval; its function (bypassing gatekeeping) remained real; its structure (active enforcement, asymmetric extraction) remained necessary. There is no mandatrophy: the constraint has not outlived its function. The rising extractiveness and suppression measurements indicate the constraint was *intensifying*, not decaying into theater. A piton would show rising theater_ratio with stable or declining extractiveness (the core function atrophying while enforcement became ritualistic). This constraint shows rising theater but *also* rising core function—the bypass was *becoming more effective*, not less. Therefore: no mandatrophy. The constraint is a tangled rope in active operation, not a degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_counterfactual,
    'Would the Reformation have happened without the printing press, or would it have remained a localized theological reform movement?',
    'Comparative historical analysis: examining reformist movements before printing (Wycliffe, Hus) and their reach/durability vs. the Reformation after printing. The beneficiary-agency reading asserts the press was instrumental, not determining; its absence would have crippled the scale and speed, not the movement itself.',
    'If the Reformation would have proceeded substantially unchanged without printing, this reading''s claim that technology was ''tool not cause'' is supported. If printing was genuinely indispensable to the movement''s existence at continental scale, the reading slides toward technological co-constitution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_determinism_counterfactual, empirical, 'Whether the Reformation''s existence and scale was determined by printing or merely amplified by it.').

omega_variable(
    reformer_printer_asymmetry,
    'Did the reformer-printer coalition extract mutual benefit or did one party exploit the other''s dependence?',
    'Granular analysis of printer-reformer contracts, profit distribution, and exit options. If printers could have exited to other profitable markets (law, medicine, commercial almanacs) without significant loss, they had structural leverage over reformers. If reformers had alternatives to print (manuscript, oral teaching, other technologies), the asymmetry reverses.',
    'If asymmetric extraction exists (one party coercing the other), the constraint reclassifies from tangled_rope toward snare for the exploited party. If mutual benefit was genuine and exit options were symmetrically constrained, the rope framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_printer_asymmetry, empirical, 'Whether the reformer-printer bond was mutual coordination or asymmetric exploitation.').

omega_variable(
    church_defeat_inevitability,
    'Could Rome have adapted to or co-opted the printing press to defend its authority, or was Church defeat inevitable once printing was deployed?',
    'Historical examination of Church responses (Gutenberg himself, early indulgence printing, later Counter-Reformation printing): could Rome have captured the printing market first, or was its institutional structure incompatible with rapid adoption?',
    'If Rome could have deployed print as effectively as reformers (but chose not to or moved too slowly), Church defeat was a failure of institutional agility, not technological inevitability. If Rome''s bureaucratic and doctrinal structure made it incapable of deploying print strategically, the constraint''s extractiveness came from the alignment of beneficiary agency (reformer-printer coalition) with Church institutional weakness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_defeat_inevitability, conceptual, 'Whether Church institutional structure foreclosed effective response to printing technology.').

omega_variable(
    kernel_reading_boundary,
    'Is this reading (beneficiary agency directing technology strategically) fundamentally distinct from the co-constitution reading, or are they two framings of the same structural phenomenon?',
    'Clarifying the referent: this reading centers *intentional coalition* and *strategic direction*; co-constitution centers *mutual shaping* and *structural co-evolution*. The readings diverge on whether reformers and printers acted *through* or *with* the technology. Testing the distinction requires examining contemporaneous statements of intent, strategic planning, and contingency in deployment.',
    'If the readings are genuinely distinct (agency vs. co-evolution), they are separate constraint stories with different ε and type classifications. If they describe the same phenomenon at different levels of aggregation, they might be better unified.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the beneficiary-agency reading is structurally distinct from the co-constitution reading of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1440, 1560).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1440, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1440, 0.08).
narrative_ontology:measurement(tech_tr_t1460, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1460, 0.12).
narrative_ontology:measurement(tech_tr_t1480, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1480, 0.18).
narrative_ontology:measurement(tech_tr_t1500, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1500, 0.26).
narrative_ontology:measurement(tech_tr_t1530, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1530, 0.35).
narrative_ontology:measurement(tech_tr_t1560, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1560, 0.41).

% Extraction over time
narrative_ontology:measurement(tech_be_t1440, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1440, 0.12).
narrative_ontology:measurement(tech_be_t1460, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1460, 0.28).
narrative_ontology:measurement(tech_be_t1480, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1480, 0.44).
narrative_ontology:measurement(tech_be_t1500, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1500, 0.58).
narrative_ontology:measurement(tech_be_t1530, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1530, 0.65).
narrative_ontology:measurement(tech_be_t1560, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1560, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t1440, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1440, 0.15).
narrative_ontology:measurement(tech_su_t1460, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1460, 0.32).
narrative_ontology:measurement(tech_su_t1480, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1480, 0.48).
narrative_ontology:measurement(tech_su_t1500, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1500, 0.62).
narrative_ontology:measurement(tech_su_t1530, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1530, 0.68).
narrative_ontology:measurement(tech_su_t1560, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1560, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__beneficiary_agency_reading, 0.12).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-way kernel contest. The beneficiary_agency reading centers on directed strategic coalitional action (reformer-printer alignment as tangled_rope, technology as deployed tool). The co-constitution reading centers on mutual shaping (technology and social movement co-evolving, neither determining). The technological_determinism reading centers on printing as independent causal force. All three describe the same historical phenomenon—the Reformation and printing press relationship—but with structurally different ε and type classifications. The three stories are linked via network.affects_constraints; each one's commentary.kernel_context names the kernel and sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
