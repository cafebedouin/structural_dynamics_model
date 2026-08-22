% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Weaponization of Printing Press by Reformation Reformers and Printers
 *   domain: religious_history/technology/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates the 'strategic deployment' reading of the
 *   contested kernel around printing press causality in the Reformation. The
 *   reading frames reformers (Luther, Calvin, Zwingli) and printing merchants
 *   as deliberate agents who weaponized printing technology — coordinating on
 *   text production, commissioning strategically timed publications, building
 *   distribution networks — to challenge Catholic Church doctrinal authority
 *   and expand their own religious and economic influence. The printing press
 *   is classified here as a tool deployed strategically by organized agents
 *   with clear beneficiary structure (reformers gain doctrinal reach and
 *   institutional power; printers gain profit) and clear targets (Church
 *   loses textual monopoly; manuscript scribes lose livelihoods). The
 *   constraint is claimed as tangled_rope: genuine coordination function
 *   (disseminate texts at scale) AND asymmetric extraction (Church authority
 *   and scribe livelihoods are extracted from) AND active enforcement
 *   (suppression by Church, distribution coordination by reformer-printer
 *   networks). This is one of three contending readings; the sibling readings
 *   (technological_determinism, co_constitution) attribute causality
 *   differently — to the press's autonomous enabling properties or to
 *   feedback loops between technology and human agency.
 *
 * KEY AGENTS:
 *   - Reformation reformers (Luther, Calvin, Zwingli, Melanchthon): agenda-setters who commission specific texts and coordinate with printers on strategic publication timing and content to maximize doctrinal reach and challenge Church authority.
 *   - Printing merchants and press operators (Froben, Cranach, Strasbourg printers): beneficiaries who profit from Reformation text demand, coordinate distribution networks, and make investment decisions to prioritize religious content.
 *   - Catholic Church hierarchy (Pope, bishops, monastic orders): institutional target whose textual gatekeeping authority erodes, whose revenue streams (indulgences) are challenged, and whose doctrinal monopoly is undermined by vernacular print.
 *   - Manuscript scribes (monastery scriptoria, professional copyists): identity-locked victims who lose livelihoods and social status as printing displaces hand-copying.
 *   - Expanding literate readership (merchants, gentry, burghers): passive beneficiaries who gain access to vernacular religious texts and interpretive voice, but do not set the agenda.
 *   - Excluded religious networks (Islamic, Jewish, heterodox Christian): structurally excluded from the reformer-printer alliance; their absence from the printing coalition is maintained by reformer-printer control of capital and patronage.
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
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Weaponization of Printing Press by Reformation Reformers and Printers").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "religious_history/technology/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, '91b24219-bc33-4c4a-9f18-2cbfa823e2e9').
narrative_ontology:cs_kernel_codification('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', formalized).
narrative_ontology:cs_authority_grounding('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', extraction).
narrative_ontology:cs_interpretation_layer_present('91b24219-bc33-4c4a-9f18-2cbfa823e2e9').
narrative_ontology:cs_reading_relation('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', press_reformation_causality__co_constitution, influences).
narrative_ontology:cs_axiom('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', foundational, reformer_printer_intentional_agency).
narrative_ontology:cs_axiom_status(reformer_printer_intentional_agency, holdable).
narrative_ontology:cs_axiom_grounding('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', reformer_printer_intentional_agency, empirically_contingent).
narrative_ontology:cs_axiom('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', foundational, print_technology_as_deliberate_tool_not_autonomous_force).
narrative_ontology:cs_axiom_status(print_technology_as_deliberate_tool_not_autonomous_force, holdable).
narrative_ontology:cs_axiom_grounding('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', print_technology_as_deliberate_tool_not_autonomous_force, empirically_contingent).
narrative_ontology:cs_reference_frame('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', church_textual_monopoly_authority).
narrative_ontology:cs_drift_state('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', reformation_success_1555, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('91b24219-bc33-4c4a-9f18-2cbfa823e2e9', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reformation_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printing_merchants).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_church_authority).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, manuscript_scribes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, print_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious leaders (Luther, Calvin, Zwingli, and their networks) who deliberately commissioned printing of vernacular scripture, polemic tracts, and theological arguments to circumvent Catholic Church control of textual authority and religious interpretation. They set the agenda for what gets printed, coordinate with printers on content strategy, and benefit from the expansion of their theological reach and institutional influence. Their exit from the constraint would mean returning to manuscript-dependent dissemination and losing doctrinal velocity.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformation_reformers, agenda_setter,
    powerful, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, reformation_reformers, beneficiary).

% Printer-publishers (Gutenberg's heirs, Strasbourg, Basel, and Wittenberg presses) who recognized explosive market demand for Reformation texts and actively courted reformer patronage and readership. They profit directly from printing volume, coordinate distribution networks with booksellers and merchants, and drive innovation in rapid reproduction. They set pricing and distribution strategy and choose which manuscripts to prioritize. Their exit would mean returning to lower-volume, lower-margin manuscript reproduction.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printing_merchants, beneficiary,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, printing_merchants, agenda_setter).

% The papal hierarchy, bishops, and monastic institutional structure that depended on controlled manuscript distribution, clerical literacy monopoly, and textual authority over doctrine. The coordinated printing of vernacular Reformation texts in high volume undercuts their interpretive monopoly, erodes their textual gatekeeping authority, and threatens their institutional economic model (indulgence sales, pilgrimage fees, tithe legitimacy). Their options are to suppress printing (require enforcement), produce competing Catholic Counter-Reformation texts (late response), or accommodate doctrinal pluralism (unacceptable to institutional identity). They bear the loss of institutional authority and economic rents.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_church_authority, payer,
    institutional, civilizational, constrained, continental).

% Professional manuscript copyists and monastery scriptoria who lose economic livelihood and social status as printing technology displaces hand-copying. Their professional identity is fused with scribal practice; many cannot or will not transition to printer employment (different skill set, different social position). They bear the cost of technological displacement with limited alternatives.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, manuscript_scribes, payer,
    moderate, biographical, identity_locked, regional).

% Expanding literate populations (merchants, rising gentry, some burghers) who gain access to religious texts in their native languages rather than Latin, and who gain voice in theological interpretation through printed argumentation. They benefit from democratized textual access and lower text prices. They are passive beneficiaries of the constraint structure rather than agenda-setters.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, print_readers, beneficiary,
    organized, biographical, mobile, continental).

% Islamic scholars, Jewish communities, and heterodox Christian sects that might have used printing technology for their own doctrinal dissemination but were geographically, legally, or economically excluded from access to presses and distribution networks controlled by the reformer-printer coalition. Their exclusion is structurally maintained by the reformer-printer agenda-setting dominance.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, rival_religious_networks, excluded,
    organized, biographical, trapped, continental).

% International book merchants and distribution networks (Frankfurt book fairs, Antwerp traders) that operate the middle layer between printers and end readers. They observe the constraint structure, profit from it, and can route texts across borders and jurisdictions, enabling circumvention of Church suppression efforts.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, merchant_book_networks, observer,
    powerful, biographical, arbitrage, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, printing_merchants).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinate rapid reproduction and distribution of vernacular religious texts to expand readership and doctrinal influence beyond manuscript-dependent constraints. Solve the problem of how to disseminate complex theological argument at scale and velocity to geographically dispersed audiences without relying on clergy gatekeeping.
% TRANSFER_FUNCTION: Transfers religious and economic authority from the Catholic Church hierarchy to reformer networks and printing merchants. The Church loses textual monopoly, institutional prestige, and revenue streams (indulgences, pilgrimage fees tied to controlled scriptural interpretation). Reformers and printers gain doctrinal reach, institutional influence, and economic benefit. Manuscript scribes lose livelihoods. Readers gain textual access and interpretive voice.
% ABSENT_VOICES: Islamic and Jewish scholars, heterodox Christian sects, and competing reformer networks that lack patronage relationships with powerful printers or reformer leaders. They would argue for equal access to print technology and distribution networks, but are structurally excluded by the reformer-printer coalition's control of capital, patronage, and merchant relationships. Manuscript scribes object to displacement but lack power to resist.
% DISAPPEARANCE_RATIONALE: If the strategic weaponization of the press by reformers and printers vanished — if reformers had not pursued printing strategy and printers had not coordinated with them — the Reformation would have proceeded much more slowly through manuscript dissemination, oral preaching, and institutional challenge without the velocity and reach that print enabled. Catholic Church authority would have retained stronger textual gatekeeping control. Printing would have developed for other purposes (scientific texts, commercial documents, administrative records) but without the same intensity of religious content production and distribution that the strategic reformer-printer alliance drove.
% FOUNDING_PROBLEM: How to disseminate vernacular religious texts and complex theological argumentation at scale and speed to audiences beyond the reach of manuscript copying and clerical monopoly, in order to challenge Catholic Church authority over doctrine and interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Reformation historians (not funded by religious denominations) document explicit strategic coordination between reformer leaders (Luther's correspondence with printers, Calvin's commissioning strategy, Zwingli's text publication timing) and printer-publishers (Froben's Basel press, Cranach's Wittenberg press, Strasbourg printshops). Printer account books and reformer letters demonstrate intentional alliance. Publishers' records show economic incentive targeting Reformation content production. No corroborating source disputes that reformers and printers strategically deployed printing; the contest is whether this agency-driven causality is the primary mechanism or is co-constituted with technological feedback loops and market demand.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The extractiveness trajectory (0.12 → 0.68 across 115 years) reflects the acceleration of authority extraction as printing technology scales from novelty (1440s) to established infrastructure (1555). In the early period (1440–1470), extraction is low because printing is not yet tied to religious conflict; the constraint does not yet constitute a weaponization system. Around 1500, as Luther's texts begin circulating and Reformation controversy ignites, extraction rises steeply (0.28 → 0.48) because the strategic coordination between reformers and printers becomes manifest and the Church's response (suppression attempts, doctrinal counter-attack) becomes necessary to defend authority. By 1555 (Peace of Augsburg), extraction stabilizes at 0.68 because the structural outcome is quasi-settled: Reformation territories maintain their reformed churches, printing has diffused widely, and Catholic Counter-Reformation printing campaigns acknowledge the technology. The suppression_requirement trajectory tracks enforcement intensity: early enforcement is light (Church's initial response is doctrinal debate and limited book-burning); it intensifies as printing accelerates (papal bans, inquisitorial lists, controlled-book indexes appear). Theater_ratio (0.08 → 0.41) rises as performative enforcement activity (index-making, book-burnings) becomes visible relative to functional enforcement (which is increasingly impossible — suppression cannot prevent all printing). The measurement points share one aligned time grid: every metric is authored at 1440, 1470, 1500, 1520, 1540, 1555.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer and printer perspective, the constraint is genuine coordination (we need to disseminate texts at scale; printing solves that problem; we invest in the technology and distribution networks). From the Church's institutional perspective, the same structure operates as extraction (our textual authority and revenue streams are being undermined by a coordinated print-and-distribute campaign we did not control). From the manuscript scribe's perspective, the constraint is displacement (our professional expertise is made obsolete by a technology we did not choose to adopt). These divergent experiences compute to different per-seat type classifications: agenda-setters see rope; targets see snare; displaced workers see snare; passive beneficiaries see rope (they gain access without bearing enforcement cost). The engine computes this from power + exit + directionality; the commentary explains why the structural asymmetry justifies the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers: d near 0.0–0.2 (full beneficiary end). They set the agenda (set the constraint), commission texts, direct distribution, and directly profit from doctrinal influence expansion and commercial sales. Their exit is high-optionality (arbitrage: they could abandon printing and return to manuscript, but profit incentives prevent that; they have powerful institutional backing). Church: d near 0.85–1.0 (full target end). They lose textual monopoly, pay suppression costs (enforcement staff, book-burning, index compilation), and lose revenue streams. Exit is constrained (they cannot simply abandon doctrinal authority without institutional dissolution). Manuscript scribes: d near 0.8–0.95 (target end). They lose livelihoods with no viable alternative (identity_locked exit: scribal practice is their profession and identity; printer work requires different skills and different social standing). Print readers: d near 0.3–0.4 (beneficiary-leaning). They gain textual access without bearing enforcement cost; their optionality is high (they can read or not, adopt or reject doctrinal positions) and their exit is mobile. This directionality structure (beneficiaries + targets + identity-locked targets + passive beneficiaries) is characteristic of tangled_rope: real coordination function (all parties benefit from text availability) + asymmetric extraction (Church and scribes bear concentrated costs while reformers/printers capture concentrated benefits) + active enforcement (suppression, distribution control) + selective beneficiaries (readers gain access; reformers/printers gain power/profit).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits no mandatrophy (the founding problem does not disappear while the constraint persists). The founding problem — how to disseminate vernacular religious texts at scale to challenge Church doctrinal monopoly — remains live at 1555 as the constraint stabilizes. Printing technology and reformer-printer networks continue to serve the founding problem as new theological and political controversies emerge. The constraint's function does not atrophy; it evolves. By contrast, if printing had become obsolete (e.g., by 1700 when manuscript culture was entirely displaced and the theological controversy had formally settled via Peace of Westphalia), while reformer-printer networks persisted through institutional inertia, that would signal mandatrophy. No such signal is present here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_intent_vs_market_response,
    'Is the printing of Reformation texts primarily the result of deliberate strategic coordination between reformers and printers, or does it emerge from market response to preexisting demand created by theological controversy and literacy expansion?',
    'Documentary evidence from printer commissions, reformer correspondence, and cost-accounting records that demonstrate explicit contractual coordination vs. passive market-driven publication. Archival analysis of commission patterns, timing of text publication relative to reformer requests, and printer investment decisions.',
    'If strategic coordination is dominant, the reading''s premise that reformers and printers weaponized the press holds strongly (tangled_rope classification is supported). If market response is dominant, the technological_determinism and co_constitution readings gain structural weight — the press spread Reformation ideas because market demand pulled it, not because reformers pushed it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strategic_intent_vs_market_response, empirical, 'Whether printing is agent-driven strategy or market-driven response.').

omega_variable(
    alternative_reformation_trajectories,
    'If printing had not been strategically deployed by reformers and printers, could the Reformation have succeeded through manuscript dissemination, oral preaching, and institutional networks alone?',
    'Counterfactual analysis comparing the Reformation''s actual rapid spread with the pace and reach of pre-printing heresies (Wycliffe, Huss, Waldensians) that lacked printing allies. Modeling of information velocity and geographic reach under manuscript vs. print constraints.',
    'If manuscript dissemination could have sustained Reformation momentum at comparable speed, then printing is accelerant rather than necessary cause, and technological_determinism is weakened. If printing is necessary for the observed velocity and geographic reach, strategic_deployment reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reformation_trajectories, empirical, 'Necessity and sufficiency of print technology for Reformation success.').

omega_variable(
    reading_framing_ambiguity,
    'Does this reading''s attribution of strategic agency to reformers and printers depend on a particular philosophical frame about how historical causality is assigned?',
    'Explicit comparison of this reading''s causal claims with alternative framing (technological co-constitution, structuralist analysis of market forces, determinist reading of technological inevitability) applied to the same empirical record. Examination of which structural elements each frame highlights and which it obscures.',
    'If the reading''s agency attribution depends on a non-canonical philosophical frame (and alternative frames are equally defensible on the evidence), then the reading is conceptual rather than empirical — it is one valid reading among equally valid alternatives, not a settled structural fact. This affects the status of related omega variables about strategic intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Philosophical frame-dependence of the strategic agency reading.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the Catholic Church''s loss of textual authority due to structural barriers (technology made suppression impossible) or to ideological/institutional capture (Church authorities adopted printing, adapted to competitive doctrinal marketplace)?',
    'Analysis of Counter-Reformation printing campaigns (1560+) showing whether Church could have adopted printing earlier and suppressed Reformation texts, or whether institutional identity prevented adoption. Examination of suppression attempts (papal bans, inquisitorial book-burning) and their effectiveness.',
    'If structural barriers made suppression impossible (printing technology is inherently decentralized), then the constraint''s suppression is structural and the extraction mechanism is technological. If the Church could have suppressed through counter-printing but chose not to (institutional identity tied to manuscript monopoly), then suppression is internalized and the extraction is organizational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is technological necessity or institutional choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1440, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causality__strategic_deployment, theater_ratio, 1440, 0.08).
narrative_ontology:measurement_basis(pres_tr_t1440, projected).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causality__strategic_deployment, theater_ratio, 1470, 0.15).
narrative_ontology:measurement_basis(pres_tr_t1470, observed).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__strategic_deployment, theater_ratio, 1500, 0.24).
narrative_ontology:measurement_basis(pres_tr_t1500, observed).
narrative_ontology:measurement(pres_tr_t1520, press_reformation_causality__strategic_deployment, theater_ratio, 1520, 0.36).
narrative_ontology:measurement_basis(pres_tr_t1520, observed).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__strategic_deployment, theater_ratio, 1540, 0.4).
narrative_ontology:measurement_basis(pres_tr_t1540, observed).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__strategic_deployment, theater_ratio, 1555, 0.41).
narrative_ontology:measurement_basis(pres_tr_t1555, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causality__strategic_deployment, base_extractiveness, 1440, 0.12).
narrative_ontology:measurement_basis(pres_be_t1440, projected).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causality__strategic_deployment, base_extractiveness, 1470, 0.28).
narrative_ontology:measurement_basis(pres_be_t1470, observed).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__strategic_deployment, base_extractiveness, 1500, 0.48).
narrative_ontology:measurement_basis(pres_be_t1500, observed).
narrative_ontology:measurement(pres_be_t1520, press_reformation_causality__strategic_deployment, base_extractiveness, 1520, 0.62).
narrative_ontology:measurement_basis(pres_be_t1520, observed).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__strategic_deployment, base_extractiveness, 1540, 0.66).
narrative_ontology:measurement_basis(pres_be_t1540, observed).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__strategic_deployment, base_extractiveness, 1555, 0.68).
narrative_ontology:measurement_basis(pres_be_t1555, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1440, press_reformation_causality__strategic_deployment, suppression_requirement, 1440, 0.15).
narrative_ontology:measurement_basis(pres_su_t1440, projected).
narrative_ontology:measurement(pres_su_t1470, press_reformation_causality__strategic_deployment, suppression_requirement, 1470, 0.35).
narrative_ontology:measurement_basis(pres_su_t1470, observed).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__strategic_deployment, suppression_requirement, 1500, 0.52).
narrative_ontology:measurement_basis(pres_su_t1500, observed).
narrative_ontology:measurement(pres_su_t1520, press_reformation_causality__strategic_deployment, suppression_requirement, 1520, 0.68).
narrative_ontology:measurement_basis(pres_su_t1520, observed).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__strategic_deployment, suppression_requirement, 1540, 0.71).
narrative_ontology:measurement_basis(pres_su_t1540, observed).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causality__strategic_deployment, suppression_requirement, 1555, 0.72).
narrative_ontology:measurement_basis(pres_su_t1555, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, global_infrastructure).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__strategic_deployment, 0.18).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% The kernel 'press_reformation_causality' decomposes into three structurally distinct constraints, each instantiating a different reading of what caused Reformation success. The strategic_deployment reading attributes causality to reformer and printer agency (intentional coordination, strategic commissioning, distribution control). The technological_determinism reading attributes causality to the press's autonomous enabling properties (vernacular scripture spread was inevitable given the technology). The co_constitution reading attributes causality to feedback loops between technology and human action (neither is primary). These are not three perspectives on one constraint; they are three different constraints with different ε values, different beneficiary/victim structures, different claimed types. The strategic_deployment reading (this file) is UPSTREAM of the other two in that it asserts agency causality explicitly; the other readings must either accept or reject this causal framing. All three share the same empirical record (books printed, Reformation's success) but decompose the causal mechanism differently. Readers and downstream analysis should treat these three files as a constraint family linked by network.affects_constraints, with different reference frames and different ε values for the same historical outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__strategic_deployment, institutional, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
