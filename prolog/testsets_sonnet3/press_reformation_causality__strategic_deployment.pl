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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Weaponization of the Printing Press Against Ecclesiastical Authority
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story authors the strategic_deployment reading of the
 *   press_reformation_causality kernel: reformers and printers are treated as
 *   rational, coordinated agents who selected and exploited printing
 *   technology as a weapon to break Rome's doctrinal and revenue monopoly,
 *   and territorial princes joined the coalition because it delivered
 *   jurisdiction and confiscated wealth. On this reading the press itself
 *   functions as a rope-grade coordination tool for the reform coalition
 *   (cheap, replicable, synchronizable) while its deployment against the
 *   Church operates as a tangled rope at the level of the whole arrangement:
 *   it coordinates the reform coalition genuinely while extracting authority,
 *   revenue, and legitimacy from Rome, indulgence sellers, and the Latin
 *   clerical monopoly through the same printed channel, sustained by active
 *   suppression contests (indices of prohibited books, excommunication,
 *   princely protection rackets) on both sides.
 *
 * KEY AGENTS:
 *   - reformist_printers: primary beneficiary/agenda_setter (organized/arbitrage) — profits and doctrinal reach from strategic print choices
 *   - reformist_clergy: primary beneficiary/agenda_setter (organized/constrained) — authority and reputational capital from vernacular argument
 *   - territorial_princes: secondary beneficiary (institutional/mobile) — jurisdiction and revenue from backing the press campaign
 *   - roman_curia: primary target (institutional/constrained) — loses doctrinal monopoly and revenue
 *   - indulgence_sellers: primary target (moderate/trapped) — commercial collapse from printed critique
 *   - latin_literate_clerical_monopoly: secondary target (organized/constrained) — loses interpretive gatekeeping
 *   - catholic_counter_printers: analytical excluded voice — mirror-image strategic actor outside this reading's frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.58).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.62).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Weaponization of the Printing Press Against Ecclesiastical Authority").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'aa890120-2c8a-4459-b6cc-6bb16d431764').
narrative_ontology:cs_kernel_codification('aa890120-2c8a-4459-b6cc-6bb16d431764', distributed).
narrative_ontology:cs_authority_grounding('aa890120-2c8a-4459-b6cc-6bb16d431764', distributed).
narrative_ontology:cs_reading_relation('aa890120-2c8a-4459-b6cc-6bb16d431764', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('aa890120-2c8a-4459-b6cc-6bb16d431764', press_reformation_causality__co_constitution, influences).
narrative_ontology:cs_axiom('aa890120-2c8a-4459-b6cc-6bb16d431764', foundational, human_strategic_agency_is_primary_causal_driver).
narrative_ontology:cs_axiom_status(human_strategic_agency_is_primary_causal_driver, holdable).
narrative_ontology:cs_axiom_grounding('aa890120-2c8a-4459-b6cc-6bb16d431764', human_strategic_agency_is_primary_causal_driver, empirically_contingent).
narrative_ontology:cs_axiom('aa890120-2c8a-4459-b6cc-6bb16d431764', secondary, printers_and_reformers_acted_as_rational_self_interested_coalition).
narrative_ontology:cs_axiom_status(printers_and_reformers_acted_as_rational_self_interested_coalition, holdable).
narrative_ontology:cs_axiom_grounding('aa890120-2c8a-4459-b6cc-6bb16d431764', printers_and_reformers_acted_as_rational_self_interested_coalition, empirically_contingent).
narrative_ontology:cs_reference_frame('aa890120-2c8a-4459-b6cc-6bb16d431764', papal_doctrinal_monopoly).
narrative_ontology:cs_drift_state('aa890120-2c8a-4459-b6cc-6bb16d431764', post_ninety_five_theses_print_diffusion, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('aa890120-2c8a-4459-b6cc-6bb16d431764', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reformist_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reformist_clergy).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, territorial_princes).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, roman_curia).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, indulgence_sellers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, latin_literate_clerical_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, lay_readers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, lay_readers).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, vernacular_scriptural_access_doctrine).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, sola_scriptura_legitimacy_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Print shops in Wittenberg, Basel, Strasbourg, and Antwerp deliberately selected pamphlets, broadsheets, and vernacular tracts for their commercial virality and their capacity to undercut Rome's control of doctrinal messaging. Printers chose print runs, formats, and pricing to maximize both profit and reach, coordinating with reformist authors to time releases against indulgence campaigns and church pronouncements. Their exit option is real: they can relocate to friendlier territories or print under pseudonyms, and many did.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformist_printers, agenda_setter,
    organized, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, reformist_printers, beneficiary).

% Luther, Zwingli, and allied theologians consciously exploited the press's economics of scale, writing short, cheap, rhetorically charged German- and vernacular-language tracts instead of Latin treatises, understanding that this format choice would multiply readership and bypass clerical gatekeeping. They collected reputational capital, patronage, and doctrinal authority as the arrangement succeeded; their exit from the confrontation once launched was limited by excommunication risk and princely dependence.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reformist_clergy, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, reformist_clergy, beneficiary).

% German princes and free cities backed reform printing operations because doing so let them seize church lands, revenues, and jurisdiction from Rome while gaining a loyal vernacular-literate populace. They funded presses, protected printers from imperial and papal sanction, and used the press's output as legal and propaganda cover for confiscation. Their exit options were extensive: alliance-switching, selective enforcement, and negotiated settlements with the Empire.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, territorial_princes, beneficiary,
    institutional, generational, mobile, national).

% The papacy and its administrative apparatus lost the capacity to control the pace, volume, and geography of doctrinal debate. Rome's traditional levers — control of Latin scholarship, manuscript scarcity, and clerical mediation of scripture — were structurally bypassed by cheap serial print. Rome tried indices of prohibited books, licensing, and excommunication, but enforcement was outpaced by print volume; the Curia bore the loss of revenue, jurisdiction, and doctrinal monopoly.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, roman_curia, payer,
    institutional, civilizational, constrained, continental).

% Local agents selling indulgences on commission depended on unchallenged clerical authority and public ignorance of countervailing scriptural argument. Once printed critiques (the 95 Theses and its many reprintings, satirical broadsheets) circulated widely and cheaply, their commercial and reputational position collapsed rapidly; they had little capacity to relocate the trade or contest the printed narrative on equal terms.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, indulgence_sellers, payer,
    moderate, biographical, trapped, regional).

% Clergy whose institutional standing rested on being the exclusive interpreters of Latin scripture lost interpretive gatekeeping as vernacular Bibles and tracts multiplied. Their skill set and institutional identity were built around scarcity of access; the strategic vernacular-print campaign devalued that scarcity directly, and their options were largely to convert, resist through counter-printing (a losing race given later Catholic entry), or lose local authority.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, latin_literate_clerical_monopoly, payer,
    organized, generational, constrained, continental).

% Literate and semi-literate lay audiences gained direct access to vernacular scripture and polemic, which many experienced as liberation from clerical mediation. Some also bore costs: exposure to confessional violence, social rupture within families and towns, and manipulation by competing propaganda campaigns whose commercial and factional motives were not always disclosed to them.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, lay_readers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, lay_readers, payer).

% Catholic polemicists and printers entered the same market later and with less initial coordination; their counter-strategic use of the press is a genuine mirror of the reform side's tactics but is outside this story's declared beneficiary/victim structure, which is authored from the reform side's strategic deployment. Their voice, if centered, would reframe the same press as a contested dual-use weapon rather than a reform-side tool.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_counter_printers, excluded,
    organized, biographical, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press genuinely solved a real coordination problem for the reform movement: how to synchronize a geographically dispersed, doctrinally aligned readership around a shared vernacular text and a shared timeline of argument, at a speed and cost no scribal network could match.
% TRANSFER_FUNCTION: The arrangement moves doctrinal authority, revenue (tithes, indulgence income, benefice control), and interpretive legitimacy away from Rome and the Latin clerical monopoly toward reformist clergy, printers, and allied territorial princes, mediated through cheap serial vernacular print.
% ABSENT_VOICES: Catholic counter-printers and the ordinary indulgence-selling clergy at the local level are structurally absent from this reading's framing — their strategic countermoves and their losses are visible only as effects, not as agency, because the reading is authored from the reform side's deployment choices.
% DISAPPEARANCE_RATIONALE: Remove the strategic, coordinated print campaign and the doctrinal challenge does not vanish, but its speed, geographic reach, and resistance to suppression collapse: without the deliberate choice of cheap vernacular pamphlet format over scarce Latin manuscript, Rome's traditional containment tools (excommunication, local suppression, slow manuscript circulation) would likely have contained the controversy the way earlier heresies were contained.
% FOUNDING_PROBLEM: Reformers needed a way to break the Church's monopoly on doctrinal interpretation and public opinion formation fast enough to survive suppression; printers needed profitable, replicable content; princes needed legal and propaganda cover to seize ecclesiastical revenue and jurisdiction. The press-as-weapon arrangement solved all three simultaneously.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the book (outside both reformist and Catholic institutional lineages) attest via surviving print-run records, pricing data, and correspondence between reformers and printers that the strategic coordination was deliberate and time-limited to the confessional struggle; no living party still needs the press deployed this way, since the doctrinal and jurisdictional battles it was built to win were resolved (favorably or unfavorably) centuries ago and the underlying press technology has long since been absorbed into ordinary print culture.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply between 1500 and 1540 (0.22 to 0.55) tracking the period of most intense strategic exploitation — Luther's 95 Theses through the early Schmalkaldic period — then plateaus and gently declines after 1580 as the confessional order stabilizes into territorial settlement (Peace of Augsburg's aftermath, then Westphalia by 1650), which is when the founding problem this reading names becomes largely dead. Theater ratio climbs modestly as both sides increasingly print for propaganda volume rather than doctrinal substance in the later period. Suppression requirement peaks in the 1540-1580 window, the height of active enforcement contests (indices, censorship, excommunications, princely counter-protection), then eases as confessional boundaries harden into accepted territorial fact rather than live contest.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist_printers/clergy seat, the press is coordination technology they built and wielded skillfully — a rope they constructed for their own ends. From the roman_curia and indulgence_sellers seat, the identical printed output is an enforcement-backed extraction of authority and revenue they could not adequately counter. The engine should compute these as genuinely different seat-level classifications from the same structural facts, not as a disagreement to be resolved by picking one true story.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist printers and clergy sit near the full-beneficiary end: they chose the technology, timed its deployment, and captured reputational, doctrinal, and commercial returns, with printers additionally holding arbitrage-grade exit (relocate presses, publish pseudonymously). Territorial princes are also beneficiaries with mobile exit (alliance-switching). Roman curia and the Latin clerical monopoly sit toward the target end: constrained exit, civilizational/generational time horizon meaning the institutional damage compounds slowly and is hard to reverse. Indulgence sellers are the most trapped: moderate power, no meaningful relocation option, and their income model was structurally dependent on exactly the information asymmetry the press destroyed.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy misclassification in both directions: it does not reduce the Reformation to pure technological inevitability (that is the sibling technological_determinism reading, which would erase reformer/printer agency and beneficiary status), nor does it claim the press was merely incidental to a purely theological dispute. By naming concrete beneficiaries (printers, clergy, princes) and concrete victims (curia, indulgence sellers, clerical monopoly) with a real coordination function (synchronizing dispersed vernacular readership) AND active enforcement on both sides (indices, excommunication, princely protection), the tangled_rope classification captures that the press was simultaneously a genuine tool the reform coalition needed and an extractive weapon aimed at a specific institutional target — exactly the hybrid the type is built to detect, distinct from a pure rope (no victims) or pure snare (no genuine coordination function for the reform side).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disaggregation,
    'Is the Reformation''s press-driven trajectory best explained by strategic actor choice (this reading), autonomous technological affordance (technological_determinism), or mutual feedback between the two (co_constitution)?',
    'Comparative print-run economics, correspondence analysis between reformers and printers documenting deliberate format/timing choices, and counterfactual case comparison against regions/periods where similar press technology existed without comparable religious upheaval (e.g., earlier Wycliffite or Hussite print/manuscript efforts).',
    'If strategic intent dominates, this reading''s tangled_rope classification (beneficiaries choosing and deploying a tool, victims paying through the same structure) is the accurate structural account. If technology''s affordances were the binding constraint regardless of actor intent, the sibling technological_determinism reading''s mountain-leaning classification would be more accurate, with far lower authored extractiveness since no party ''weaponizes'' an inevitability. If feedback dominates, the co_constitution reading''s distinct classification applies and none of the three readings alone is complete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disaggregation, conceptual, 'Committer-frame ambiguity: which kernel reading (strategic_deployment vs technological_determinism vs co_constitution) best fits the historical record, and where the disagreement is structurally located.').

omega_variable(
    beneficiary_intent_vs_retrospective_attribution,
    'How much of the ''strategic weaponization'' framing is drawn from reformers'' and printers'' own contemporaneous self-descriptions of their tactics, versus retrospective historiographical attribution of coherent strategy onto what may have been more improvised, opportunistic, or locally contingent decisions?',
    'Close reading of printer correspondence, guild records, and reformer private letters (as distinct from public polemic) for explicit statements of tactical intent regarding format, pricing, and timing choices, cross-checked against modern book-history scholarship''s confidence intervals on intentionality claims.',
    'If intent is well-corroborated by contemporaneous private sources, the beneficiary/agenda_setter roles and the tangled_rope classification are on firm ground. If intent is largely retrospective narrative imposed by later historians, the beneficiary declarations still hold structurally (they did benefit) but the ''strategic'' framing itself would be overstated relative to a more contingent, co_constitution-style account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_intent_vs_retrospective_attribution, empirical, 'Whether documented strategic intent is contemporaneous or retrospectively constructed.').

omega_variable(
    reform_side_press_as_natural_or_constructed_advantage,
    'Was the reform coalition''s press advantage a constructed strategic asset they built and defended, or did it emerge naturally from pre-existing print-market economics (cheap vernacular pamphlets already outsold expensive Latin manuscripts for unrelated commercial reasons) that reformers merely rode?',
    'Market data on pre-Reformation vernacular versus Latin print volumes and pricing in the decades before 1517, to establish whether the vernacular-cheap-format advantage predates and is independent of reformist strategic choice.',
    'If the vernacular/cheap-format advantage predates reform strategy, part of what this reading calls ''strategic deployment'' may be better described as strategic exploitation of a pre-existing rope-type coordination structure (the vernacular print market) rather than construction of a wholly new weapon — softening but not eliminating the tangled_rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_side_press_as_natural_or_constructed_advantage, empirical, 'Whether the press''s reform-favorable properties were pre-existing market structure or reformer-constructed advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causality__strategic_deployment, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__strategic_deployment, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__strategic_deployment, theater_ratio, 1517, 0.15).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causality__strategic_deployment, theater_ratio, 1540, 0.25).
narrative_ontology:measurement(pres_tr_t1580, press_reformation_causality__strategic_deployment, theater_ratio, 1580, 0.3).
narrative_ontology:measurement(pres_tr_t1620, press_reformation_causality__strategic_deployment, theater_ratio, 1620, 0.32).
narrative_ontology:measurement(pres_tr_t1650, press_reformation_causality__strategic_deployment, theater_ratio, 1650, 0.28).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causality__strategic_deployment, base_extractiveness, 1450, 0.1).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__strategic_deployment, base_extractiveness, 1500, 0.22).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__strategic_deployment, base_extractiveness, 1517, 0.4).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causality__strategic_deployment, base_extractiveness, 1540, 0.55).
narrative_ontology:measurement(pres_be_t1580, press_reformation_causality__strategic_deployment, base_extractiveness, 1580, 0.6).
narrative_ontology:measurement(pres_be_t1620, press_reformation_causality__strategic_deployment, base_extractiveness, 1620, 0.58).
narrative_ontology:measurement(pres_be_t1650, press_reformation_causality__strategic_deployment, base_extractiveness, 1650, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causality__strategic_deployment, suppression_requirement, 1450, 0.1).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__strategic_deployment, suppression_requirement, 1500, 0.25).
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__strategic_deployment, suppression_requirement, 1517, 0.45).
narrative_ontology:measurement(pres_su_t1540, press_reformation_causality__strategic_deployment, suppression_requirement, 1540, 0.65).
narrative_ontology:measurement(pres_su_t1580, press_reformation_causality__strategic_deployment, suppression_requirement, 1580, 0.7).
narrative_ontology:measurement(pres_su_t1620, press_reformation_causality__strategic_deployment, suppression_requirement, 1620, 0.6).
narrative_ontology:measurement(pres_su_t1650, press_reformation_causality__strategic_deployment, suppression_requirement, 1650, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__strategic_deployment, 0.05).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causality kernel, decomposed per the ε-invariance principle because the natural-language label 'printing press caused the Reformation' conflates structurally distinct causal claims. strategic_deployment (this story) authors reformers/printers as intentional beneficiary-agents wielding print as a tangled-rope weapon against Rome (moderate-high, rising-then-plateauing ε). technological_determinism authors the press as an autonomous enabling mountain/rope with no strategic beneficiary structure (expected low, flat ε). co_constitution authors a feedback-loop account where technology and agency mutually shape outcomes, expected to sit structurally between the other two with its own distinct beneficiary/victim topology rather than an average of theirs. Each carries its own ε and classification; none is derived from the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
