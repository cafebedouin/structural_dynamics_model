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
 *   human_readable: Strategic Weaponization of Print Technology by Reformation Coalition
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint instantiates the STRATEGIC DEPLOYMENT reading of the
 *   press-Reformation kernel. It asserts that Reformation theologians and
 *   print entrepreneurs deliberately weaponized printing technology to
 *   displace Catholic institutional authority, extract legitimacy from Church
 *   gatekeepers, and capture ideological authority over lay readers. The
 *   constraint is CLAIMED as snare (pure extraction using strategic
 *   deployment) while the authored metrics describe the measured operation:
 *   high extractiveness (0.68 at interval end) reflecting the systematic
 *   displacement of Catholic authority monopoly; high suppression (0.76)
 *   reflecting the enforcement cost the Church must bear to respond to
 *   coordinated print strategy; moderate theater (0.42) reflecting that the
 *   coordination function (mass text distribution) is real but increasingly
 *   becomes a vehicle for extraction rather than neutral service. The
 *   temporal series captures escalation from 1440 (pre-print baseline)
 *   through the interval to 1600 (consolidation of reform institutional
 *   displacement). The measurement grid is shared across all three metrics at
 *   six time points spanning 160 years.
 *
 * KEY AGENTS:
 *   - Reform theologians: institutional agenda-setters directing print strategy to displace Church authority
 *   - Print entrepreneurs: powerful capital-holders profiting from print investment and control over textual reproduction; beneficiaries of expanded market
 *   - Catholic institutional authority: institutional payer bearing the cost of displacement and forced to build counter-apparatus (Index, seminaries, counter-polemic)
 *   - Pre-reformation scribal economy: moderate-power collateral victims whose livelihood is displaced by print obsolescence
 *   - Lay vernacular readers: powerless beneficiaries instrumentalized as the legitimacy base the coalition targets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.68).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.76).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, snare).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Weaponization of Print Technology by Reformation Coalition").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'e2256c9c-fa9f-42b0-aa3a-1238d92584fc').
narrative_ontology:cs_kernel_codification('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', fixed_text).
narrative_ontology:cs_authority_grounding('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', lineage).
narrative_ontology:cs_interpretation_layer_present('e2256c9c-fa9f-42b0-aa3a-1238d92584fc').
narrative_ontology:cs_reading_relation('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', foundational, human_agency_primacy_in_causation).
narrative_ontology:cs_axiom_status(human_agency_primacy_in_causation, holdable).
narrative_ontology:cs_axiom_grounding('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', human_agency_primacy_in_causation, deontological).
narrative_ontology:cs_axiom('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', foundational, intentional_weaponization_of_capacity).
narrative_ontology:cs_axiom_status(intentional_weaponization_of_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', intentional_weaponization_of_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', catholic_universal_textual_monopoly).
narrative_ontology:cs_drift_state('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', reformation_consolidation_1600, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('e2256c9c-fa9f-42b0-aa3a-1238d92584fc', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, reform_theologians).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, print_entrepreneurs).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_institutional_authority).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, pre_reformation_scribal_economy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, lay_vernacular_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and disseminate theological arguments against Catholic institutional authority. They identify printing as a strategic tool to reach vernacular audiences, deliberately commission press runs of polemics, heresy, and scripture in accessible languages, and actively cultivate relationships with printers and patrons who fund mass production. Their goal is institutional displacement of Catholic authority structures through ideological reach and legitimacy capture among lay populations.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, reform_theologians, agenda_setter,
    institutional, generational, mobile, continental).

% Invest capital in press infrastructure and control the means of textual reproduction. They profit from the high demand for reform materials, patron funding tied to religious controversy, and the emergence of a mass market for printed text. They make strategic choices about which manuscripts to accept, how many copies to produce, and where to distribute them—choices that amplify reformist messages and squeeze the scribal economy that Catholic institutional monopolies depended on.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, print_entrepreneurs, agenda_setter,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, print_entrepreneurs, beneficiary).

% Faces coordinated displacement of its textual authority monopoly and ideological hegemony. The Church's control over manuscript reproduction, scriptural interpretation, and clerical gatekeeping is systematically undermined by the press-theology coalition's strategy. The institution loses revenue from indulgence sales, loses monopoly legitimacy as vernacular scripture reaches lay readers directly, and must expend resources on suppression, polemical response, and enforcement. Its constrained exit reflects that a medieval institution cannot simply abandon institutional Christianity or retreat to pre-print models without ceasing to function.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_institutional_authority, payer,
    institutional, civilizational, constrained, continental).

% Monastery scriptoriums, professional copyists, and manuscript trade networks that depended on the scarcity and sacred value of hand-copied texts. The print press makes their labor-intensive production economically obsolete and undermines the mystique of the scribal object. Copyists cannot match print volume and cost, so their livelihood contracts. They are collateral displacement: they lose market position because printing technology executes the reformists' strategic goal more efficiently.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, pre_reformation_scribal_economy, payer,
    moderate, biographical, constrained, regional).

% Gain direct access to scripture, theology, and polemic in native languages for the first time. The press-theology coalition strategically targets them as the legitimacy base, so they receive vernacular materials at scale. They benefit from access to text and ideological agency—they are no longer gatekept by clerical monopoly. However, they are also the vehicle for the coalition's strategic objective, so their agency is instrumentalized: they are targeted as an audience to be won, not consulted as agents setting the coalition's goals.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, lay_vernacular_readers, beneficiary,
    powerless, biographical, mobile, continental).

% The Church's eventual response: investment in printing of Catholic polemic, the Index Librorum Prohibitorum (banned books list), censorship enforcement, theological seminaries, and mass-media engagement. From this seat, the strategic deployment becomes visible as the constraint requiring counter-enforcement. The apparatus emerges not as a natural response to print technology but as a reactive institutional adaptation to the fact that reformists weaponized the press strategically.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_counter_reformation_apparatus, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, reform_theologians).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Print technology coordinates the distribution of textual argument at scale, removing the labor bottleneck that once made text scarce and monolithically controlled. The coordination function is real: mass reproduction solves a genuine collective-action problem (how to reach many readers efficiently). This reading asserts that reformists and printers consciously deployed this coordination function as a weapon against Catholic monopoly.
% TRANSFER_FUNCTION: Transfers textual authority, ideological legitimacy, and economic rent from Catholic institutional gatekeepers to the reform coalition (theologians and printers). Transfers livelihood from the pre-print scribal economy to the print economy. Transfers access to scripture from clerical monopoly to lay vernacular readers. The constraint moves power, institutional authority, and economic opportunity, not direct monetary transfer—but money and patronage follow the shifted legitimacy.
% ABSENT_VOICES: Lay readers are present as beneficiaries but not as agents setting the coalition's agenda. Scribal workers are completely excluded—no seat for them to voice that their craft is being made obsolete. The technological determinist position (that the press itself drove the Reformation inevitably) is absent from this story's framing, though it remains live in contemporary historical debate. Pre-Reformation institutional defenders (theologians arguing for monastic authority, defenders of Latin monopoly) are silenced by the coordinated press assault and have no seat at the table that decides the constraint's shape.
% DISAPPEARANCE_RATIONALE: If the strategic print-theology coalition had not formed, the press technology would have continued to develop and exist (it is not magical—it is a tool), but it would not have been systematically weaponized to displace Catholic authority. The Reformation would have proceeded differently: without coordinated print strategy, religious argument would remain more localized, the reach of vernacular polemic would be smaller, and Catholic institutional authority would have longer to adapt or suppress emerging heterodoxy. The coalition's strategic choice to weaponize the press, not the press itself, is what reorganized Christendom.
% FOUNDING_PROBLEM: Catholic institutional monopoly over textual authority and scriptural interpretation excludes lay readers from direct engagement with sacred text and creates a bottleneck for religious innovation or criticism. Reformers face the problem that their arguments circulate slowly and remain confined to educated audiences; they cannot reach the mass base needed to displace institutional authority. The printing press offers a strategic solution: mass production at scale.
% FOUNDING_PROBLEM_CORROBORATION: Reformers' own letters, commission records, and correspondence with printers attest to the deliberate strategy: Luther's explicit instructions to printers about which works to publish, the coordination of publication dates across cities, the funding of multiple-language editions. Print historians (Eisenstein, Pettegree, McKitterick) and Reformation scholars outside the reform tradition document the coalition structure. Catholic counter-reformation records attest that the Church perceived print strategy as a coordinated threat, not as inevitable technology effects. The Archive of the Inquisition and Vatican records show conscious recognition that the Coalition's weaponization required institutional counter-apparatus.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness rises from near-zero at Gutenberg (1440) to 0.68 by 1600 because the constraint's operation is the cumulative displacement of Catholic textual monopoly. Early printing (1440–1470) is merely technological capacity; no extraction yet. The coalition's strategic deployment accelerates 1470–1530 (rising to 0.42) as reformers and printers coordinate vertically to flood markets with polemical and scriptural material in vernacular languages. The acceleration 1530–1570 (0.42→0.66) captures the consolidation phase: institutional displacement is now visible, the Church must invest heavily in counter-apparatus, Catholic institutional authority is ceding territory to reformed entities. The leveling 1570–1600 (0.66→0.68) reflects that by the end of the interval, reformed institutional structures are stable and Catholic response is systemized; the extraction reaches an equilibrium. Suppression rises in parallel (0.05→0.76) because maintaining the constraint requires active enforcement: the Church builds the Index, funding mechanisms to print counter-polemic, theological seminaries, and political alliances to suppress reformed print networks. At t=1600, suppression is near-maximal because the constraint's persistence now depends entirely on sustained institutional conflict, not on any lingering technological advantage or surprise. Theater ratio remains moderate (0.0→0.42) because the press never stops serving its coordination function—it genuinely distributes text efficiently—but the ratio rises because increasingly the constraint's persistence depends on defending market control and ideological monopoly rather than on the intrinsic efficiency of the technology. The claim/metric independence is maintained: the constraint is CLAIMED snare (weaponized deployment for institutional displacement) and the metrics descriptively show high extractiveness + high suppression + moderate theater, which is exactly what a snare measured in operation looks like. The engine decides whether the computation agrees.
 *
 * PERSPECTIVAL GAP:
 *   Reform coalition seats compute as beneficiary or agenda-setter; Catholic institutional seat computes as trapped target; lay readers compute as semi-beneficiary but instrumentalized. These divergences emerge from the authored structural data (power, exit, beneficiary/victim) without being asserted in the claimed_type field. The snare claim states that pure extraction + active enforcement is what the constraint structure is; the divergent computations for each seat should bear that out.
 *
 * DIRECTIONALITY LOGIC:
 *   The reform theologians are near-pure beneficiaries: they gain ideological authority, institutional displacement of their competitor, and reach to mass audiences (d near 0.0). Print entrepreneurs are also strong beneficiaries: they profit from scale-up of print production, gain market control, and extract rents from the shift to printed text (d near 0.1–0.2). Catholic institutional authority is the clear target: loses monopoly authority, loses revenue streams, must expend resources on suppression and counter-apparatus, and cannot exit without ceasing to be institutional Christianity (d near 0.95). The pre-reformation scribal economy is also a target despite not being directly attacked: they lose livelihood and market position because print technology executes the reformers' strategy (d near 0.8). Lay readers are near-symmetric: genuine coordination benefit (access to text) but instrumentalized as the legitimacy base (d near 0.5). No directionality overrides are needed: the beneficiary/victim + exit declarations produce accurate derivations. The constraining of Catholic institutional authority and the mobility of reform coalition members (who can relocate presses, find patrons in multiple cities) naturally drive the asymmetry without needing override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Catholic monopoly over textual authority excludes lay readers and constrains religious innovation) remains live throughout the interval. The constraint persists because the reform coalition strategically maintains enforcement (coordinating press investment, securing patron funding, directing print production toward ideological goals) to extract the benefit of displacement. Mandatrophy does not apply: the coordination function (mass text distribution) and the extraction function (displacement of Catholic monopoly) are not separable in this reading—they are the same action viewed from different seats. The constraint is not a zombie—it is a functioning snare that persists because the beneficiaries actively maintain it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_intent_vs_structural_necessity,
    'Did reformers and printers consciously strategize print deployment as a weapon against Catholic authority, or did they merely recognize and act on the press''s structural capacity to displace monopoly once it became available?',
    'Textual analysis of correspondence, commission records, and funding decisions: do reformers and printers explicitly state intention to weaponize the press for institutional displacement, or do they only discuss printing as a tool for spreading truth (with institutional displacement as a side effect they celebrate but did not originally intend)?',
    'If strategic intent is primary, the constraint is snare (weaponized extraction by coalition choice). If the coalition merely recognized and capitalized on structural opportunity, the constraint might reclassify toward tangled_rope (coordination with incidental extraction) or toward a reading influenced more by technological_determinism (the press''s structural capacity, not the coalition''s agency, drove the outcome). The sibling co_constitution reading would gain ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_intent_vs_structural_necessity, conceptual, 'Whether weaponization was deliberate coalition strategy or recognition of structural opportunity.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.76) structural—the press technology makes Catholic monopoly impossible to maintain regardless of enforcement effort—or internalized—Catholic authority accepts the loss of universal legitimacy and reformulates its identity within a pluralist Christian ecosystem?',
    'Post-Reformation institutional trajectory: does the Catholic Counter-Reformation represent continued enforcement of a dying monopoly (structural suppression persisting), or does it represent adaptive institutional reframing that internalizes loss of universal authority and reconstructs Catholic identity around regional power, clerical discipline, and counter-polemic (internalized suppression)?',
    'If suppression is entirely structural, the constraint is more clearly a snare maintained by external pressure alone. If suppression is substantially internalized, Catholic institutional authority itself has absorbed the constraint''s legitimacy framework and becomes partly complicit in maintaining the pluralist ecosystem that displaced its universal monopoly. This would lower the effective suppression and suggest reclassification toward tangled_rope (Catholic authority as unwilling partner in coordination) or suggest that mandatrophy is partially resolved (the founding problem''s solution was accepted, even if costly).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Structural vs. internalized suppression in Catholic institutional response to Reformation displacement.').

omega_variable(
    lay_reader_instrumentalization,
    'Are lay vernacular readers beneficiaries (gaining genuine access and ideological agency) or instrumentalized tools of the reform coalition (targeted as a legitimacy base without voice in coalition agenda)?',
    'Textual analysis of reformation-era vernacular materials: do they address lay readers as agents with capacity for judgment and interpretation, or as audiences to be persuaded and controlled? Institutional history of reformed churches: do they grant lay readers institutional voice in governance decisions, or do they reconstitute clerical authority with reformed clergy replacing Catholic clergy?',
    'If lay readers are genuine beneficiaries with agency, the constraint is more clearly snare (extraction from Catholic authority via displacement) with distributed benefit. If lay readers are instrumentalized, the constraint is snare (extraction from both Catholic authority and lay readers, with benefit concentrated in the reform coalition). The reading remains strategically deployed, but the victim set expands to include the lay reader base.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_reader_instrumentalization, empirical, 'Whether lay vernacular readers gain agency or are instrumentalized by the reform coalition.').

omega_variable(
    kernel_frame_under_determination,
    'Which reading of the press-Reformation kernel—strategic_deployment (this constraint), technological_determinism, or co_constitution—best explains the historical record?',
    'The three readings coexist as live interpretive positions in contemporary Reformation scholarship. No single reading has become canonical. Each reading is defended by respected scholars and each frames different evidence as primary. Resolution depends on the investigator''s stance on human agency primacy vs. technological capacity vs. feedback-loop co-constitution.',
    'If technological_determinism gains ground, the classification shifts toward rope or mountain (the press''s structural capacity makes mass reproduction inevitable once the tool exists, regardless of human strategy). If co_constitution gains ground, the classification shifts toward tangled_rope (feedback between print economy and religious controversy, neither side fully controlling the outcome). This reading remains valid as a live scholarly position but would be contextualized within a family of constraint stories, each a reading of the same kernel with different structural implications.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_frame_under_determination, conceptual, 'The kernel itself is under-determined: different readings of press-Reformation causality remain live in contemporary scholarship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1440, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causality__strategic_deployment, theater_ratio, 1440, 0.0).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causality__strategic_deployment, theater_ratio, 1470, 0.12).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causality__strategic_deployment, theater_ratio, 1500, 0.24).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causality__strategic_deployment, theater_ratio, 1530, 0.35).
narrative_ontology:measurement(pres_tr_t1570, press_reformation_causality__strategic_deployment, theater_ratio, 1570, 0.4).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causality__strategic_deployment, theater_ratio, 1600, 0.42).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causality__strategic_deployment, base_extractiveness, 1440, 0.08).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causality__strategic_deployment, base_extractiveness, 1470, 0.22).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causality__strategic_deployment, base_extractiveness, 1500, 0.42).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causality__strategic_deployment, base_extractiveness, 1530, 0.61).
narrative_ontology:measurement(pres_be_t1570, press_reformation_causality__strategic_deployment, base_extractiveness, 1570, 0.66).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causality__strategic_deployment, base_extractiveness, 1600, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1440, press_reformation_causality__strategic_deployment, suppression_requirement, 1440, 0.05).
narrative_ontology:measurement(pres_su_t1470, press_reformation_causality__strategic_deployment, suppression_requirement, 1470, 0.18).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causality__strategic_deployment, suppression_requirement, 1500, 0.35).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causality__strategic_deployment, suppression_requirement, 1530, 0.58).
narrative_ontology:measurement(pres_su_t1570, press_reformation_causality__strategic_deployment, suppression_requirement, 1570, 0.71).
narrative_ontology:measurement(pres_su_t1600, press_reformation_causality__strategic_deployment, suppression_requirement, 1600, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, resource_allocation).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__strategic_deployment, 0.18).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% The press-Reformation kernel decomposes into three readings, each a distinct constraint with different epsilon values and stakeholder structures. The technological_determinism reading emphasizes the press's autonomous enabling capacity (mountain-leaning, low extractiveness from anyone—the press simply makes scale possible). The co_constitution reading emphasizes feedback loops neither reformers nor the press controlled fully (tangled_rope, moderate extractiveness, shared agency). The strategic_deployment reading (this constraint) emphasizes deliberate coalition choice to weaponize print for institutional displacement (snare, high extractiveness, concentrated beneficiary agency). Each reading has its own beneficiary/victim structure, its own claim, and its own metrics. They are linked as siblings of a single kernel, not as alternative framings of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
