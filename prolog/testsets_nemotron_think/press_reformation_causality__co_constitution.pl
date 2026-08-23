% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Reformation Co-Constitutive Feedback Loops
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The co_constitution reading of press_reformation_causality holds that
 *   printing technology and human agency (reformers, printers, readers,
 *   authorities) mutually shaped each other through feedback loops between
 *   the print economy and religious controversy. The printing press did not
 *   simply 'cause' the Reformation (technological_determinism), nor was it
 *   merely a tool strategically deployed by reformers (strategic_deployment).
 *   Instead, the economics of print (fixed costs, marginal returns, network
 *   effects) and the dynamics of religious controversy (vernacular demand,
 *   confessional competition, censorship) co-evolved: print made mass
 *   vernacular distribution economically viable, which expanded the market
 *   for religious texts, which funded more print capacity, which accelerated
 *   doctrinal fragmentation, which increased demand for polemical print. This
 *   constraint story models that co-constitutive dynamic as a tangled_rope:
 *   genuine coordination (text reproduction, cross-territorial communication,
 *   confessional identity formation) intertwined with asymmetric extraction
 *   (printers' monopoly rents, reformers' ideological capture, state
 *   licensing revenue, Church's lost authority).
 *
 * KEY AGENTS:
 *   - printers_publishers: Primary beneficiaries (economic rents) and agenda_setters (production control) — institutional/organized power, arbitrage exit, continental scope
 *   - reformers: Beneficiaries (ideological spread) and payers (persecution risk) — organized power, identity_locked exit, continental scope
 *   - catholic_church_hierarchy: Victims (doctrinal monopoly loss) and agenda_setters (censorship enforcement) — institutional power, trapped exit, universal scope
 *   - vernacular_readers: Beneficiaries (access) and payers (cost/risk) — moderate power, constrained exit, regional scope
 *   - state_authorities: Beneficiaries (licensing revenue, confessional control) and agenda_setters (print regulation) — institutional power, mobile exit, national scope
 *   - traditional_scribes: Victims (displacement) — powerless, trapped exit, local scope
 *   - unlicensed_printers: Victims (suppression) and payers (legal risk) — moderate power, constrained exit, regional scope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.58).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.62).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Reformation Co-Constitutive Feedback Loops").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '10e52211-414b-4dd9-bab0-6a302c12932b').
narrative_ontology:cs_kernel_codification('10e52211-414b-4dd9-bab0-6a302c12932b', distributed).
narrative_ontology:cs_authority_grounding('10e52211-414b-4dd9-bab0-6a302c12932b', practice).
narrative_ontology:cs_interpretation_layer_present('10e52211-414b-4dd9-bab0-6a302c12932b').
narrative_ontology:cs_reading_relation('10e52211-414b-4dd9-bab0-6a302c12932b', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('10e52211-414b-4dd9-bab0-6a302c12932b', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('10e52211-414b-4dd9-bab0-6a302c12932b', foundational, technology_and_agency_mutually_constitutive).
narrative_ontology:cs_axiom_status(technology_and_agency_mutually_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('10e52211-414b-4dd9-bab0-6a302c12932b', technology_and_agency_mutually_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('10e52211-414b-4dd9-bab0-6a302c12932b', secondary, feedback_loops_drive_historical_outcomes).
narrative_ontology:cs_axiom_status(feedback_loops_drive_historical_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('10e52211-414b-4dd9-bab0-6a302c12932b', feedback_loops_drive_historical_outcomes, empirically_contingent).
narrative_ontology:cs_reference_frame('10e52211-414b-4dd9-bab0-6a302c12932b', co_constitutive_feedback_loops).
narrative_ontology:cs_drift_state('10e52211-414b-4dd9-bab0-6a302c12932b', contemporary_digital_analogy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('10e52211-414b-4dd9-bab0-6a302c12932b', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printers_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, vernacular_readers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, state_authorities).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_church_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, traditional_scribes).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, unlicensed_printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, reformers).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, vernacular_readers).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, vernacular_scripture_access).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, decentralized_text_reproduction).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, confessional_identity_formation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the production and distribution of printed texts across European trade networks. Set prices, negotiate privileges with authorities, and determine which texts get printed. Collect monopoly rents from high-demand religious texts (Bibles, polemics, indulgences). Can relocate presses to favorable jurisdictions (Basel, Geneva, Antwerp, Venice) when local suppression intensifies. Their economic interest aligns with maximum text volume and confessional pluralism.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printers_publishers, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, printers_publishers, beneficiary).

% Luther, Calvin, Zwingli and their networks depend on print to propagate vernacular scripture and polemic beyond local preaching. Gain unprecedented ideological reach but bear persecution risk (excommunication, imperial ban, execution). Cannot exit the theological commitment that defines their identity — the constraint is fused with their vocation. Depend on printer networks they do not control; negotiate favorable terms but remain structurally dependent on print infrastructure.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reformers, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reformers, payer).

% Loses doctrinal monopoly and Latin-literacy control as vernacular print spreads. Responds with Index Librorum Prohibitorum (1559), Inquisition censorship, and Council of Trent decrees on printing. The censorship apparatus becomes a self-justifying bureaucracy extracting resources from the faithful while failing to stop Protestant print. Institutional identity is constituted through doctrinal unity — cannot exit the constraint without dissolving the institution. Bears both the extraction (lost authority) and the enforcement cost (censorship machinery).
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_church_hierarchy, agenda_setter,
    institutional, civilizational, trapped, universal).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, catholic_church_hierarchy, payer).

% Gain access to affordable vernacular Bibles, catechisms, and devotional texts previously unavailable. This access reshapes piety, enables lay interpretation, and fuels confessional identity. But they pay higher prices for licensed texts, bear risk of possessing prohibited books, and are mobilized into confessional conflicts they did not choose. Exit is constrained by regional confessional enforcement (cuius regio, eius religio) and literacy barriers.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, vernacular_readers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, vernacular_readers, payer).

% Imperial, territorial, and city governments grant printing privileges, collect licensing fees, and enforce confessional uniformity (or tolerate pluralism for economic gain). The print trade becomes a revenue source and a tool of state-building. Can shift policy as confessional politics evolve — mobile exit at the institutional level. Their enforcement of printing regulations is the active suppression that sustains the constraint's tangled_rope structure.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, state_authorities, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, state_authorities, beneficiary).

% Manuscript production collapses as print undercuts price and speed. Scribal guilds lose livelihood with no alternative employment for their specialized skills. No organized resistance capacity; individual exit means abandoning craft identity. Displacement is near-total in major print centers within two generations.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, traditional_scribes, payer,
    powerless, immediate, trapped, local).

% Operate outside the privilege system — printing prohibited texts, evading censors, serving underground markets. Face confiscation, fines, imprisonment, and exile. Their suppression is the enforcement mechanism that maintains the licensed printers' monopoly and the authorities' control. Some eventually gain licenses (co-opted); others persist as a shadow print economy. Exit is constrained by capital requirements and geographic mobility.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, unlicensed_printers, payer,
    moderate, biographical, constrained, regional).

% Widows and daughters frequently inherited and ran print shops (e.g., Katharina von Bora's press, Charlotte Guillard in Paris), but guild structures and university statutes barred them from formal recognition. Their labor and capital sustained the print economy while their voices were excluded from authorship, editorial control, and privilege-granting. Would object to the gendered extraction but had no structural seat at the table.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, women_print_workers, excluded,
    powerless, biographical, trapped, local).

% Hebrew printing flourished in Venice, Prague, and Poland but operated under distinct censorship (Church expurgation, state licensing). Jewish readers gained access to standardized liturgy and halakhic texts, but the print economy extracted via monopoly privileges granted to Christian printers. Their exclusion from the main confessional print market created a parallel constrained economy. Would object to the Christian-confessional framing of the constraint but were structurally outside it.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, jewish_communities, excluded,
    powerless, generational, constrained, continental).

% Produce the historiography that frames the press-reformation relationship. The co_constitution reading (Eisenstein, Febvre-Martin, Scribner, Pettegree) competes with technological_determinism and strategic_deployment. Scholars do not collect rents from the historical constraint but their interpretations shape contemporary analogies (digital media, platform regulation). Their analytical seat computes the classification from the structural data authored here.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, printers_publishers).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The print economy solved the coordination problem of reproducible, standardized text distribution across linguistic and political boundaries: a single press run could supply thousands of identical copies, enabling vernacular scripture, synchronized polemic, and confessional identity formation at scale. Religious controversy solved the demand-side coordination: confessional competition created sustained, networked demand for print that financed the infrastructure.
% TRANSFER_FUNCTION: The arrangement transfers: (1) Economic value — from book buyers (laity, churches, states) to printers/publishers via monopoly pricing on privileged texts; (2) Doctrinal authority — from Catholic magisterium to reformers and vernacular readers via uncontrolled scripture access; (3) Licensing revenue — from printers to state authorities via privilege fees and censorship fines; (4) Literacy/epistemic access — from Latin-clergy monopoly to vernacular laity, with confessional elites capturing the new interpretive authority.
% ABSENT_VOICES: Women (print workers, readers, patrons) excluded from authorship and guild governance; Jewish communities operating under parallel but subordinate print regimes; peasant oral culture displaced by textual standardization; non-European societies encountering print through missionary imposition rather than indigenous adoption; radical reformers (Anabaptists, Spiritualists) suppressed by both magisterial Protestants and Catholics — their print networks were the most aggressively censored.
% DISAPPEARANCE_RATIONALE: If the co-constitutive feedback loops vanished overnight in 1520: vernacular scripture distribution would revert to manuscript (slow, expensive, error-prone); confessional identities would fail to crystallize across territories; the print trade would lose its primary demand driver and collapse to humanist/scholarly niche; state licensing systems would lose their enforcement object; the entire confessional map of early modern Europe would be unrecognizable. The constraint is constitutive of the historical outcome.
% FOUNDING_PROBLEM: How to coordinate religious reform across linguistic, political, and institutional boundaries without a centralized ecclesiastical monopoly on text reproduction? How to make text reproduction economically viable at scale when fixed costs are high and demand is fragmented?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (coordinating reform without centralized text control) is historically dead: Westphalia (1648) settled the confessional map, and the print trade matured into a stable commercial sector. Corroboration from OUTSIDE the beneficiary historians: economic historians of the book trade (Febvre-Martin, Plantin-Moretus archives) confirm the print economy's commercial logic became self-sustaining independent of religious controversy by 1600; historians of censorship (Darnton, Martin) document the Church's censorship apparatus persisting long after its doctrinal effectiveness ended — a classic mandatrophy signal. No beneficiary group (printers, reformers, states) claims the original founding problem remains live; they defend the evolved arrangement on new grounds (copyright, public order, market efficiency).
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__co_constitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58) reflects distributed but substantial transfers: economic (printer profits, state licenses), ideological (Church authority to reformers), and epistemic (Latin monopoly to vernacular access). Suppression (0.62) peaks during the Index/Inquisition era (1550s) as Church and states actively enforce printing privileges and doctrinal conformity. Theater ratio (0.28) is moderate: censorship rites and imprint colophons perform regulatory legitimacy while actual control shifts to economic and network dynamics. Accessibility collapse (0.45) is partial — manuscript culture persisted, oral transmission continued, and regional variation remained high. Resistance (0.71) is strong: from Church counter-reformation, from competing printers, from authorities resisting confessional fragmentation. The claimed tangled_rope type fits: coordination (vernacular public sphere, print trade networks) AND extraction (monopoly rents, ideological capture, licensing fees) WITH active enforcement (privileges, Index, Inquisition, imperial mandates).
 *
 * PERSPECTIVAL GAP:
 *   Printers experience the constraint as rope (coordination infrastructure they build and profit from). Reformers experience it as scaffold (enabling infrastructure for a transitional phase of confessional formation). Church hierarchy experiences it as snare (extraction of their doctrinal monopoly via enforced censorship that fails). Vernacular readers experience it as rope (genuine access gain with diffuse costs). State authorities experience it as tangled_rope (coordination of confessional order + extraction via licensing). The engine computes these seat divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Printers and state authorities are structural beneficiaries (d~0.15-0.25): they collect rents and control production rules. Reformers are near-symmetric (d~0.5): they gain ideological reach but bear persecution risk and depend on printer networks. Catholic Church hierarchy is a high-target (d~0.85): loses monopoly authority, bears censorship costs, cannot exit its institutional identity. Traditional scribes and unlicensed printers are high-targets (d~0.9): displaced or suppressed with minimal alternatives. Vernacular readers are mild beneficiaries (d~0.3): gain access but pay prices and bear confessional conflict costs. Exit options modulate these: printers have arbitrage (move cities), reformers are identity_locked (theological commitment), Church is trapped (institutional continuity), readers are constrained (regional markets).
 *
 * MANDATROPHY ANALYSIS:
 *   The printing press as technology was a scaffold (enabling infrastructure with sunset — the incunabula period transitions to mature print trade). But the co-constitutive feedback loop itself became a tangled_rope: the coordination function (vernacular text circulation) never 'completed' into a steady state; instead it stabilized as an extractive equilibrium where printers, states, and confessional establishments all captured rents from the ongoing controversy. No party benefits enough to dismantle the system, and no party is hurt enough to overthrow it — classic mandatrophy where the founding problem (how to coordinate reform across boundaries) is dead (Westphalia settled the confessional map) but the arrangement persists in evolved form (modern publishing, media regulation, platform dynamics).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'This constraint is the co_constitution reading of the press_reformation_causality kernel. How does the structural relationship to sibling readings (technological_determinism, strategic_deployment) affect classification?',
    'Compare epsilon and beneficiary/victim structures across all three readings. If technological_determinism shows near-zero extraction (mountain) and strategic_deployment shows concentrated extraction (snare), the co_constitution reading''s distributed extraction pattern (tangled_rope) is validated as a distinct structural claim.',
    'If sibling readings collapse into the same structural type, the kernel''s contestation is rhetorical not structural. If they remain distinct, the kernel decomposition is warranted per ε-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Kernel-reading structural differentiation across the press_reformation_causality family').

omega_variable(
    extraction_frame_ambiguity,
    'Does ''extraction'' appropriately describe ideological spread and confessional identity formation, or does it impose an economic metaphor on theological dynamics?',
    'Test whether the engine''s extraction metric captures non-material transfers (authority, legitimacy, salvation-anxiety) equivalently to material transfers. Compare classification outcomes with and without ideological transfers counted as extraction.',
    'If ideological transfers register as extraction, the tangled_rope classification holds. If they register as coordination-only, the constraint may be a rope or scaffold instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_frame_ambiguity, conceptual, 'Whether non-material transfers constitute extraction in the co-constitution dynamic').

omega_variable(
    temporal_boundary_indeterminacy,
    'Where does the co-constitutive constraint begin and end? Gutenberg (1450)? Luther (1517)? Peace of Westphalia (1648)?',
    'Measure extractiveness and suppression at candidate boundary points. A genuine constraint shows metric continuity across its interval; arbitrary boundaries produce metric discontinuities.',
    'Boundary choice changes the measured epsilon trajectory and may shift classification (e.g., early period more scaffold-like, late period more piton-like).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_boundary_indeterminacy, empirical, 'Historical periodization of the press-reformation feedback loop').

omega_variable(
    church_dual_role,
    'The Catholic Church is both victim (loss of doctrinal monopoly) and agenda_setter (censorship apparatus). Does this dual position create a single seat with contradictory directionality, or two distinct seats?',
    'Model Church hierarchy and Church censorship apparatus as separate stakeholders. If their directionality values diverge significantly (hierarchy d~0.8, censorship d~0.3), they are distinct seats. If similar, a single seat with internal tension.',
    'Two seats means the constraint extracts from the Church hierarchy while subsidizing its censorship apparatus — a within-agent tangled_rope. One seat means the Church''s net position determines its classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(church_dual_role, empirical, 'Whether the Church''s victim and enforcer roles constitute one or two structural positions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 1450, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reformation_co_constitution_tr_t1450, press_reformation_causality__co_constitution, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(press_reformation_co_constitution_tr_t1500, press_reformation_causality__co_constitution, theater_ratio, 1500, 0.12).
narrative_ontology:measurement(press_reformation_co_constitution_tr_t1517, press_reformation_causality__co_constitution, theater_ratio, 1517, 0.22).
narrative_ontology:measurement(press_reformation_co_constitution_tr_t1550, press_reformation_causality__co_constitution, theater_ratio, 1550, 0.31).
narrative_ontology:measurement(press_reformation_co_constitution_tr_t1600, press_reformation_causality__co_constitution, theater_ratio, 1600, 0.29).
narrative_ontology:measurement(press_reformation_co_constitution_tr_t1650, press_reformation_causality__co_constitution, theater_ratio, 1650, 0.28).

% Extraction over time
narrative_ontology:measurement(press_reformation_co_constitution_be_t1450, press_reformation_causality__co_constitution, base_extractiveness, 1450, 0.15).
narrative_ontology:measurement(press_reformation_co_constitution_be_t1500, press_reformation_causality__co_constitution, base_extractiveness, 1500, 0.32).
narrative_ontology:measurement(press_reformation_co_constitution_be_t1517, press_reformation_causality__co_constitution, base_extractiveness, 1517, 0.48).
narrative_ontology:measurement(press_reformation_co_constitution_be_t1550, press_reformation_causality__co_constitution, base_extractiveness, 1550, 0.61).
narrative_ontology:measurement(press_reformation_co_constitution_be_t1600, press_reformation_causality__co_constitution, base_extractiveness, 1600, 0.59).
narrative_ontology:measurement(press_reformation_co_constitution_be_t1650, press_reformation_causality__co_constitution, base_extractiveness, 1650, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(press_reformation_co_constitution_su_t1450, press_reformation_causality__co_constitution, suppression_requirement, 1450, 0.25).
narrative_ontology:measurement(press_reformation_co_constitution_su_t1500, press_reformation_causality__co_constitution, suppression_requirement, 1500, 0.45).
narrative_ontology:measurement(press_reformation_co_constitution_su_t1517, press_reformation_causality__co_constitution, suppression_requirement, 1517, 0.72).
narrative_ontology:measurement(press_reformation_co_constitution_su_t1550, press_reformation_causality__co_constitution, suppression_requirement, 1550, 0.78).
narrative_ontology:measurement(press_reformation_co_constitution_su_t1600, press_reformation_causality__co_constitution, suppression_requirement, 1600, 0.65).
narrative_ontology:measurement(press_reformation_co_constitution_su_t1650, press_reformation_causality__co_constitution, suppression_requirement, 1650, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, resource_allocation).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__co_constitution, 0.12).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, print_technology_as_scaffold).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, confessionalization_as_piton).

% DUAL FORMULATION NOTE:
% The press_reformation_causality kernel decomposes into three constraint stories: (1) technological_determinism — printing as Mountain (epsilon ~0.05, emerges_naturally); (2) strategic_deployment — reformers/printers as Snare/Tangled Rope with concentrated beneficiaries; (3) co_constitution — this story, distributed feedback loops as Tangled Rope with distributed extraction. The decomposition follows ε-invariance: each reading authors a different epsilon on a different structural referent. The technological_determinism reading's referent is the press as physical technology; the strategic_deployment reading's referent is the strategic actor-network; this reading's referent is the co-evolutionary dynamic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__co_constitution, institutional, 0.85).
constraint_indexing:directionality_override(press_reformation_causality__co_constitution, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
