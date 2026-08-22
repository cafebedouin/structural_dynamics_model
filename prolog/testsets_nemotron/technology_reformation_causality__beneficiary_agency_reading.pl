% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)
 *   domain: historical/religious/technological
 *
 * SUMMARY:
 *   This constraint story models the 'beneficiary agency reading' of the
 *   print-Reformation nexus: reformers and printers formed a tactical
 *   coalition that strategically deployed printing technology to bypass
 *   Church authority. Technology is a scaffold — a temporary support for the
 *   coalition's authority bypass — not the cause. The coalition is a tangled
 *   rope: reformers gain ideological authority and institutional legitimacy;
 *   printers gain market share, profits, and political protection; both
 *   extract from the Church's doctrinal monopoly and from the laity's
 *   cognitive sovereignty (which is partially transferred but also
 *   contested). The Church is trapped; the populace is trapped; manuscript
 *   producers are displaced. The ε of 0.58 derives from the value of the
 *   authority bypass — what the coalition gains by circumventing the Church's
 *   suppression apparatus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.58).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.42).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition Authority Bypass (Beneficiary Agency Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "historical/religious/technological").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, '63e408ba-b2be-4f45-98cb-5cdd787b24a2').
narrative_ontology:cs_kernel_codification('63e408ba-b2be-4f45-98cb-5cdd787b24a2', implicit).
narrative_ontology:cs_authority_grounding('63e408ba-b2be-4f45-98cb-5cdd787b24a2', practice).
narrative_ontology:cs_interpretation_layer_present('63e408ba-b2be-4f45-98cb-5cdd787b24a2').
narrative_ontology:cs_reading_relation('63e408ba-b2be-4f45-98cb-5cdd787b24a2', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('63e408ba-b2be-4f45-98cb-5cdd787b24a2', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('63e408ba-b2be-4f45-98cb-5cdd787b24a2', foundational, human_agency_primary_over_technological_determinism).
narrative_ontology:cs_axiom_status(human_agency_primary_over_technological_determinism, holdable).
narrative_ontology:cs_axiom_grounding('63e408ba-b2be-4f45-98cb-5cdd787b24a2', human_agency_primary_over_technological_determinism, deontological).
narrative_ontology:cs_axiom('63e408ba-b2be-4f45-98cb-5cdd787b24a2', foundational, technology_as_deployable_instrument_not_autonomous_force).
narrative_ontology:cs_axiom_status(technology_as_deployable_instrument_not_autonomous_force, holdable).
narrative_ontology:cs_axiom_grounding('63e408ba-b2be-4f45-98cb-5cdd787b24a2', technology_as_deployable_instrument_not_autonomous_force, empirically_contingent).
narrative_ontology:cs_reference_frame('63e408ba-b2be-4f45-98cb-5cdd787b24a2', pre_print_discursive_enclosure).
narrative_ontology:cs_drift_state('63e408ba-b2be-4f45-98cb-5cdd787b24a2', post_westphalia_confessional_settlement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('63e408ba-b2be-4f45-98cb-5cdd787b24a2', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reform_movement_leaders).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, commercial_printers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, vernacular_literacy_advocates).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, catholic_ecclesiastical_authority).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, traditional_manuscript_producers).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, populace_subject_to_ideological_contest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Luther, Calvin, Zwingli and their lieutenants strategically deploy printing to bypass Church censorship and distribute vernacular theology. They gain ideological authority, institutional legitimacy, and material support from printers. Their exit is identity-locked — their vocation and self-concept are fused with the reform movement they lead. They cannot exit without ceasing to be who they are.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, reform_movement_leaders, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, reform_movement_leaders, beneficiary).

% Printers in Wittenberg, Basel, Strasbourg, Geneva, and Antwerp produce reform pamphlets and vernacular Bibles at scale. They profit from unprecedented demand, gaining market share and political protection from reforming magistrates. Their exit is constrained — they have invested in presses, type, and distribution networks specific to this trade; switching costs are high but not identity-total.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, commercial_printers, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, commercial_printers, agenda_setter).

% The Roman Curia, local bishoprics, and the Inquisition lose control over doctrinal dissemination, face revenue loss from indulgence sales, and must expend enormous resources on censorship (Index Librorum Prohibitorum, 1559) and counter-reformation. Their exit is trapped — the institution cannot abandon its claim to doctrinal authority without dissolving its raison d'être.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, catholic_ecclesiastical_authority, payer,
    institutional, civilizational, trapped, continental).

% Scribes, illuminators, and manuscript booksellers are displaced by print economics. Their skills are devalued, their guilds lose monopoly privileges, and they lack the capital to transition to printing. Exit is constrained by age, skill specificity, and local economic embedment.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, traditional_manuscript_producers, payer,
    powerless, biographical, constrained, local).

% Humanist educators and civic reformers who championed lay literacy before the Reformation gain institutional adoption of their curricula, state funding for schools, and cultural prestige. Their exit is mobile — their intellectual project is portable across regimes and they can align with whoever advances vernacular education.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, vernacular_literacy_advocates, beneficiary,
    moderate, generational, mobile, continental).

% Ordinary people in German lands, Switzerland, and the Low Countries face coerced confessional choice (cuius regio, eius religio), social disruption, iconoclasm, and warfare. They bear the extraction of the reformer-printer coalition's authority bypass without meaningful exit — migration is blocked by feudal bonds, poverty, and the confessionalization of neighboring territories.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, populace_subject_to_ideological_contest, payer,
    powerless, biographical, trapped, local).

% Scholars (Eisenstein, Febvre & Martin, Pettegree, Rubin) who analyze the print-reformation nexus from outside the contest. They observe the strategic deployment, the mutual extraction, and the scaffold function of print technology without participating in the ideological struggle.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, historical_sociologists_of_media, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press solved a genuine coordination problem for the reform movement: how to disseminate a coherent theological challenge across fragmented political territories faster than the Church could suppress it locally. Printers provided the production capacity; reformers provided the content and the distribution networks (colporteurs, peddlers, sympathetic merchants).
% TRANSFER_FUNCTION: The arrangement moves three things: (1) ideological authority from Catholic hierarchy to reformer-printer coalition; (2) revenue from indulgence sales and manuscript production to commercial printers producing vernacular Bibles and polemical pamphlets; (3) cognitive sovereignty from clerical mediation to lay readers (via vernacular scripture), but this transfer is contested and partial.
% ABSENT_VOICES: Women printers and distributors (e.g., Katharina von Bora's role in Luther's household press operations, women colporteurs) are structurally excluded from the historical record of the coalition. Anabaptist and radical reformers who used print but were suppressed by magisterial reformers are excluded from the 'mainstream' coalition's benefits. Jewish communities in print centers (Venice, Prague, Basel) subject to censorship and expulsion are absent from the beneficiary calculus.
% DISAPPEARANCE_RATIONALE: If the reformer-printer coalition's strategic deployment of print vanished overnight (e.g., Luther's 95 Theses never printed, no vernacular Bible projects funded by printers), the Reformation as a mass movement fails. The Church retains doctrinal monopoly; manuscript culture persists longer; the Thirty Years' War likely does not occur in its historical form; the trajectory of European state formation and literacy is fundamentally altered.
% FOUNDING_PROBLEM: The Catholic Church's monopoly on Latinate theological discourse and sacramental mediation blocked reform-minded theologians from reaching lay audiences, while the indulgence system extracted wealth from the laity under doctrinal pretenses that reformers judged fraudulent. The founding problem was how to break this discursive and economic enclosure.
% FOUNDING_PROBLEM_CORROBORATION: Catholic counter-reformation sources (Council of Trent sessions, Jesuit correspondence) attest the Church recognized the founding problem as real and dangerous. Independent economic historians (Febvre & Martin, Pettegree) corroborate the enclosure thesis from outside the reform movement. The reformers' own polemical self-justification is not the sole attestation.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.58) is substantial but not total: the coalition extracts authority and revenue, but also delivers genuine coordination (vernacular dissemination, literacy expansion). Suppression (0.42) is moderate: the Church's censorship apparatus is real but porous; the coalition's own suppression of radical reformers and Catholic dissent is active. Theater ratio (0.31) reflects that the coalition's ideological rhetoric (sola scriptura, priesthood of all believers) increasingly masks the material interests of printer profits and magisterial state-building. Accessibility collapse (0.38) is incomplete: manuscript culture, oral transmission, and Catholic print persist alongside the coalition's output. Resistance (0.67) is high: the Church mounts the Index, the Inquisition, the Council of Trent, and military suppression; radical reformers resist magisterial co-optation.
 *
 * PERSPECTIVAL GAP:
 *   The reformer-printer coalition experiences the constraint as rope (genuine coordination against a corrupt monopoly). The Church experiences it as snare (extraction of its authority and revenue). The populace experiences it as tangled rope (vernacular access gained, but confessional coercion imposed). The engine computes this seat divergence from the structural data authored here.
 *
 * DIRECTIONALITY LOGIC:
 *   Reform leaders are identity-locked agenda-setters who benefit ideologically and institutionally (d ≈ 0.15). Printers are constrained beneficiaries who profit materially (d ≈ 0.25). The Church is a trapped institutional payer (d ≈ 0.95). Manuscript producers are constrained powerless victims (d ≈ 0.75). The populace is trapped powerless victims (d ≈ 0.90). Literacy advocates are mobile beneficiaries (d ≈ 0.20). The analytical observer sits at d = 0.50. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold function (print as temporary support for authority bypass) is real but the coalition never declared a sunset. The mandate (break the Church's discursive enclosure) was achieved by ~1555 (Peace of Augsburg), but the extraction machinery (printer privileges, state churches, confessional censorship) persisted and intensified. The constraint did not dissolve — it metastasized into confessional state apparatuses. This is mandatrophy: the founding problem is dead/contested but the arrangement persists with amplified extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    print_causality_vs_instrumentality,
    'Is the printing press a necessary condition for the Reformation''s scale and speed, or merely an accelerant of a movement that would have found other channels (preaching, manuscript networks, university disputations)?',
    'Counterfactual modeling using network diffusion simulations on known pre-print communication networks (Lollard, Hussite, humanist correspondence networks) calibrated against print-era diffusion data.',
    'If necessary condition, the technology has more causal weight and the ''scaffold'' claim weakens toward ''rope''. If mere accelerant, the ''beneficiary agency'' reading is strongly vindicated and the coalition''s extraction is more clearly strategic choice than structural inevitability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(print_causality_vs_instrumentality, conceptual, 'The causal status of print technology in the Reformation''s emergence.').

omega_variable(
    coalition_extraction_boundary,
    'Where does the reformer-printer coalition''s genuine coordination function end and its asymmetric extraction begin? Is vernacular scripture distribution a coordination good or an extraction mechanism (e.g., establishing new clerical monopolies over interpretation)?',
    'Comparative analysis of printer contracts, censorship records, and consistorial discipline in Geneva, Wittenberg, and Zurich — measuring lay access to print vs. clerical control of interpretation.',
    'If coordination dominates, the constraint trends toward rope. If extraction dominates (new clerical control replaces old), it trends toward snare. The tangled_rope classification hangs on this boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_extraction_boundary, empirical, 'The coordination-extraction boundary within the reformer-printer coalition''s operation.').

omega_variable(
    kernel_reading_structural_delta,
    'How does the beneficiary_agency_reading''s structural claim (tangled_rope coalition, scaffold technology) differ from the technological_determinism_reading''s claim (print as mountain/rope inevitability) and the co_constitution_reading''s claim (mutual shaping)?',
    'Formal comparison of the three readings'' ε values, stakeholder structures, and temporal trajectories — each reading must produce a distinct constraint story with non-overlapping metric profiles.',
    'If the readings produce indistinguishable ε/stakeholder profiles, the kernel decomposition fails ε-invariance and must be recomposed. Distinct profiles validate the three-story family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Structural distinctness of the three kernel readings per ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 1517, 1563).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_ref_beneficiary_tr_t1517, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1517, 0.12).
narrative_ontology:measurement(tech_ref_beneficiary_tr_t1525, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1525, 0.21).
narrative_ontology:measurement(tech_ref_beneficiary_tr_t1535, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1535, 0.28).
narrative_ontology:measurement(tech_ref_beneficiary_tr_t1545, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1545, 0.33).
narrative_ontology:measurement(tech_ref_beneficiary_tr_t1555, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1555, 0.3).
narrative_ontology:measurement(tech_ref_beneficiary_tr_t1563, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 1563, 0.31).

% Extraction over time
narrative_ontology:measurement(tech_ref_beneficiary_be_t1517, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1517, 0.28).
narrative_ontology:measurement(tech_ref_beneficiary_be_t1525, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1525, 0.42).
narrative_ontology:measurement(tech_ref_beneficiary_be_t1535, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1535, 0.55).
narrative_ontology:measurement(tech_ref_beneficiary_be_t1545, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1545, 0.52).
narrative_ontology:measurement(tech_ref_beneficiary_be_t1555, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1555, 0.56).
narrative_ontology:measurement(tech_ref_beneficiary_be_t1563, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 1563, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tech_ref_beneficiary_su_t1517, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1517, 0.25).
narrative_ontology:measurement(tech_ref_beneficiary_su_t1525, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1525, 0.38).
narrative_ontology:measurement(tech_ref_beneficiary_su_t1535, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1535, 0.45).
narrative_ontology:measurement(tech_ref_beneficiary_su_t1545, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1545, 0.4).
narrative_ontology:measurement(tech_ref_beneficiary_su_t1555, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1555, 0.43).
narrative_ontology:measurement(tech_ref_beneficiary_su_t1563, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 1563, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__beneficiary_agency_reading, 0.12).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality__co_constitution_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, confessional_state_formation).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, vernacular_literacy_institutionalization).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, print_censorship_regimes).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'printing press caused the Reformation' claim into three structurally distinct readings. This reading (beneficiary_agency) models the reformer-printer coalition as tangled_rope extracting from Church authority via strategic print deployment (ε=0.58). The technological_determinism_reading models print as mountain/rope with near-zero extraction. The co_constitution_reading models mutual shaping with intermediate extraction. Their ε values differ by >0.3, satisfying ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_reformation_causality__beneficiary_agency_reading, organized, 0.15).
constraint_indexing:directionality_override(technology_reformation_causality__beneficiary_agency_reading, moderate, 0.25).
constraint_indexing:directionality_override(technology_reformation_causality__beneficiary_agency_reading, institutional, 0.95).
constraint_indexing:directionality_override(technology_reformation_causality__beneficiary_agency_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
