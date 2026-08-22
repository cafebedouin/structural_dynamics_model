% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Binding Rulings with Compliance Obligations
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body operates as a binding referee: panel
 *   rulings create legal obligations to comply, backed by authorized
 *   retaliation if the losing party does not bring its measures into
 *   conformity. Member states accepted this by ratifying the Dispute
 *   Settlement Understanding, trading policy discretion within covered
 *   domains for market access guarantees. This reading of the DSB authority
 *   kernel treats the binding character as the treaty's operational core —
 *   non-compliance is a treaty violation, not a policy choice. The claimed
 *   type is tangled_rope because the system solves a genuine coordination
 *   problem (credible commitment to market access) while extracting
 *   compliance from respondents who would prefer discretion. The measurement
 *   series shows extractiveness rising as the docket expanded and retaliation
 *   became routine; theater ratio grew as procedural compliance increasingly
 *   substitutes for substantive policy change; suppression requirement
 *   plateaued once the Appellate Body crisis (2019) froze the enforcement
 *   apex.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.58).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.42).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Binding Rulings with Compliance Obligations").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, 'bbb4501e-3a9b-40bb-bbdc-c175ae582edf').
narrative_ontology:cs_kernel_codification('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', formalized).
narrative_ontology:cs_authority_grounding('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', lineage).
narrative_ontology:cs_interpretation_layer_present('bbb4501e-3a9b-40bb-bbdc-c175ae582edf').
narrative_ontology:cs_reading_relation('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', foundational, dsb_rulings_create_legal_obligation_to_comply).
narrative_ontology:cs_axiom_status(dsb_rulings_create_legal_obligation_to_comply, holdable).
narrative_ontology:cs_axiom_grounding('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', dsb_rulings_create_legal_obligation_to_comply, conventional).
narrative_ontology:cs_axiom('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', foundational, retaliation_authorization_is_legitimate_enforcement).
narrative_ontology:cs_axiom_status(retaliation_authorization_is_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', retaliation_authorization_is_legitimate_enforcement, conventional).
narrative_ontology:cs_reference_frame('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', dsu_treaty_text_as_ratified).
narrative_ontology:cs_drift_state('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', post_appellate_body_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bbb4501e-3a9b-40bb-bbdc-c175ae582edf', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, export_oriented_economies).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, rules_based_trade_advocates).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, developing_country_complainants).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, sovereignty_prioritizing_states).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_policy_constituencies).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, non_compliant_respondents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain enforceable market access for their exports through binding rulings; use the DSB proactively as complainants. Their exit is constrained by dependence on the multilateral system for market access guarantees, but they have bilateral/regional alternatives. They collect the coordination benefit and occasionally bear respondent costs.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, export_oriented_economies, beneficiary,
    powerful, generational, constrained, global).

% Includes WTO Secretariat, DSB panels, Appellate Body (when functional), and pro-multilateralism NGOs. They administer and defend the binding character of rulings as the system's legitimacy core. They do not capture rents but their institutional survival depends on the constraint's perceived effectiveness.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, rules_based_trade_advocates, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, rules_based_trade_advocates, agenda_setter).

% Use DSB to challenge richer countries' barriers; gain legal victories but face implementation hurdles. Their exit is constrained — they need the system more than powerful respondents need them — but they derive net benefit from the legal shield against unilateral pressure.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, developing_country_complainants, beneficiary,
    moderate, biographical, constrained, global).

% Accept binding rulings as treaty obligation but resist when rulings touch core regulatory domains (health, environment, security). They bear compliance costs and face retaliation risk. Their exit is constrained by the cost of leaving the WTO (loss of MFN, trade disruption) but they drive the Appellate Body blockade to limit judicial reach.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, sovereignty_prioritizing_states, payer,
    powerful, generational, constrained, global).

% Industries, workers, regulators, and communities affected by compliance-driven policy changes. They did not consent to the treaty and have no direct exit — their state's WTO membership binds them. They bear adjustment costs (job loss, regulatory change) with no direct access to DSB proceedings.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_policy_constituencies, payer,
    organized, biographical, trapped, national).

% States that lose DSB cases and refuse/delay compliance. They face authorized retaliation — a direct transfer from their exporters to the complainant. Their exit is trapped: comply (political cost), retaliate back (escalation), or withdraw (systemic cost). They are the extraction target in the constraint's current operation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, non_compliant_respondents, payer,
    moderate, immediate, trapped, national).

% Analyze the DSB's jurisprudence, legitimacy, and effectiveness from outside the compliance dynamic. They see the full structure — coordination function, extraction asymmetry, and the three reading frameworks — but neither collect nor pay.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, legal_scholars_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the credible commitment problem in multilateral trade: states exchange market access concessions that would be vulnerable to unilateral reversal without a binding dispute mechanism with enforcement teeth.
% TRANSFER_FUNCTION: Moves policy adjustment costs from the system onto losing respondents (who must change laws/regulations) and moves retaliation authority to winning complainants (who gain leverage). The net transfer is from sovereignty/discretion to legal certainty/market access.
% ABSENT_VOICES: Future generations who inherit the treaty framework without consent; non-WTO members excluded from the system's benefits but affected by its norms; domestic constituencies in respondent states who bear compliance costs without representation in Geneva; small economies that lack capacity to use the DSB effectively.
% DISAPPEARANCE_RATIONALE: If binding rulings and retaliation authorization vanished overnight, the WTO would revert to GATT-style diplomatic settlement — market access commitments would lose credibility, unilateral trade measures would proliferate, and power-based bargaining would replace legal adjudication. The multilateral trade system would reorganize around bilateral/regional deals and power politics.
% FOUNDING_PROBLEM: Post-WWII trade architecture required a mechanism to make tariff bindings and non-discrimination commitments credible — without enforcement, concessions are reversible and the system unravels. The DSB was built to convert political commitments into legal obligations with teeth.
% FOUNDING_PROBLEM_CORROBORATION: The credible commitment problem is attested by trade economists (e.g., Bagwell & Staiger), WTO founding documents, and the continued demand for DSB access by new acceding members. However, the sovereignty-prioritizing states and the judicial_activism_reading proponents contest whether the current binding-referee form is necessary or proportionate to the founding problem — corroboration exists for the problem's existence, not for this reading's specific solution.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58) reflects that respondents bear real adjustment costs — changing laws, regulations, or practices — while complainants gain market access. Suppression (0.42) is moderate: states can technically withdraw (Article XV) but face prohibitive exit costs (loss of MFN, retaliation exposure). Theater ratio (0.28) captures the growing gap between procedural compliance (implementing rulings formally) and substantive alignment (changing the underlying policy logic). Accessibility collapse (0.52) reflects that alternatives (bilateral deals, unilateralism) exist but are costly. Resistance (0.48) captures the Appellate Body blockade and rising criticism from both sovereignty-prioritizing and developing-country constituencies.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (export-oriented economy), the constraint is a rope: it coordinates credible commitments, and the compliance cost is the price of others' commitments. From the target seat (sovereignty-prioritizing state), it is a snare: the binding ruling extracts policy concessions backed by retaliation threat, with no reciprocal benefit when the state is respondent. The engine computes this divergence from the structural data — the binding referee reading does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Export-oriented economies and developing-country complainants are structural beneficiaries (d ~0.15-0.25): they gain enforceable market access and a forum where power asymmetry is partially mitigated by legal rules. Sovereignty-prioritizing states and domestic constituencies bearing adjustment costs are targets (d ~0.7-0.85): they lose policy space and face retaliation if they resist. Rules-based trade advocates sit near symmetric (d ~0.45): they value the system but bear credibility costs when it fails. Non-compliant respondents are trapped targets (d ~0.9) — retaliation authorization leaves no credible exit. The DSB itself as agenda_setter is institutional (d ~0.3) — it administers but does not capture the gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credible commitment to market access — remains live (founding_problem_status: live), but the arrangement has accumulated extraction (rising base_extractiveness) and theater (rising theater_ratio) beyond the coordination function. The Appellate Body crisis reveals that the enforcement mechanism depends on a consensus the major players no longer sustain. This is not mandatrophy (the problem isn't dead) but a coordination-extraction tension that the tangled_rope classification captures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'Where exactly do the sibling readings of the WTO DSB authority kernel disagree structurally — on the binding force of rulings, the scope of surrendered discretion, or the legitimacy of retaliatory authorization?',
    'Comparative analysis of treaty text (DSU Articles 3, 21, 22), panel/Appellate Body jurisprudence, and state practice on compliance and retaliation across the three reading frameworks.',
    'If disagreement centers on binding force, this reading''s high extractiveness/suppression reflects genuine treaty obligation; if on scope of surrendered discretion, extraction is contextual; if on retaliatory legitimacy, the enforcement mechanism itself is contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, conceptual, 'Structural locus of disagreement among kernel readings of WTO DSB authority').

omega_variable(
    sovereignty_market_access_tradeoff_naturalness,
    'Is the sovereignty-for-market-access trade inherent to multilateral trade law, or a constructed constraint benefiting export-oriented members?',
    'Counterfactual analysis of GATT/WTO accession negotiations: whether sovereignty concessions were explicit conditions or emergent outcomes; historical comparison with alternative institutional designs (e.g., ITO Havana Charter).',
    'If inherent, this reading''s claimed tangled_rope with genuine coordination function is structurally accurate; if constructed, the constraint leans toward snare with export-oriented economies as concentrated beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_market_access_tradeoff_naturalness, conceptual, 'Whether the sovereignty-market_access trade is natural law or constructed extraction').

omega_variable(
    compliance_pressure_mechanism,
    'Does compliance pressure operate through genuine reciprocity (rope-like) or through asymmetric power to authorize retaliation (snare-like)?',
    'Empirical study of retaliation authorization frequency, compliance timelines, and power asymmetry in outcomes — comparing strong vs. weak complainants and respondents.',
    'If compliance is reciprocal, the tangled_rope coordination function holds; if asymmetric, the binding referee reading masks power-based extraction and should reclassify toward snare from the weak-state seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_pressure_mechanism, empirical, 'Whether retaliation authorization creates symmetric coordination or asymmetric extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__binding_referee_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__binding_referee_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__binding_referee_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__binding_referee_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement(wto__tr_t2020, wto_dsb_authority__binding_referee_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__binding_referee_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(wto__be_t2020, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2020, 0.57).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2005, 0.38).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(wto__su_t2020, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__binding_referee_reading, 0.12).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_mfn_principle).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_national_treatment).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_subsidies_agreement).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_trips_enforcement).

% DUAL FORMULATION NOTE:
% Part of the wto_dsb_authority constraint family (3 readings). This binding_referee_reading treats binding rulings with retaliation backing as the operational core. The advisory_coordination_reading treats DSB as facilitative; the judicial_activism_reading treats interpretive drift as illegitimate overreach. All three share the kernel but instantiate different constraints with different ε, beneficiaries, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, institutional, 0.3).
constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, powerful, 0.75).
constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, moderate, 0.8).
constraint_indexing:directionality_override(wto_dsb_authority__binding_referee_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
