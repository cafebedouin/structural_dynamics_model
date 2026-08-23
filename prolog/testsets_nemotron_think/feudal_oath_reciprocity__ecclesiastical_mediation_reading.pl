% ============================================================================
% CONSTRAINT STORY: feudal_oath_reciprocity__ecclesiastical_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_oath_reciprocity__ecclesiastical_mediation_reading, []).

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
 *   constraint_id: feudal_oath_reciprocity__ecclesiastical_mediation_reading
 *   human_readable: Feudal Oath Bound by Christian Charity and Sacramental Obligations
 *   domain: medieval_political_economy/legal_history/institutional_analysis
 *
 * SUMMARY:
 *   The ecclesiastical mediation reading treats the feudal oath as a
 *   sacramental bond whose content is defined by Christian charity (caritas)
 *   and natural law, not by lordly will. The church, through canonical courts
 *   and penitential discipline, claims authority to judge whether a lord's
 *   extraction exceeds theological limits — usury, unjust exactions, failure
 *   of protection. This reading reached its peak institutional expression in
 *   the Gregorian reform and Fourth Lateran Council (1215), then stabilized
 *   as canon law became a professionalized interpretive layer. The constraint
 *   is a tangled rope: it coordinates genuine reciprocal protection (vassals
 *   gain appeal against arbitrary power) while extracting interpretive
 *   authority and material revenue for the church, and actively suppressing
 *   lordly autonomy through excommunication and interdict.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.42).
domain_priors:suppression_score(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.55).
domain_priors:theater_ratio(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(feudal_oath_reciprocity__ecclesiastical_mediation_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, tangled_rope).
narrative_ontology:human_readable(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "Feudal Oath Bound by Christian Charity and Sacramental Obligations").
narrative_ontology:topic_domain(feudal_oath_reciprocity__ecclesiastical_mediation_reading, "medieval_political_economy/legal_history/institutional_analysis").

domain_priors:requires_active_enforcement(feudal_oath_reciprocity__ecclesiastical_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'a49856af-9f0c-4dfe-890a-38d151ba8bdd').
narrative_ontology:cs_kernel_codification('a49856af-9f0c-4dfe-890a-38d151ba8bdd', formalized).
narrative_ontology:cs_authority_grounding('a49856af-9f0c-4dfe-890a-38d151ba8bdd', lineage).
narrative_ontology:cs_interpretation_layer_present('a49856af-9f0c-4dfe-890a-38d151ba8bdd').
narrative_ontology:cs_reading_relation('a49856af-9f0c-4dfe-890a-38d151ba8bdd', feudal_oath_reciprocity__lord_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('a49856af-9f0c-4dfe-890a-38d151ba8bdd', feudal_oath_reciprocity__vassal_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('a49856af-9f0c-4dfe-890a-38d151ba8bdd', foundational, oath_as_sacramental_bond).
narrative_ontology:cs_axiom_status(oath_as_sacramental_bond, holdable).
narrative_ontology:cs_axiom_grounding('a49856af-9f0c-4dfe-890a-38d151ba8bdd', oath_as_sacramental_bond, theological).
narrative_ontology:cs_axiom('a49856af-9f0c-4dfe-890a-38d151ba8bdd', foundational, charity_limits_extraction).
narrative_ontology:cs_axiom_status(charity_limits_extraction, holdable).
narrative_ontology:cs_axiom_grounding('a49856af-9f0c-4dfe-890a-38d151ba8bdd', charity_limits_extraction, deontological).
narrative_ontology:cs_axiom('a49856af-9f0c-4dfe-890a-38d151ba8bdd', secondary, ecclesiastical_jurisdiction_over_temporal_oaths).
narrative_ontology:cs_axiom_status(ecclesiastical_jurisdiction_over_temporal_oaths, holdable).
narrative_ontology:cs_axiom_grounding('a49856af-9f0c-4dfe-890a-38d151ba8bdd', ecclesiastical_jurisdiction_over_temporal_oaths, conventional).
narrative_ontology:cs_reference_frame('a49856af-9f0c-4dfe-890a-38d151ba8bdd', carolingian_oath_theology).
narrative_ontology:cs_drift_state('a49856af-9f0c-4dfe-890a-38d151ba8bdd', post_gregorian_reform, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a49856af-9f0c-4dfe-890a-38d151ba8bdd', '').
narrative_ontology:cs_kernel_id(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities).
narrative_ontology:constraint_beneficiary(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals_peasants).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals_peasants).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, sacramental_oath_theology).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, christian_charity_doctrine).
narrative_ontology:constraint_vindicates(feudal_oath_reciprocity__ecclesiastical_mediation_reading, corpus_christi_ecclesiology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce sacramental oath obligations through canonical courts, penitential discipline, and excommunication. Gain interpretive authority over feudal relations and material support (tithes, papal provisions, episcopal revenues) from legitimizing the social order. Can shift between papal, conciliar, and local episcopal jurisdictions.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, ecclesiastical_authorities, beneficiary).

% Bound by oath to provide protection and justice to vassals in return for service, but constrained by theological limits on extraction (usury prohibitions, just price, charity obligations). Enforcement comes through threat of excommunication, interdict, and loss of legitimacy. Exit requires either papal dispensation (rare) or open schism (existential risk).
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_lords, payer,
    powerful, biographical, constrained, regional).

% Receive protection from arbitrary extraction through ecclesiastical mediation: can appeal to bishop or papal delegate against lordly excess. But also bear costs of tithes, feudal dues, and labor services. Exit is constrained by serfdom, geographic immobility, and dependence on manorial economy; flight to towns or frontier is possible but risky.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals_peasants, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(feudal_oath_reciprocity__ecclesiastical_mediation_reading, vassals_peasants, payer).

% Seek to centralize justice and taxation, bypassing both feudal intermediaries and ecclesiastical courts. Would object to church claims over temporal oaths but are excluded from the sacramental framework. Their growing bureaucratic apparatus (exchequer, royal courts) creates an alternative enforcement channel.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, secular_monarchs, excluded,
    powerful, generational, mobile, national).

% Develop the doctrinal architecture (Decretum Gratiani, decretals, summae) that defines the obligation's content. Their commentaries shape how bishops judge lordly extraction. They do not directly collect rents but their interpretive labor sustains the church's authority.
narrative_ontology:constraint_stakeholder(feudal_oath_reciprocity__ecclesiastical_mediation_reading, canonical_lawyers, observer,
    organized, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Binds warrior aristocracy to reciprocal protection obligations in a stateless order by sacralizing the oath, making violation a sin subject to ecclesiastical sanction rather than merely a breach of contract.
% TRANSFER_FUNCTION: Channels interpretive authority and material revenue (tithes, judicial fees, papal provisions) to the church; limits the extraction lords can impose on vassals (labor, produce, money) by defining theological maxima; transfers risk of arbitrary lordly power onto the ecclesiastical enforcement apparatus.
% ABSENT_VOICES: Urban merchants, Jewish communities, and frontier settlers operating outside the manorial-oath nexus. They would contest both the church's universal jurisdictional claim and the lord's protected extraction, but are structurally excluded from the sacramental framework.
% DISAPPEARANCE_RATIONALE: If the sacramental binding vanished overnight, lords would revert to maximal extraction bounded only by vassal resistance capacity (the lord_extraction_reading), vassals would lose their primary appeal channel beyond flight or revolt, and the church would lose its central leverage over the social order — the feudal polity would restructure around naked power or royal bureaucratic law.
% FOUNDING_PROBLEM: How to constrain warrior aristocracy in a fragmented post-Carolingian world where no central monopoly of force exists, by rooting reciprocal obligation in a transcendent sanction that outlasts any single lord's life or dynasty.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (Bloch, Kantorowicz, Tierney) document the Gregorian reform's deliberate construction of this framework; chroniclers (Orderic Vitalis, Lambert of Hersfeld) record contemporary lords accepting ecclesiastical judgment on extraction disputes. No non-ecclesiastical source from the period corroborates the church's claim that the framework arose organically from Christian consensus rather than papal-engineered jurisdictional expansion.
narrative_ontology:disappearance_verdict(feudal_oath_reciprocity__ecclesiastical_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(feudal_oath_reciprocity__ecclesiastical_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).
:- end_tests(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the church's take (tithes, judicial fees, provisions) is real but bounded by its need to maintain legitimacy as a neutral arbiter; suppression is higher (0.55) because the constraint's persistence depends on active ecclesiastical enforcement (canonical courts, threat of interdict) not voluntary compliance; theater ratio (0.28) reflects that many episcopal interventions were performative — public penance ceremonies, published decretals — while actual extraction limits on powerful lords were often negotiated. Accessibility collapse (0.62) is significant: once the oath is sacralized, secular alternatives (charter law, royal courts) are theologically delegitimized. Resistance (0.48) is moderate: lords resisted specific interventions (Investiture Controversy, Magna Carta's ecclesiastical clauses) but rarely the framework itself.
 *
 * PERSPECTIVAL GAP:
 *   From the bishop's seat, the arrangement is genuine coordination solving the problem of binding violence to justice; from the lord's seat, it is an extractive imposition limiting his traditional rights; from the villein's seat, it is the only shield against arbitrary power. The engine computes this divergence from the structural data — the claimed type (tangled_rope) acknowledges all three simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical authorities are structural beneficiaries (d ~0.15): they collect interpretive rents and material support while controlling the enforcement apparatus. Secular lords are targets (d ~0.75): they bear the constraint's limits on extraction and face sanctions for violation, with constrained exit (schism is existential). Vassals/peasants sit near symmetric (d ~0.5): they gain protection from maximal extraction but pay tithes and dues; their exit is constrained by serfdom. Secular monarchs are excluded (d undefined): they operate outside the sacramental frame but their rising power erodes it. Canonical lawyers are analytical observers (d ~0.5): they shape the constraint's content without directly collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constraining warrior aristocracy without state monopoly) is dead — royal bureaucracies, standing armies, and Roman law reception provided alternative solutions by 1350. Yet the arrangement persisted because the church's interpretive authority had become a revenue source (mandatrophy unresolved). The theater ratio rise from 0.15 to 0.28 tracks this: more energy went into maintaining the performance of ecclesiastical judgment than into actual constraint of lordly power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_sincerity_vs_instrumentalism,
    'Did ecclesiastical authorities genuinely believe the sacramental framework constrained extraction, or was it an instrumental cover for jurisdictional and revenue expansion?',
    'Compare episcopal court records (actual judgments limiting lordly exactions) against papal provision registers and tax farming contracts (material extraction by the church). If judgments against lords correlate with church revenue needs, instrumentalism is supported.',
    'If instrumental, the constraint''s claimed coordination function is a cover for a snare; if sincere, the tangled_rope classification stands with genuine coordination alongside real extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_sincerity_vs_instrumentalism, conceptual, 'Whether the church''s interpretive authority was held in good faith or as a revenue strategy.').

omega_variable(
    charity_limit_operationalization,
    'How were the theological limits on extraction (just price, usury, charity) operationalized in specific manorial contexts — were they enforceable standards or aspirational rhetoric?',
    'Manorial court rolls, episcopal visitation records, and peasant petitions to canonical courts. Measure frequency and outcomes of peasant appeals against lordly exactions on charity grounds.',
    'If operationalized, vassals_peasants'' beneficiary role is substantive; if rhetorical, their beneficiary status is theatrical and the constraint leans toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charity_limit_operationalization, empirical, 'Whether theological extraction limits had practical bite in manorial administration.').

omega_variable(
    church_as_extractor_ambiguity,
    'Does the church''s own extraction (tithes, first fruits, papal provisions) constitute a second-order snare layered on the tangled rope, making the church both coordinator and extractor?',
    'Quantify church revenue from feudal societies as percentage of agrarian surplus; compare to lordly extraction. Assess whether peasant appeals against church exactions were heard in the same canonical courts.',
    'If church extraction rivals or exceeds lordly extraction, the constraint family may need a third reading (church_extraction_reading) and the current reading''s claimed_type may understate total system extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_as_extractor_ambiguity, empirical, 'Whether the ecclesiastical mediator is itself a major extractor from the same population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 1050, 1350).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tr_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1050, 0.15).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tr_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1100, 0.18).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tr_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1150, 0.22).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tr_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1200, 0.25).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tr_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1250, 0.28).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tr_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1300, 0.28).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_tr_t1350, feudal_oath_reciprocity__ecclesiastical_mediation_reading, theater_ratio, 1350, 0.28).

% Extraction over time
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_be_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1050, 0.35).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_be_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1100, 0.38).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_be_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1150, 0.4).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_be_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1200, 0.42).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_be_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1250, 0.43).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_be_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1300, 0.42).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_be_t1350, feudal_oath_reciprocity__ecclesiastical_mediation_reading, base_extractiveness, 1350, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_su_t1050, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1050, 0.45).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_su_t1100, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1100, 0.5).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_su_t1150, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1150, 0.52).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_su_t1200, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1200, 0.55).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_su_t1250, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1250, 0.55).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_su_t1300, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1300, 0.55).
narrative_ontology:measurement(feudal_oath_reciprocity__ecclesiastical_mediation_reading_su_t1350, feudal_oath_reciprocity__ecclesiastical_mediation_reading, suppression_requirement, 1350, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_oath_reciprocity__ecclesiastical_mediation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, 0.12).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__lord_extraction_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, feudal_oath_reciprocity__vassal_coordination_reading).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, canon_law_jurisdiction).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, manorial_court_system).
narrative_ontology:affects_constraint(feudal_oath_reciprocity__ecclesiastical_mediation_reading, royal_justice_centralization).

% DUAL FORMULATION NOTE:
% The feudal_oath_reciprocity kernel decomposes into three structurally distinct constraints with different ε values and beneficiary/victim structures. This reading (ecclesiastical_mediation) has moderate ε (0.42) with church as agenda_setter/beneficiary and lords as payers. The lord_extraction_reading has high ε (est. 0.75) with lords as agenda_setters and vassals as payers. The vassal_coordination_reading has low ε (est. 0.25) with charter text as coordination mechanism. They are linked because each was historically invoked as the 'true meaning' of the same oath ceremony.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, institutional, 0.15).
constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, powerful, 0.75).
constraint_indexing:directionality_override(feudal_oath_reciprocity__ecclesiastical_mediation_reading, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
