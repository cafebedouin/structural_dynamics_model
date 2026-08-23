% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Printing Press Physical Capacity as Mountain Constraint on Information Dissemination
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   This constraint story models the technological_mediation_reading of the
 *   reformation_composite kernel: the claim that the printing press's
 *   physical properties (movable type, oil-based ink, press mechanics)
 *   constitute a mountain constraint — a fixed, natural-law-like capacity for
 *   mass text reproduction — that structurally enabled the Reformation's
 *   continental scale. The reading treats publication rates and literacy as
 *   primary observables, arguing that the physics of print transformed local
 *   dissent into mass movement. The claimed_type is mountain
 *   (emerges_naturally: true) because the constraint is the physical capacity
 *   itself, not the historical event. However, identifiable beneficiaries
 *   (printers, reformers, vernacular readers) and victims (scribes, censors,
 *   Latin elite) exist, triggering FSM evaluation. The engine will compute
 *   per-seat classifications from the structural data; the authored claim and
 *   metrics remain independent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.08).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.05).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.07).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.07).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Printing Press Physical Capacity as Mountain Constraint on Information Dissemination").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '12799698-52d7-4329-8ac8-805aacffbc22').
narrative_ontology:cs_kernel_codification('12799698-52d7-4329-8ac8-805aacffbc22', distributed).
narrative_ontology:cs_authority_grounding('12799698-52d7-4329-8ac8-805aacffbc22', distributed).
narrative_ontology:cs_reading_relation('12799698-52d7-4329-8ac8-805aacffbc22', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('12799698-52d7-4329-8ac8-805aacffbc22', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_axiom('12799698-52d7-4329-8ac8-805aacffbc22', foundational, print_capacity_as_primary_causal_driver).
narrative_ontology:cs_axiom_status(print_capacity_as_primary_causal_driver, holdable).
narrative_ontology:cs_axiom_grounding('12799698-52d7-4329-8ac8-805aacffbc22', print_capacity_as_primary_causal_driver, empirically_contingent).
narrative_ontology:cs_axiom('12799698-52d7-4329-8ac8-805aacffbc22', secondary, manuscript_bottleneck_as_sufficient_condition_for_localized_dissent).
narrative_ontology:cs_axiom_status(manuscript_bottleneck_as_sufficient_condition_for_localized_dissent, holdable).
narrative_ontology:cs_axiom_grounding('12799698-52d7-4329-8ac8-805aacffbc22', manuscript_bottleneck_as_sufficient_condition_for_localized_dissent, empirically_contingent).
narrative_ontology:cs_reference_frame('12799698-52d7-4329-8ac8-805aacffbc22', pre_print_manuscript_culture).
narrative_ontology:cs_drift_state('12799698-52d7-4329-8ac8-805aacffbc22', print_age_reformation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('12799698-52d7-4329-8ac8-805aacffbc22', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, printers_publishers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, dissenting_theologians).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_urban_public).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, vernacular_translators).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, scribal_guilds).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, censorship_authorities).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, latin_literate_elite).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, print_culture_thesis).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, technological_determinism_of_reformation).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, media_ecology_shifts_religious_consciousness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate printing presses across the Holy Roman Empire, Swiss cantons, and French cities. Produce and distribute pamphlets, vernacular Bibles, and polemical tracts. Profit from unprecedented demand for printed matter. Can relocate presses to more tolerant jurisdictions when pressured.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, printers_publishers, beneficiary,
    organized, biographical, mobile, continental).

% Luther, Calvin, Zwingli, and radical reformers use print to bypass ecclesiastical gatekeepers. Their writings spread faster than authorities can suppress them. Their exit is constrained by physical danger (excommunication, imperial ban) but print gives them a continental platform they never had in manuscript culture.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, dissenting_theologians, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, dissenting_theologians, agenda_setter).

% Merchants, artisans, and educated laity in German and Swiss cities gain direct access to theological arguments in vernacular. Participation in religious debate becomes possible without clerical mediation. Exit from traditional parish structures is socially costly but print provides alternative interpretive communities.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_urban_public, beneficiary,
    moderate, biographical, constrained, regional).

% Translators like Luther (German New Testament), Lefèvre d'Étaples (French), Tyndale (English) find printers eager for vernacular Scripture. Their work achieves continental circulation impossible in manuscript era. Can move between print centers when local authorities crack down.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, vernacular_translators, beneficiary,
    moderate, biographical, mobile, continental).

% Monastic and commercial scriptoria lose their monopoly on book production. Skills honed over generations become economically obsolete within decades. No viable exit — retraining options are minimal, and guild structures prevent adaptation. The constraint (print physics) destroys their livelihood structure directly.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, scribal_guilds, payer,
    organized, biographical, trapped, local).

% Papal Index, imperial edicts, university theology faculties, and city councils attempt to control print output. The physical speed and geographic dispersion of presses make pre-publication censorship ineffective. They adapt by licensing printers and post-publication prosecution, but the constraint forces costly enforcement escalation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, censorship_authorities, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, censorship_authorities, agenda_setter).

% Clergy, jurists, and humanists whose authority rested on Latin textual monopoly face vernacular competition. Their interpretive control fragments as lay readers access Scripture directly. Some adapt (Erasmus), many resist. Exit means accepting diminished gatekeeping role — structurally constrained by the new information ecology.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, latin_literate_elite, payer,
    powerful, generational, constrained, continental).

% Scholars from Eisenstein to Pettegree debate the causal weight of print vs. theology vs. politics. They analyze publication datasets, survival rates, and circulation networks. Their analytical seat is not shaped by the constraint but by the interpretive field it generates.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, early_modern_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_composite__technological_mediation_reading, diffuse).
narrative_ontology:fixing_cost_class(reformation_composite__technological_mediation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of mass information dissemination: identical texts can be produced in thousands of copies and distributed across linguistic territories, enabling synchronized theological debate across fragmented political jurisdictions without central authority.
% TRANSFER_FUNCTION: Moves textual authority from manuscript gatekeepers (scribal guilds, ecclesiastical censors, Latin-literate elite) to print entrepreneurs and vernacular readers. The transfer is not primarily monetary — it is epistemic authority and interpretive control. Printers gain economic rents; reformers gain audience; authorities lose monopoly.
% ABSENT_VOICES: Peasant populations (largely illiterate, dependent on oral transmission of printed content via preaching), women (excluded from print authorship and guild structures in most territories), Jewish communities (subject to distinct censorship regimes, Hebrew printing controlled separately), Anabaptist radicals (suppressed by both Protestant and Catholic print establishments). These voices would challenge the 'liberation by print' narrative but are structurally absent from the print archive.
% DISAPPEARANCE_RATIONALE: If the printing press's physical capacity for mass reproduction vanished overnight (reverting to manuscript culture), the Reformation as a continental movement would collapse into localized disputes. The synchronization of dissent across the Empire, Switzerland, France, and the Low Countries depends on the physics of movable type. Theological fragmentation would persist but not at continental scale; political realignment would lack the communicative infrastructure for coordinated confession-building.
% FOUNDING_PROBLEM: The constraint was not 'built' — the printing press emerged from Gutenberg's innovation (c. 1440) and spread as a commercial technology. The 'founding problem' it solved was the bottleneck of manuscript production: slow, expensive, error-prone copying that limited textual circulation to elite networks. The Reformation (1517+) exploited this pre-existing capacity; the press did not arise to cause the Reformation.
% FOUNDING_PROBLEM_CORROBORATION: Eisenstein (1979) and Pettegree (2015) document the press as a commercial innovation solving manuscript bottlenecks, not a Reformation tool. The 'print culture thesis' is corroborated by economic historians of the book trade (Febvre & Martin, 1958; Davenport, 2017) who show press diffusion preceded and enabled religious controversy. No beneficiary group (printers, reformers) invented the press for Reformation purposes — the technology's mountain-like physics predates its religious use.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.08, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.08) because the physical constraint itself extracts nothing — it is a capability. Suppression is minimal (0.05) because print enables rather than coerces; the rising suppression_requirement measurements reflect authorities' reactive enforcement against the constraint's output, not the constraint's own suppression. Theater ratio is low (0.07) — the press performs its function genuinely. Accessibility_collapse is very high (0.92): manuscript production for mass dissemination becomes economically non-viable once print exists. Resistance is low (0.12): the physics of print meets no resistance, though its products do. The measurement series tracks the constraint's operational profile from invention (1440) through the Reformation century, showing rising suppression_requirement as authorities react to print's output.
 *
 * PERSPECTIVAL GAP:
 *   From the printer/reformer seat, the press is pure coordination (rope-like) — it solves their dissemination problem. From the scribe/censor seat, it is a snare — it destroys their livelihood/control without consent. The analytical seat sees the mountain: the physics is fixed, the human responses vary. The engine computes this seat divergence from the structural data; the mountain claim applies to the physical constraint, not the historical responses to it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (printers, reformers, vernacular readers) sit at low directionality (d near 0) — the constraint subsidizes their activity by collapsing dissemination costs. Victims (scribes, censors, Latin elite) sit at high directionality (d near 1) — the constraint extracts their gatekeeping rents and authority. The engine derives this from beneficiary/victim declarations plus exit options: printers are mobile (arbitrage-grade exit), scribes are trapped, censors are constrained but institutional. The analytical observer sits at d=0.5 (analytical exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The printing press has no mandate — it is a technology, not an institution. Mandatrophy does not apply. The FSM omega captures the related ambiguity: whether the 'technological determination' thesis is a genuine mountain reading or a constructed narrative benefiting certain interpreters.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_constructed_narrative,
    'Is the printing press''s role as ''mountain constraint enabling the Reformation'' a genuine structural fact of physics and information ecology, or a constructed narrative that benefits technological determinist historiography and modern print-culture scholars?',
    'Counterfactual simulation: if publication rates and literacy data from 1450-1550 show that print diffusion patterns predict Reformation adoption better than theological or political variables, the mountain reading gains empirical support. If theological/political variables dominate in multivariate models, the mountain claim is a narrative overlay.',
    'If the mountain claim is a constructed narrative (false summit), the engine''s FSM signature would reclassify toward tangled_rope — the ''technological determination'' thesis would be a coordination story (print enables dissent) with asymmetric extraction (technological determinists gain interpretive authority, alternative readings are marginalized). If genuine mountain, the classification holds and the reading''s ε-invariance is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_constructed_narrative, conceptual, 'Whether the technological mediation thesis is a structural fact or a historiographical construction benefiting its proponents.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to the reformation_composite kernel differ from its siblings, and where is the disagreement located?',
    'Compare the three readings'' ε values, beneficiary/victim structures, and claimed_types. The disagreement is located in the primary causal observable: publication rates/literacy (this reading), doctrinal commitments (theological), sovereignty claims (political). Each reading authors a different ε over the same historical referent.',
    'Documents the committer-frame structure for cross-reading analysis. The engine uses reading_relations and axioms in cs_structure to compute foreclosure/influence dynamics across the kernel family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committee-frame metadata: this reading''s position in the reformation_composite kernel family, structural delta from siblings, and location of interpretive disagreement.').

omega_variable(
    print_physics_vs_social_adoption,
    'The mountain claim applies to the physics of movable type (fixed, natural-law-like). But the Reformation''s continental scale required social adoption (literacy, trade networks, vernacular standardization). Is the constraint properly the physics alone, or the socio-technical system?',
    'Decompose into two constraints: (1) print_physics_mountain (movable type physics) — genuine mountain, ε≈0; (2) print_culture_socio_technical (adoption, literacy, trade) — likely rope or tangled_rope with beneficiaries/victims. The ε-invariance principle requires this decomposition if observables yield different ε.',
    'If the constraint story conflates physics with adoption, its ε is unstable (measurement-dependent). Decomposition yields two stories with stable ε, linked via network.affects_constraints. The current story models the physics mountain; a sibling story would model the socio-technical adoption.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(print_physics_vs_social_adoption, conceptual, 'Whether the mountain constraint is the physics alone or the socio-technical system, per ε-invariance decomposition principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 1440, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_tr_t1440, reformation_composite__technological_mediation_reading, theater_ratio, 1440, 0.02).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_tr_t1470, reformation_composite__technological_mediation_reading, theater_ratio, 1470, 0.03).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_tr_t1500, reformation_composite__technological_mediation_reading, theater_ratio, 1500, 0.04).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_tr_t1517, reformation_composite__technological_mediation_reading, theater_ratio, 1517, 0.05).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_tr_t1530, reformation_composite__technological_mediation_reading, theater_ratio, 1530, 0.06).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_tr_t1555, reformation_composite__technological_mediation_reading, theater_ratio, 1555, 0.07).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_tr_t1600, reformation_composite__technological_mediation_reading, theater_ratio, 1600, 0.07).

% Extraction over time
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_be_t1440, reformation_composite__technological_mediation_reading, base_extractiveness, 1440, 0.02).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_be_t1470, reformation_composite__technological_mediation_reading, base_extractiveness, 1470, 0.03).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_be_t1500, reformation_composite__technological_mediation_reading, base_extractiveness, 1500, 0.04).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_be_t1517, reformation_composite__technological_mediation_reading, base_extractiveness, 1517, 0.05).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_be_t1530, reformation_composite__technological_mediation_reading, base_extractiveness, 1530, 0.07).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_be_t1555, reformation_composite__technological_mediation_reading, base_extractiveness, 1555, 0.08).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_be_t1600, reformation_composite__technological_mediation_reading, base_extractiveness, 1600, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_su_t1440, reformation_composite__technological_mediation_reading, suppression_requirement, 1440, 0.01).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_su_t1470, reformation_composite__technological_mediation_reading, suppression_requirement, 1470, 0.02).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_su_t1500, reformation_composite__technological_mediation_reading, suppression_requirement, 1500, 0.03).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_su_t1517, reformation_composite__technological_mediation_reading, suppression_requirement, 1517, 0.05).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_su_t1530, reformation_composite__technological_mediation_reading, suppression_requirement, 1530, 0.15).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_su_t1555, reformation_composite__technological_mediation_reading, suppression_requirement, 1555, 0.25).
narrative_ontology:measurement(reformation_composite__technological_mediation_reading_su_t1600, reformation_composite__technological_mediation_reading, suppression_requirement, 1600, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__technological_mediation_reading, information_standard).
narrative_ontology:boltzmann_floor_override(reformation_composite__technological_mediation_reading, 0.02).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).

% DUAL FORMULATION NOTE:
% This reading (technological_mediation) and its siblings (theological_fragmentation, political_realignment) form the reformation_composite constraint family. Each reading authors a different ε over the same historical referent (the Reformation), with different primary observables and different beneficiary/victim structures. This reading claims mountain (print physics); theological reading likely claims tangled_rope (doctrinal coordination + extraction); political reading likely claims scaffold (state-building coordination with sunset). The family is linked by mutual affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
