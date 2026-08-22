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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Weaponization of the Printing Press by Reformers and Printers
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint models the strategic deployment reading of the
 *   press-Reformation relationship: reformers (Luther, Calvin, Zwingli, and
 *   their networks) and printing entrepreneurs (Froben, Petreius, the
 *   Wittenberg press, the Geneva presses) deliberately used the printing
 *   press as a weapon to break the Catholic Church's doctrinal and economic
 *   monopoly. The press itself functions as a rope — a coordination
 *   technology that solved a genuine collective-action problem for the reform
 *   movement. But the Church experiences the same arrangement as a snare: an
 *   extractive constraint that strips legitimacy, revenue, and territorial
 *   control through active suppression (censorship, licensing, Index) that
 *   fails to restore the status quo ante. The claimed_type is 'rope' from the
 *   reformer/printer seat; the engine will compute a different type from the
 *   Church's seat. This divergence is the measurement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.68).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.72).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Weaponization of the Printing Press by Reformers and Printers").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, 'da8997f0-13ce-45db-8f47-acf08c578a7e').
narrative_ontology:cs_kernel_codification('da8997f0-13ce-45db-8f47-acf08c578a7e', distributed).
narrative_ontology:cs_authority_grounding('da8997f0-13ce-45db-8f47-acf08c578a7e', practice).
narrative_ontology:cs_interpretation_layer_present('da8997f0-13ce-45db-8f47-acf08c578a7e').
narrative_ontology:cs_reading_relation('da8997f0-13ce-45db-8f47-acf08c578a7e', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('da8997f0-13ce-45db-8f47-acf08c578a7e', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('da8997f0-13ce-45db-8f47-acf08c578a7e', foundational, human_agency_primary_in_technological_effects).
narrative_ontology:cs_axiom_status(human_agency_primary_in_technological_effects, holdable).
narrative_ontology:cs_axiom_grounding('da8997f0-13ce-45db-8f47-acf08c578a7e', human_agency_primary_in_technological_effects, deontological).
narrative_ontology:cs_axiom('da8997f0-13ce-45db-8f47-acf08c578a7e', secondary, technology_is_neutral_instrument).
narrative_ontology:cs_axiom_status(technology_is_neutral_instrument, holdable).
narrative_ontology:cs_axiom_grounding('da8997f0-13ce-45db-8f47-acf08c578a7e', technology_is_neutral_instrument, instrumental).
narrative_ontology:cs_reference_frame('da8997f0-13ce-45db-8f47-acf08c578a7e', pre_print_ecclesiastical_monopoly).
narrative_ontology:cs_drift_state('da8997f0-13ce-45db-8f47-acf08c578a7e', westphalian_confessional_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da8997f0-13ce-45db-8f47-acf08c578a7e', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printing_entrepreneurs).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_church_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, vernacular_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Used the press to disseminate vernacular scripture, polemical pamphlets, and theological treatises that bypassed clerical gatekeeping. Gained rapid ideological reach and movement coordination. Their structural position depended on maintaining access to sympathetic printers and distribution networks; exit meant returning to manuscript circulation or oral preaching with drastically reduced scale.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, protestant_reformers, beneficiary,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, protestant_reformers, agenda_setter).

% Profited from surging demand for vernacular Bibles, reformist pamphlets, and anti-papal literature. Operated in a competitive market across the Holy Roman Empire, Switzerland, and the Low Countries; could relocate presses to friendlier jurisdictions. Their benefit was economic and contingent on the Reformation's momentum — not ideologically locked to it.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printing_entrepreneurs, beneficiary,
    moderate, biographical, mobile, continental).

% Faced an unprecedented challenge to doctrinal monopoly and institutional revenue (indulgences, ecclesiastical courts). Responded with censorship (Index Librorum Prohibitorum), press licensing, and the Council of Trent's doctrinal hardening. Could not exit the conflict — the press had already escaped its control — but could escalate suppression. The constraint extracted legitimacy, revenue, and territorial control from the Church.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_church_authority, payer,
    institutional, civilizational, constrained, universal).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, catholic_church_authority, agenda_setter).

% Gained direct access to scripture and religious argument in their own languages, enabling lay theological participation. Their situation depended on the continued production and circulation of vernacular texts; exit meant returning to clerical mediation. Not a unified actor — literacy varied by region, class, and gender.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, vernacular_readers, beneficiary,
    organized, biographical, constrained, continental).

% Used press regulation to manage religious conflict within their territories — some protecting reformist printing (e.g., Wittenberg, Strasbourg, Basel), others enforcing Catholic censorship. Their agenda-setting was strategic: press policy served territorial sovereignty and fiscal interests (printing privileges, tax revenue). Not mere passive enforcers of Church or reformer will.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, imperial_and_city_authorities, agenda_setter,
    institutional, generational, constrained, regional).

% Analyze the press-Reformation relationship through surviving print runs, correspondence, censorship records, and economic data. Their structural position is outside the constraint — they do not bear its costs or collect its rents — but their interpretations shape how the constraint's legacy is understood in subsequent institutional memory.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, historians_of_early_print, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solved a genuine coordination problem for reformers: how to synchronize theological argument, mobilize lay support, and sustain a movement across fragmented political jurisdictions without a central organization. For printers, it coordinated supply (capital, labor, distribution) with a predictable, high-demand product line.
% TRANSFER_FUNCTION: Moves attention, legitimacy, and revenue from the Catholic Church's centralized doctrinal economy to a decentralized print market. Reformers gain ideological reach; printers gain market share; the Church loses monopoly control over salvation narratives and the financial flows (indulgences, dispensations) attached to them.
% ABSENT_VOICES: Illiterate peasants, women excluded from the print public sphere, Jewish communities whose presses were differentially targeted, and Anabaptist radicals who were suppressed by both Catholic and magisterial Protestant authorities — all would have objected to the terms on which the press was deployed, but were not in the room when printing privileges and censorship policies were set.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the press by reformers and printers vanished overnight — i.e., if the press reverted to a neutral technology with no ideological alignment — the Reformation as a coordinated, trans-territorial movement would collapse into local disputations. The Catholic Church would retain doctrinal monopoly in its territories. The print economy would reorient to Latin humanist texts, devotional works, and administrative printing. The world rearranges.
% FOUNDING_PROBLEM: The Catholic Church's monopoly on Latin scripture, sacramental mediation, and indulgence revenue created a coordination failure: dissenting theology could not scale, laypeople had no independent access to doctrinal sources, and reform-minded princes lacked a tool to legitimate territorial religious reform.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem — the Church's information monopoly and the coordination failure it imposed on dissent — is dead in its original form. The press broke that monopoly irreversibly. Contemporary Catholic historians (e.g., Hubert Jedin, John W. O'Malley) and Protestant historians (e.g., Heiko Oberman, Carlos Eire) corroborate that the structural conditions the press was weaponized to overcome no longer obtain. The arrangement (strategic press deployment) persists in transformed ways (media strategy, platform politics) but the original founding problem is resolved.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the Church's loss of monopoly rents (indulgences, dispensations, ecclesiastical court fees) and doctrinal control — not because the press inherently extracts, but because the strategic deployment by reformers/printers redirected those flows. Suppression (0.72) is high because the Church's response required continuous, escalating enforcement (Index, Inquisition, press licensing, territorial bans) that never fully succeeded. Theater ratio (0.25) is moderate: early Reformation printing was functionally efficient (pamphlets reached markets in weeks), but later confessionalization produced performative orthodoxy-enforcement on all sides. Accessibility collapse (0.45) is moderate — alternatives (manuscript circulation, oral preaching) persisted but could not scale. Resistance (0.58) reflects the Church's sustained but ultimately unsuccessful countermeasures.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer/printer seat, the press is a liberating coordination tool (rope). From the Church's seat, it is an extraction machine backed by force (snare). The engine computes per-seat types from the same structural data; this perspectival gap is not a bug — it is the phenomenon under study. The claimed_type 'rope' reflects the reformer/printer framing; the Church's computed type will differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are beneficiaries (d ≈ 0.1–0.2): the constraint subsidizes their coordination and profit. The Church is the primary target (d ≈ 0.9): it bears the extraction of legitimacy and revenue. Vernacular readers are secondary beneficiaries with constrained exit (d ≈ 0.3). Imperial/city authorities are agenda-setters with constrained exit — they deploy the constraint strategically but cannot fully control its effects. Historians are analytical observers (d = 0.5). The structural asymmetry is that the same press technology is a rope for one coalition and a snare for the other — not because of measurement differences, but because of opposed directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Church information monopoly) is dead — the press irreversibly broke it. Yet the strategic deployment logic persists: modern actors (political movements, state propagandists, platform algorithms) still weaponize communication infrastructure against institutional monopolies. The constraint has not resolved its mandatrophy — it has mutated. The original arrangement (Luther's pamphlets, Froben's press) is historically complete, but its structural logic is redeployed. This is not a piton (theatrical maintenance of a dead function); it is a scaffold whose sunset clause was never triggered because the coordination function it enabled (mass ideological mobilization) became a permanent feature of modernity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_as_autonomous_vs_appropriated,
    'Does the printing press have an inherent causal tendency toward religious fragmentation, or was its Reformation-era effect entirely contingent on human strategic choices?',
    'Counterfactual comparison: regions with similar press density but different political/theological conditions (e.g., Italy vs. Germany). If fragmentation correlates with press density alone, technological_determinism gains ground; if it correlates with reformer/printer agency, strategic_deployment gains ground.',
    'If the press has autonomous fragmenting tendencies, strategic_deployment understates structural determination; if effects are wholly contingent on agency, technological_determinism is falsified and the rope classification for reformers strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_as_autonomous_vs_appropriated, conceptual, 'Whether the press''s causal role is inherent or appropriated.').

omega_variable(
    economic_vs_ideological_motivation,
    'Were printing entrepreneurs primarily motivated by profit or by ideological commitment to the Reformation?',
    'Archival analysis of printer correspondence, financial records, and publication choices — did printers publish Catholic works when profitable? Did they relocate for market access or confessional safety?',
    'If primarily profit-motivated, printers are mobile beneficiaries (rope coordination); if ideologically locked, they are identity_locked (tangled_rope/snare dynamics). Affects exit_options classification and thus directionality.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_vs_ideological_motivation, empirical, 'Printer motivation: economic opportunism vs. confessional commitment.').

omega_variable(
    church_extraction_measurement,
    'How much of the Church''s lost revenue and legitimacy was directly extracted by the press deployment vs. lost to broader secularization and state-building?',
    'Quantitative history: compare indulgence revenue, ecclesiastical court cases, and papal tax flows in territories with vs. without reformist printing, controlling for princely appropriation.',
    'If extraction is mostly redirected to reformers/printers, the snare classification for the Church strengthens. If mostly lost to state appropriation, the Church is a victim of a broader structural shift — the press is a catalyst, not the extractor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(church_extraction_measurement, empirical, 'Attribution of Church losses: press deployment vs. structural secularization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causality__strategic_deployment, theater_ratio, 1517, 0.05).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causality__strategic_deployment, theater_ratio, 1525, 0.12).
narrative_ontology:measurement(pres_tr_t1535, press_reformation_causality__strategic_deployment, theater_ratio, 1535, 0.18).
narrative_ontology:measurement(pres_tr_t1545, press_reformation_causality__strategic_deployment, theater_ratio, 1545, 0.22).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causality__strategic_deployment, theater_ratio, 1555, 0.24).
narrative_ontology:measurement(pres_tr_t1618, press_reformation_causality__strategic_deployment, theater_ratio, 1618, 0.25).
narrative_ontology:measurement(pres_tr_t1648, press_reformation_causality__strategic_deployment, theater_ratio, 1648, 0.25).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causality__strategic_deployment, base_extractiveness, 1517, 0.25).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causality__strategic_deployment, base_extractiveness, 1525, 0.42).
narrative_ontology:measurement(pres_be_t1535, press_reformation_causality__strategic_deployment, base_extractiveness, 1535, 0.55).
narrative_ontology:measurement(pres_be_t1545, press_reformation_causality__strategic_deployment, base_extractiveness, 1545, 0.61).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causality__strategic_deployment, base_extractiveness, 1555, 0.65).
narrative_ontology:measurement(pres_be_t1618, press_reformation_causality__strategic_deployment, base_extractiveness, 1618, 0.68).
narrative_ontology:measurement(pres_be_t1648, press_reformation_causality__strategic_deployment, base_extractiveness, 1648, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1517, press_reformation_causality__strategic_deployment, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causality__strategic_deployment, suppression_requirement, 1525, 0.5).
narrative_ontology:measurement(pres_su_t1535, press_reformation_causality__strategic_deployment, suppression_requirement, 1535, 0.62).
narrative_ontology:measurement(pres_su_t1545, press_reformation_causality__strategic_deployment, suppression_requirement, 1545, 0.68).
narrative_ontology:measurement(pres_su_t1555, press_reformation_causality__strategic_deployment, suppression_requirement, 1555, 0.7).
narrative_ontology:measurement(pres_su_t1618, press_reformation_causality__strategic_deployment, suppression_requirement, 1618, 0.72).
narrative_ontology:measurement(pres_su_t1648, press_reformation_causality__strategic_deployment, suppression_requirement, 1648, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__strategic_deployment, 0.03).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This is the strategic_deployment reading of the press_reformation_causality kernel. It decomposes the natural-language claim 'the press caused the Reformation' into a structurally precise constraint: reformers and printers used the press as a rope (coordination tool) to extract legitimacy and revenue from the Catholic Church (snare). The technological_determinism reading models the press as an autonomous mountain-like enabler; the co_constitution reading models feedback loops. All three share the kernel but instantiate different constraints with different ε values, stakeholder structures, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__strategic_deployment, institutional, 0.88).
constraint_indexing:directionality_override(press_reformation_causality__strategic_deployment, organized, 0.15).
constraint_indexing:directionality_override(press_reformation_causality__strategic_deployment, moderate, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
