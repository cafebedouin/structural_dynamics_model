% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Reformation as Technological Mediation via the Printing Press
 *   domain: historical/epistemological/religious
 *
 * SUMMARY:
 *   The Reformation as technological mediation treats Gutenberg's press not
 *   merely as context but as the fundamental constraint determining the scale
 *   and speed of religious dissent. In this reading, the physical properties
 *   of movable typeâidentical reproduction, lower per-unit cost, rapid
 *   continental distributionâconstitute a fixed technological substrate
 *   (presented as mountain-like) that transforms localized Wittenberg
 *   criticism into a mass movement. The constraint is CLAIMED as mountain
 *   (technological determinism) while the metrics and stakeholder structure
 *   reveal identifiable beneficiaries (printers, reformers, literate public)
 *   and victims (Catholic hierarchy, manuscript scribes), triggering
 *   false-summit evaluation. The claim/metric independence is maintained: the
 *   structural claim follows the technological-determinism historiography,
 *   while the metrics describe the constraint's actual extractive and
 *   coordinative operation.
 *
 * KEY AGENTS:
 *   - urban_printers: Primary agenda-setter (organized/mobile) â controlled the material means of reproduction and determined which texts scaled.
 *   - reformist_theologians: Primary beneficiary (moderate/constrained) â gained audiences impossible under manuscript/oral limits; structurally dependent on printers.
 *   - catholic_hierarchy: Primary target (institutional/constrained) â lost information monopoly and faced mass-produced dissent at scale.
 *   - manuscript_scribes: Secondary target (moderate/trapped) â bear technological obsolescence with minimal exit options.
 *   - literate_urban_public: Coordinated beneficiary (moderate/mobile) â gained access to dissenting ideas but paid for the medium.
 *   - illiterate_rural_populations: Excluded observer (powerless/trapped) â invisible to the print-mediated narrative despite comprising the demographic majority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.38).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.55).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Reformation as Technological Mediation via the Printing Press").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical/epistemological/religious").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '8023bb12-76d5-4137-905b-c79b056d4ed8').
narrative_ontology:cs_kernel_codification('8023bb12-76d5-4137-905b-c79b056d4ed8', distributed).
narrative_ontology:cs_authority_grounding('8023bb12-76d5-4137-905b-c79b056d4ed8', expertise).
narrative_ontology:cs_interpretation_layer_present('8023bb12-76d5-4137-905b-c79b056d4ed8').
narrative_ontology:cs_reading_relation('8023bb12-76d5-4137-905b-c79b056d4ed8', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_reading_relation('8023bb12-76d5-4137-905b-c79b056d4ed8', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_axiom('8023bb12-76d5-4137-905b-c79b056d4ed8', foundational, technological_mediation_primacy).
narrative_ontology:cs_axiom_status(technological_mediation_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8023bb12-76d5-4137-905b-c79b056d4ed8', technological_mediation_primacy, empirically_contingent).
narrative_ontology:cs_axiom('8023bb12-76d5-4137-905b-c79b056d4ed8', secondary, print_scale_as_mass_mobilization_enabler).
narrative_ontology:cs_axiom_status(print_scale_as_mass_mobilization_enabler, holdable).
narrative_ontology:cs_axiom_grounding('8023bb12-76d5-4137-905b-c79b056d4ed8', print_scale_as_mass_mobilization_enabler, empirically_contingent).
narrative_ontology:cs_reference_frame('8023bb12-76d5-4137-905b-c79b056d4ed8', print_culture_substrate).
narrative_ontology:cs_drift_state('8023bb12-76d5-4137-905b-c79b056d4ed8', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('8023bb12-76d5-4137-905b-c79b056d4ed8', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, urban_printers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, reformist_theologians).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_urban_public).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, catholic_hierarchy).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, manuscript_scribes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated presses in major European cities and chose which theological texts to reproduce and distribute. Controlled the material substrate that scaled dissent. Profited directly from the surge in demand for pamphlets and vernacular Bibles. Could relocate to more permissive territories or shift content based on market and political pressure.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, urban_printers, agenda_setter,
    organized, generational, mobile, continental).

% Gained audiences orders of magnitude larger than manuscript or oral transmission allowed. Their ideas scaled from local academic disputes to mass movements within months. Depended on printers for reach; abandoning print meant returning to ineffective personal networks and losing the public they had gained.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, reformist_theologians, beneficiary,
    moderate, biographical, constrained, continental).

% Lost the information monopoly that had enabled centralized doctrinal control. Faced coordinated, mass-produced dissent that outpaced the production capacity of Catholic counter-polemic. Could not suppress the technology without major political and economic costs; attempted censorship and licensing but could not eliminate the physical possibility of reproduction.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, catholic_hierarchy, payer,
    institutional, generational, constrained, continental).

% Livelihood and social function collapsed as print economics undercut the cost and speed of manuscript production. Skills developed over lifetimes became obsolete within a generation. Few alternative occupations matched their status or income; many were absorbed into print shops at lower wages or left the literate trades entirely.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, manuscript_scribes, payer,
    moderate, biographical, trapped, regional).

% Gained unprecedented access to theological debates previously restricted to clerical elites. Could compare competing claims, read vernacular scripture, and participate in public discourse. Paid for pamphlets and books; bore the indirect cost of social upheaval but received informational benefit.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_urban_public, beneficiary,
    moderate, biographical, mobile, regional).

% Comprised the demographic majority but could not access printed dissent directly. Their religious experience remained oral, visual, and local. Excluded from the continental mass movement except through second-hand oral transmission or elite mobilization. Their experience is invisible to publication-rate observables.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, illiterate_rural_populations, excluded,
    powerless, generational, trapped, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables mass reproduction and distribution of theological texts, solving the collective-action problem of synchronizing dissent across geographically dispersed European populations without relying on centralized ecclesiastical infrastructure or fragile personal networks.
% TRANSFER_FUNCTION: Moves textual authority and audience attention from monastic scriptoria and ecclesiastical oral transmission to decentralized urban print shops; moves economic surplus from manuscript patrons and institutional church purchasers to printer-publishers and paper merchants.
% ABSENT_VOICES: Illiterate rural majorities, oral-tradition bearers, women mystics operating outside literate channels, and manuscript scribes are excluded from the technological mediation; their religious experience is invisible to publication-rate observables and marginal to the print-defined narrative of the Reformation.
% DISAPPEARANCE_RATIONALE: Without the press, Luther's dissent remains a localized Wittenberg academic dispute; the continental mass movement dissolves back into personal networks and oral transmission too slow and fragile to challenge imperial-papal institutional authority at scale. The political and theological dimensions of the Reformation lose their mass base.
% FOUNDING_PROBLEM: How to coordinate religious dissent across hundreds of miles when ecclesiastical authority controls institutional communication, personal networks are geographically limited, and manuscript production is too slow and expensive to sustain mass mobilization against a transnational institution.
% FOUNDING_PROBLEM_CORROBORATION: Bibliographers and historians of technology attest to the correlation between print output and Reformation diffusion. Corroboration from outside the benefiting parties (printers, reformers) comes from secular political historians and economic historians who document print's structural role independent of theological content; however, social historians of religion contest the primacy of print, noting the continuation of robust popular piety and oral tradition that the technological reading marginalizes.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.38, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.38) is moderate-low because the press's primary effect is coordination, but the asymmetry between printers/reformers (who capture audience and revenue) and the Catholic hierarchy (which loses gatekeeping rents) introduces genuine extraction. Suppression (0.55) tracks the maturation of licensing and censorship regimes that actively suppressed rival manuscript and oral channels; it is a raw structural property, not scaled by power or scope. Theater ratio (0.20) is low because the coordination function is genuineâprinted texts did synchronize dissentâbut rises slightly as print privileges and confessional control bureaucratize. Accessibility collapse (0.88) is high because manuscript culture rapidly became non-viable as an alternative once print economics took hold, mimicking mountain-like alternative collapse despite the technology's human origin. Resistance (0.45) reflects sustained institutional counter-pressure from the Church, which distinguishes this from a genuine physical law.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (urban_printers) and beneficiary seats (reformers, literate public) compute low directionalityâthey experience the constraint as enabling infrastructure. The payer seats (Catholic hierarchy, manuscript scribes) compute high directionality, experiencing the same technology as extractive displacement. The excluded seat (illiterate rural populations) experiences neither benefit nor cost directly; their absence from the constraint's operation is itself a structural feature. The engine will compute per-seat classifications: mountain-like from the printer/reformer perspective, snare-like or tangled-rope-like from the defeated institutional perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to agents who captured the information-scaling surplus: printers captured revenue, reformers captured audience, and the literate public captured access. Victim declarations map to agents who lost monopoly rents or livelihoods. The directionality derivation chain produces low d for beneficiaries (subsidized by the constraint) and high d for victims (extracted by the constraint). No overrides are necessary because the structural relationships are transparent: printers and reformers have mobile or constrained exit but are named beneficiaries; the Church has constrained exit and is named victim; scribes have trapped exit and are named victim.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the technological mediation as pure extraction (snare) by preserving its genuine coordination functionâdissenters genuinely needed a coordination mechanism to scale. It also prevents mislabeling it as pure coordination (rope) by documenting the asymmetric capture of gains by printers and reformers and the displacement costs borne by manuscript scribes and the Catholic hierarchy. The mountain claim is the most natural mislabeling: treating a human technology with identifiable beneficiaries as an irreducible physical limit. The FSM signature is designed to catch exactly this move, reclassifying toward tangled_rope when beneficiaries are present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_agency,
    'Is the printing press''s role in scaling dissent an irreducible physical property of the technology (mountain), or a constructed constraint benefiting identifiable printers and reformers?',
    'Comparative analysis of Reformation-like movements without print technology (e.g., earlier medieval heresies) to determine if the scale is technologically determined or politically/theologically constructed.',
    'If agency-driven, the constraint is a tangled rope or snare rather than a mountain; the FSM reclassification is warranted and the false-summit evaluation resolves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_agency, conceptual, 'Whether the press operates as autonomous physical driver or as instrument of identifiable beneficiaries.').

omega_variable(
    publication_rate_causality,
    'Do publication rates and literacy data demonstrate technological causation, or merely correlation with political fragmentation and pre-existing theological dissent?',
    'Counterfactual analysis and bibliometric regression controlling for political fragmentation, urbanization, and pre-existing popular anticlericalism.',
    'If correlation only, the mountain claim collapses and the constraint reclassifies toward rope or scaffold; if causation is established, the technological-determinism claim is empirically reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_rate_causality, empirical, 'Whether publication observables track causation or correlation.').

omega_variable(
    infrastructure_vs_determinism_framing,
    'Does this reading frame the press as passive infrastructure (rope) or as autonomous historical driver (mountain)?',
    'Examine whether the reading treats the press as enabling condition or sufficient cause by analyzing how it handles cases where print did not produce Reformation-scale outcomes (e.g., early humanist printing).',
    'Infrastructure framing lowers extractiveness and reclassifies toward rope; determinism framing maintains mountain claim but risks FSM when beneficiaries are structurally present.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_vs_determinism_framing, conceptual, 'Under-determination of the coordination-extraction boundary in CS framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_composite__technological_mediation_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(refo_tr_t10, reformation_composite__technological_mediation_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(refo_tr_t20, reformation_composite__technological_mediation_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(refo_tr_t30, reformation_composite__technological_mediation_reading, theater_ratio, 30, 0.16).
narrative_ontology:measurement(refo_tr_t40, reformation_composite__technological_mediation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(refo_tr_t50, reformation_composite__technological_mediation_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_composite__technological_mediation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(refo_be_t10, reformation_composite__technological_mediation_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(refo_be_t20, reformation_composite__technological_mediation_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(refo_be_t30, reformation_composite__technological_mediation_reading, base_extractiveness, 30, 0.3).
narrative_ontology:measurement(refo_be_t40, reformation_composite__technological_mediation_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(refo_be_t50, reformation_composite__technological_mediation_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_composite__technological_mediation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(refo_su_t10, reformation_composite__technological_mediation_reading, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(refo_su_t20, reformation_composite__technological_mediation_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(refo_su_t30, reformation_composite__technological_mediation_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(refo_su_t40, reformation_composite__technological_mediation_reading, suppression_requirement, 40, 0.51).
narrative_ontology:measurement(refo_su_t50, reformation_composite__technological_mediation_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the reformation_composite kernel, decomposed per the epsilon-invariance principle. The technological_mediation_reading treats print infrastructure as the primary causal driver, while sibling readings assign primacy to political sovereignty assertions or theological incompatibilities. Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
