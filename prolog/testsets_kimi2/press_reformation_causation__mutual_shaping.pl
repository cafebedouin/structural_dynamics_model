% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Printing Press and Reformation Mutual Shaping
 *   domain: history of technology/religious history/media studies
 *
 * SUMMARY:
 *   This constraint story models the press-reformation nexus as a
 *   co-evolutionary scaffold: the printing press created communicative
 *   possibilities that reformers exploited, while reformist demand and
 *   content in turn directed technical innovation, genre development, and the
 *   geographic diffusion of presses. The arrangement is neither a mountain
 *   (technology did not inevitably cause the Reformation) nor a pure rope
 *   (the dynamic was not cost-free or symmetric). It functioned as a
 *   transitional enabling structure that displaced an established information
 *   order and was itself transformed as it institutionalized. As a kernel
 *   reading, it instantiates the mutual_shaping reading of the
 *   press_reformation_causation kernel, distinct from technological
 *   determinism and strategic deployment siblings.
 *
 * KEY AGENTS:
 *   - Vernacular printers (agenda_setter/beneficiary): organized, mobile across jurisdictions, set production agendas in response to reform demand.
 *   - Reformist movements (agenda_setter/beneficiary): organized, identity-locked, authored content while being shaped by print affordances.
 *   - Established ecclesiastical authority (payer): institutional, constrained, lost information monopoly.
 *   - Manuscript producers (payer): powerless, trapped, displaced by scale.
 *   - Urban reading public (beneficiary): moderate, constrained, directed demand.
 *   - Rural illiterate population (excluded): powerless, trapped, outside the textual public sphere.
 *   - Book historians (observer): analytical seat reconstructing the dynamic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.42).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.38).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Printing Press and Reformation Mutual Shaping").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history of technology/religious history/media studies").

domain_priors:requires_active_enforcement(press_reformation_causation__mutual_shaping).
narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'fb8bd6f3-afef-4da4-a0cd-f090e339ef59').
narrative_ontology:cs_kernel_codification('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', distributed).
narrative_ontology:cs_authority_grounding('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', expertise).
narrative_ontology:cs_interpretation_layer_present('fb8bd6f3-afef-4da4-a0cd-f090e339ef59').
narrative_ontology:cs_reading_relation('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', press_reformation_causation__strategic_deployment, influences).
narrative_ontology:cs_axiom('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', foundational, media_agency_codetermination).
narrative_ontology:cs_axiom_status(media_agency_codetermination, holdable).
narrative_ontology:cs_axiom_grounding('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', media_agency_codetermination, empirically_contingent).
narrative_ontology:cs_axiom('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', foundational, technological_affordance_constraint).
narrative_ontology:cs_axiom_status(technological_affordance_constraint, holdable).
narrative_ontology:cs_axiom_grounding('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', technological_affordance_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', coevolutionary_equilibrium).
narrative_ontology:cs_drift_state('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', contemporary_book_history, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('fb8bd6f3-afef-4da4-a0cd-f090e339ef59', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformist_movements).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, urban_reading_public).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, established_ecclesiastical_authority).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, manuscript_producers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operated presses across European cities, selecting and producing texts in response to reform-driven demand. Profited from explosive markets for pamphlets and vernacular Bibles. Relocated among jurisdictions to evade censorship and find favorable regulatory environments, actively shaping the technical and geographic development of the press.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, vernacular_printers, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, vernacular_printers, beneficiary).

% Authored theological content that created and sustained print demand. Their religious identity was inseparable from the reform cause; exit meant recantation or exile. Directed printer output through patronage, correspondence, and doctrinal authority while being shaped by the medium's affordances for mass vernacular argument.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformist_movements, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, reformist_movements, beneficiary).

% Gained access to vernacular religious texts, pamphlets, and broadsheets previously mediated by clergy. Their literacy levels and purchasing power directed printer production toward popular reform themes, creating a feedback loop between reader demand and output.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, urban_reading_public, beneficiary,
    moderate, biographical, constrained, regional).

% Lost the information monopoly and doctrinal gatekeeping function supported by Latin manuscript culture. Invested in censorship and licensing apparatus but could not suppress the distributed, cross-border print network that reformers and printers maintained.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, established_ecclesiastical_authority, payer,
    institutional, civilizational, constrained, continental).

% Experienced collapse of demand for hand-copied texts as print scaled. Trapped by guild-structured skill specificity in a disappearing craft with limited retraining pathways.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, manuscript_producers, payer,
    powerless, biographical, trapped, local).

% Constituted the majority of the population but was excluded from the textual public sphere created by the press-reformation nexus. Their religious experience remained orally mediated; they had no direct voice in print discourse.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, rural_illiterate_population, excluded,
    powerless, biographical, trapped, local).

% Observe the structural relationship between media technology and religious transformation from a temporal and analytic distance, reconstructing the causal dynamics from archival and material evidence.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, book_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated the mass production and distribution of vernacular religious ideas with the organizational energy of reform movements, solving the problem of how to disseminate dissent and vernacular scripture at scale across a politically fragmented continent.
% TRANSFER_FUNCTION: Moved textual authority and economic value from ecclesiastical manuscript centers and Latin literacy gatekeepers to vernacular print shops and reformist networks; transferred readership from clerical mediation to direct lay engagement.
% ABSENT_VOICES: The rural illiterate majority, women excluded from Latin education and public theological authorship, and orthodox lay Catholics in reformist territories who lost traditional religious media without gaining voice in the new print sphere.
% DISAPPEARANCE_RATIONALE: Without the mutual shaping dynamic, the Reformation would have remained localized academic dissent; European religious geography would not have polarized so rapidly, the public sphere would not have vernacularized at this speed, and the print economy would not have reoriented toward mass pamphleteering.
% FOUNDING_PROBLEM: How to communicate religious dissent and vernacular scripture across a politically fragmented continent with weak central authority and strong ecclesiastical information control.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of the book (Febvre, Martin, Eisenstein) and communication scholars attest the problem from outside the benefiting parties; Catholic historiography acknowledges the information-control crisis but disputes that the print-reformation symbiosis was the legitimate or necessary solution.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is moderate: the arrangement genuinely solved a coordination problem (mass vernacular communication) while extracting authority and economic value from the established Church and manuscript producers. Suppression (0.38) reflects the active suppression of the old information order rather than suppression of participants within the new arrangement. Theater ratio (0.25) is low because the coordination was materially functional, though some performative maintenance of reformist solidarity existed. Accessibility collapse (0.45) captures how manuscript and oral mediation became structurally non-viable at scale once the print-reformation nexus was established. Resistance (0.55) is moderate-to-high because the displaced authority structure actively contested the arrangement through censorship, doctrinal counter-production, and institutional reform (Counter-Reformation). The temporal series show extraction and suppression peaking during the height of confessional conflict (1520s-1530s) and declining as the scaffold institutionalized into territorialized religious orders by the 1550s.
 *
 * PERSPECTIVAL GAP:
 *   The printer and reformer seats experience the constraint as enabling coordination that they actively maintain; the established church and manuscript producers experience it as extractive displacement. The engine will compute divergent per-seat classifications from this structural asymmetry: the beneficiary seats should compute toward coordination types while the payer seats compute toward extraction types, with the overall constraint resolving as scaffold due to its transitional intent and sunset trajectory.
 *
 * DIRECTIONALITY LOGIC:
 *   Printers and reformist movements sit near the beneficiary end: they receive economic, ideological, and organizational returns from the arrangement. Their mobile or identity-locked exit options modulate their directionality â mobile printers retain some arbitrage but are locked into the reform economy; reformers are identity-fused to the movement. The established church and manuscript producers sit near the target end: they bear the costs of lost monopoly and displacement. The urban reading public sits nearer symmetric, gaining cultural access while paying indirectly for texts and living with confessional instability.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling the arrangement as pure extraction (snare) because the press genuinely coordinated a new public sphere and the reformers were not merely extracting from the Church but building an alternative communicative infrastructure. It also prevents mislabeling as pure rope because the displacement of manuscript producers and the Church's information monopoly was real asymmetric extraction, not a symmetric collective-action solution. The sunset clause is essential: the intense mutual shaping phase was transitional, meant to establish new religious and media institutions, not to persist indefinitely. By 1555 (Peace of Augsburg) and the emergence of confessionalized states, the founding problem was dead and the scaffold had either dissolved into new institutional ropes or decayed into piton-like ceremonial remnants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_vs_inertial_persistence,
    'Did the press-reformation mutual shaping arrangement genuinely sunset into institutionalized religious and print orders, or did it persist inertially as a piton beyond its transitional function?',
    'Trace whether the specific symbiotic practices of the 1520s-1540s (pamphlet warfare, prince-protected smuggling networks, reformist printer cartels) dissolved after the Peace of Augsburg and the Index of Prohibited Books, or persisted as theatrical remnants.',
    'If inertial persistence dominated, the constraint''s classification shifts toward piton; if genuine transition occurred, scaffold classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_vs_inertial_persistence, empirical, 'Whether the scaffold genuinely sunset or decayed into inertial performance.').

omega_variable(
    coordination_extraction_separability,
    'Could the reformers'' message have achieved comparable distribution without the specific extractive displacement of manuscript producers and ecclesiastical authority?',
    'Counterfactual analysis comparing reform success in territories with weaker print infrastructure versus strong urban print networks.',
    'If distribution required displacement, coordination and extraction are structurally inseparable (tangled rope tendency); if separable, scaffold classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction in the press-reformation nexus are structurally separable.').

omega_variable(
    sibling_reading_relation_ambiguity,
    'Does the mutual shaping reading structurally foreclose technological determinism, or merely represent a different weighting within a shared causal framework?',
    'Examine whether empirical findings about print causation logically contradict mutual shaping premises or merely redistribute causal weight.',
    'If foreclosing, the kernel has a resolved logical structure and epsilon variance across readings is high; if coexisting, the constraint family remains contested with overlapping classification boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_relation_ambiguity, conceptual, 'Structural relationship between mutual shaping and technological determinism readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__mutual_shaping, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pres_tr_t8, press_reformation_causation__mutual_shaping, theater_ratio, 8, 0.2).
narrative_ontology:measurement(pres_tr_t16, press_reformation_causation__mutual_shaping, theater_ratio, 16, 0.28).
narrative_ontology:measurement(pres_tr_t24, press_reformation_causation__mutual_shaping, theater_ratio, 24, 0.25).
narrative_ontology:measurement(pres_tr_t32, press_reformation_causation__mutual_shaping, theater_ratio, 32, 0.22).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causation__mutual_shaping, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__mutual_shaping, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pres_be_t8, press_reformation_causation__mutual_shaping, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(pres_be_t16, press_reformation_causation__mutual_shaping, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(pres_be_t24, press_reformation_causation__mutual_shaping, base_extractiveness, 24, 0.45).
narrative_ontology:measurement(pres_be_t32, press_reformation_causation__mutual_shaping, base_extractiveness, 32, 0.42).
narrative_ontology:measurement(pres_be_t40, press_reformation_causation__mutual_shaping, base_extractiveness, 40, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causation__mutual_shaping, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(pres_su_t8, press_reformation_causation__mutual_shaping, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(pres_su_t16, press_reformation_causation__mutual_shaping, suppression_requirement, 16, 0.55).
narrative_ontology:measurement(pres_su_t24, press_reformation_causation__mutual_shaping, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(pres_su_t32, press_reformation_causation__mutual_shaping, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(pres_su_t40, press_reformation_causation__mutual_shaping, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
