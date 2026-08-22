% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Co-Constitution of Print and Reformation
 *   domain: history/religious_history/media_studies
 *
 * SUMMARY:
 *   Technology and human agency co-constituted the Reformation through
 *   feedback loops between the print economy and religious controversy.
 *   Printers invested in vernacular and controversial titles because
 *   reform-generated demand created profitable markets; reformers gained mass
 *   audiences that reshaped their theology and political strategy; the
 *   Catholic Church lost its interpretive monopoly as texts circulated beyond
 *   ecclesiastical control; secular princes consolidated territory by
 *   aligning with confessional movements; and manuscript copyists were
 *   displaced by press production. The arrangement coordinated a translocal
 *   public sphere while extracting authority from traditional institutions
 *   and livelihoods from obsolete skilled workers. Extraction was distributed
 *   across multiple seats rather than captured by any single agent.
 *
 * KEY AGENTS:
 *   - printers: Primary agenda-setters (moderate/constrained) â invested in presses, selected titles, bore confiscation risk, profited from bestsellers
 *   - reform_leaders: Primary beneficiaries (organized/constrained) â gained mass audience through print, became dependent on printers and princes
 *   - catholic_hierarchy: Primary target (institutional/constrained) â lost interpretive monopoly, funded counter-reformation and censorship
 *   - vernacular_readers: Coordinated beneficiaries (powerless/mobile) â gained textual access, paid for media, faced doctrinal competition
 *   - secular_princes: Secondary agenda-setters (powerful/arbitrage) â consolidated territory and Church wealth
 *   - manuscript_copyists: Secondary targets (powerless/trapped) â displaced by press technology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.65).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.68).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.65).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Co-Constitution of Print and Reformation").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9').
narrative_ontology:cs_kernel_codification('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', distributed).
narrative_ontology:cs_authority_grounding('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', distributed).
narrative_ontology:cs_reading_relation('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', press_reformation_causality__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', foundational, mutual_constitution_of_agency_and_technology).
narrative_ontology:cs_axiom_status(mutual_constitution_of_agency_and_technology, holdable).
narrative_ontology:cs_axiom_grounding('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', mutual_constitution_of_agency_and_technology, empirically_contingent).
narrative_ontology:cs_axiom('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', secondary, print_economy_as_structuring_practice).
narrative_ontology:cs_axiom_status(print_economy_as_structuring_practice, holdable).
narrative_ontology:cs_axiom_grounding('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', print_economy_as_structuring_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', symmetric_agency_technology_interaction).
narrative_ontology:cs_drift_state('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1ea0471d-e8c9-4e0c-8a21-4526cc58a8e9', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reform_leaders).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, vernacular_readers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, secular_princes).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, manuscript_copyists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invested capital in presses and type; selected titles based on theological controversy and market demand; profited from bestsellers but faced confiscation risks in Catholic territories; organized distribution networks across jurisdictions.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printers, agenda_setter,
    moderate, biographical, constrained, regional).

% Provided theological content that printers distributed; gained unprecedented mass audiences; became dependent on print networks for movement cohesion and on princely protection for survival; schism made return to Catholic communion dangerous or impossible.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reform_leaders, beneficiary,
    organized, generational, constrained, continental).

% Lost monopoly on scriptural interpretation and doctrinal arbitration; diverted resources to censorship, theological polemic, and the Counter-Reformation; faced territorial defections and revenue losses as princes embraced reform.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_hierarchy, payer,
    institutional, civilizational, constrained, global).

% Purchased pamphlets and vernacular Bibles; gained direct textual access previously mediated by clergy; exposed to competing doctrinal claims without traditional institutional guidance; social pressure to conform to territorial confession.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, vernacular_readers, beneficiary,
    powerless, biographical, mobile, regional).

% Leveraged religious controversy to consolidate territorial sovereignty, confiscate Church properties, and regulate religious institutions within their borders; mediated between imperial authority, local reform movements, and printing economies.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, secular_princes, agenda_setter,
    powerful, generational, arbitrage, regional).

% Skilled artisans whose livelihood depended on hand-copying texts; faced rapid skill obsolescence as print production scaled; lacked capital or training to transition to press operation.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, manuscript_copyists, payer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The print-reformation feedback loop coordinated the rapid, wide distribution of theological ideas across fragmented European polities, creating a translocal public sphere and enabling mass religious mobilization without centralized ecclesiastical control.
% TRANSFER_FUNCTION: Moves authority over scriptural interpretation from the Catholic hierarchy to a distributed network of printers, reformers, and literate laypeople; moves wealth from readers and the Church to printers and secular rulers; moves social control from ecclesiastical courts to territorial princes.
% ABSENT_VOICES: Illiterate laypeople who constituted the majority but were excluded from direct textual engagement; women whose theological voices were largely excluded from the print public sphere; peasants whose revolts were suppressed by reformers and princes alike despite shared use of print.
% DISAPPEARANCE_RATIONALE: Without the feedback loop between print economy and religious controversy, theological dissent would have remained localized and containable; the Church's interpretive monopoly would have persisted longer; secular princes would have lacked the ideological infrastructure for territorial religious consolidation.
% FOUNDING_PROBLEM: The late medieval Church faced growing criticism over indulgences, corruption, and centralized authority, but lacked mechanisms for rapid, scalable coordination of dissent across fragmented European polities.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historiographers of the book (Elizabeth Eisenstein, Adrian Johns) and Reformation scholars (Andrew Pettegree) attest the coordination problem and its resolution through print; Catholic historiography and earlier Marxist scholarship offered competing accounts of the founding problem's nature, corroborating that the problem was real but its characterization is disputed.
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is moderate-high because the arrangement systematically transferred authority and wealth from the Church and traditional scribes to printers, reformers, and princes, while locking lay populations into confessional camps. Suppression (0.68) reflects active enforcement by Church censorship, princely edicts, and confessional boundaries. Theater_ratio (0.48) captures the growing performative dimension: book burnings, public disputations, and printer self-presentation as divine agents became ritualized. Accessibility_collapse (0.50) indicates that once the print-reformation loop was established, returning to manuscript mediation or Church-only interpretation became politically and economically impossible in Protestant territories. Resistance (0.60) reflects sustained Catholic counter-reformation, imperial edicts against heresy, and peasant revolts. The metrics and claimed_type are authored independently: the co-constitution reading structurally fits tangled_rope, and the metrics describe the actual operation without tuning to match.
 *
 * PERSPECTIVAL GAP:
 *   From the printer's seat, the arrangement was a risky but profitable coordination of supply and demand in a new market. From the reformer's seat, it was an indispensable infrastructure for salvation and community formation. From the Catholic hierarchy's seat, it was an extractive attack on legitimate authority and social order. From the lay reader's seat, it was simultaneously liberating (direct access) and coercive (confessional identity lock). The engine will compute different directionalities: printers and reformers near the beneficiary end, the Church and copyists near the target end, readers near symmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (printers, reformers, readers, princes) derive low directionality because the constraint subsidizes their coordination or authority. Victims (catholic_hierarchy, manuscript_copyists) derive high directionality because the constraint extracts authority and livelihood from them. The derivation is unmodified by overrides because the structural relationships are clear from the historical record.
 *
 * MANDATROPHY ANALYSIS:
 *   The co-constitution reading prevents mandatrophy by refusing to reduce the Reformation to either pure coordination (the press as neutral tool) or pure extraction (reformers as cynical manipulators). Instead, it captures the hybrid: the press genuinely coordinated a mass movement, but that coordination was inseparable from the extraction of Church authority and the displacement of traditional scribes. The reading's mandate â explaining the Reformation through media history â remains live, but the specific co-constitution arrangement it describes was historically transient (the Reformation succeeded and became institutionalized). The story does not declare mandatrophy_resolved because the historiographical framework persists even though the historical arrangement it models is past.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_constitution_reading_location,
    'Does the co-constitution reading of press-reformation causality represent a distinct structural constraint, or is it a composite of scaffold and tangled-rope dynamics that should be decomposed?',
    'Decompose into separate constraint stories for the printing press as infrastructure (scaffold) and the printer-reformer-Church triad as extraction/coordination (tangled rope), then compare classification stability.',
    'If decomposable, this reading dissolves into a constraint family; if irreducibly composite, co-constitution stands as a tangled_rope with scaffold elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_constitution_reading_location, conceptual, 'Whether the co-constitution reading is atomically one constraint or a family').

omega_variable(
    distributed_extraction_beneficiary,
    'In the distributed extraction pattern of the Reformation print economy, which seat captured the majority of extracted value: printers through profit, reformers through authority, or secular princes through territorial consolidation?',
    'Economic history of the book trade (Pettegree, Febvre & Martin) plus prosopography of reformer-printer contracts and princely account books.',
    'Identifying a concentrated capturer would reclassify from diffuse tangled_rope toward snare; confirming diffusion supports the current classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_extraction_beneficiary, empirical, 'Distributed extraction beneficiary ambiguity').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the confessional identity lock that prevented exit from Catholic or Protestant territories a structural enforcement (princely law) or an internalized cognitive pattern (faith-as-identity)?',
    'Comparative analysis of conversion and emigration rates under different enforcement regimes; where suppression persists under lax enforcement, internalization dominates.',
    'Internalized suppression raises effective extraction for lay populations beyond the structural measure; structural suppression is bounded by territorial enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs internalized suppression in confessional identity').

omega_variable(
    kernel_reading_decomposition,
    'This constraint instantiates the co_constitution reading of the press_reformation_causality kernel. How would classifying the technological_determinism or strategic_deployment readings change the beneficiary structure and directionality assignments?',
    'Generate the sibling constraints and compare seat classifications across the family.',
    'Technological determinism would likely classify the press as mountain or rope with no victims; strategic deployment would classify as snare with concentrated reformer/printer beneficiaries and Church victim; co-constitution distributes directionality across all seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Sibling reading structural delta for this kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prc_co_tr_t0, press_reformation_causality__co_constitution, theater_ratio, 0, 0.2).
narrative_ontology:measurement(prc_co_tr_t8, press_reformation_causality__co_constitution, theater_ratio, 8, 0.28).
narrative_ontology:measurement(prc_co_tr_t16, press_reformation_causality__co_constitution, theater_ratio, 16, 0.35).
narrative_ontology:measurement(prc_co_tr_t24, press_reformation_causality__co_constitution, theater_ratio, 24, 0.42).
narrative_ontology:measurement(prc_co_tr_t32, press_reformation_causality__co_constitution, theater_ratio, 32, 0.45).
narrative_ontology:measurement(prc_co_tr_t40, press_reformation_causality__co_constitution, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(prc_co_be_t0, press_reformation_causality__co_constitution, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(prc_co_be_t8, press_reformation_causality__co_constitution, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(prc_co_be_t16, press_reformation_causality__co_constitution, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(prc_co_be_t24, press_reformation_causality__co_constitution, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(prc_co_be_t32, press_reformation_causality__co_constitution, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(prc_co_be_t40, press_reformation_causality__co_constitution, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(prc_co_su_t0, press_reformation_causality__co_constitution, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(prc_co_su_t8, press_reformation_causality__co_constitution, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(prc_co_su_t16, press_reformation_causality__co_constitution, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(prc_co_su_t24, press_reformation_causality__co_constitution, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(prc_co_su_t32, press_reformation_causality__co_constitution, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(prc_co_su_t40, press_reformation_causality__co_constitution, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, identity_coordination).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% This constraint is part of the press_reformation_causality family. The kernel decomposes into three structurally distinct readings: co_constitution (distributed hybrid), technological_determinism (autonomous technology), and strategic_deployment (intentional weaponization). Each reading carries a different epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
