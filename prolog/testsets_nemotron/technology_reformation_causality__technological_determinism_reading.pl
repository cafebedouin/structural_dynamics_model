% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__technological_determinism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__technological_determinism_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: technology_reformation_causality__technological_determinism_reading
 *   human_readable: Printing Press as Inevitable Driver of Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the technological determinism reading
 *   of the technology_reformation_causality kernel. The claim is that the
 *   printing press, as a material technology with fixed physical properties
 *   (reproducibility, cost reduction, error elimination), structurally
 *   necessitated the Reformation by making mass vernacular scripture
 *   distribution inevitable. The press is treated as a mountain — a physical
 *   constraint that exists independent of human intention, whose operation
 *   (drastically lowering the cost and increasing the speed and fidelity of
 *   text reproduction) created a new material reality that the Reformation
 *   was forced to adapt to. Reformers, printers, and ecclesiastical
 *   authorities are downstream adapters to this physical fact. The ε is
 *   near-zero because the constraint itself extracts nothing; it is a
 *   production cost reduction, not a mechanism of transfer. The claimed_type
 *   is mountain because the press's physical operation would persist
 *   regardless of who defends it or whether anyone enforces it, and no party
 *   collects rents from its operation as a physical fact. The high
 *   accessibility_collapse (0.92) reflects that once movable type exists, the
 *   alternative of manuscript culture is functionally unrecoverable at scale.
 *   Low resistance (0.03) reflects that the physical process meets no active
 *   opposition — opposition is to the social consequences, not the technology
 *   itself.
 *
 * KEY AGENTS:
 *   - printing_press_technology: Mountain agent (physical constraint) — the material apparatus of movable type, its cost structure, and its reproductive fidelity
 *   - reformers: Downstream adapters (powerful/organized/biographical/constrained) — Luther, Calvin, Zwingli et al. who leveraged the press's output but did not create its material properties
 *   - printers_publishers: Downstream adapters (organized/moderate/biographical/mobile) — commercial operators who deployed the technology for profit and ideological alignment
 *   - catholic_church_authority: Downstream adapter (institutional/generational/constrained) — the pre-existing authority structure forced to respond to a material reality it did not create
 *   - vernacular_readers: Beneficiaries of cost reduction (moderate/biographical/constrained) — lay populations gaining access to scripture in their own languages
 *   - analytical_observer: Observer (analytical/civilizational/analytical/universal) — historians and media theorists evaluating the causal claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__technological_determinism_reading, 0.05).
domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, 0.02).
domain_priors:theater_ratio(technology_reformation_causality__technological_determinism_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__technological_determinism_reading, mountain).
narrative_ontology:human_readable(technology_reformation_causality__technological_determinism_reading, "Printing Press as Inevitable Driver of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(technology_reformation_causality__technological_determinism_reading, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__technological_determinism_reading, '0668ec5b-13a8-49fd-9f58-b4bcf92a1809').
narrative_ontology:cs_kernel_codification('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', distributed).
narrative_ontology:cs_authority_grounding('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', distributed).
narrative_ontology:cs_reading_relation('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', technology_reformation_causality__beneficiary_agency_reading, coexists_with).
narrative_ontology:cs_reading_relation('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', technology_reformation_causality__co_constitution_reading, coexists_with).
narrative_ontology:cs_axiom('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', foundational, material_conditions_determine_historical_outcomes).
narrative_ontology:cs_axiom_status(material_conditions_determine_historical_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', material_conditions_determine_historical_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', foundational, technology_as_mountain_not_tool).
narrative_ontology:cs_axiom_status(technology_as_mountain_not_tool, holdable).
narrative_ontology:cs_axiom_grounding('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', technology_as_mountain_not_tool, conventional).
narrative_ontology:cs_reference_frame('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', press_as_physical_fact).
narrative_ontology:cs_drift_state('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', contemporary_media_theory, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0668ec5b-13a8-49fd-9f58-b4bcf92a1809', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, printers_publishers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__technological_determinism_reading, vernacular_readers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, reformers).
narrative_ontology:constraint_victim(technology_reformation_causality__technological_determinism_reading, catholic_church_authority).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, technological_determinism_in_religious_change).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, material_conditions_determine_ideological_outcomes).
narrative_ontology:constraint_vindicates(technology_reformation_causality__technological_determinism_reading, vernacular_scripture_necessarily_fractures_religious_unity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The material technology of movable type printing: fixed physical properties of reproducibility, cost reduction, error elimination, and speed. It does not act, choose, or collect; it simply operates according to its physics. All human agents are downstream of its material facts.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, printing_press_technology, observer,
    analytical, civilizational, analytical, universal).

% Religious leaders (Luther, Calvin, Zwingli, et al.) who leveraged the press's output to disseminate vernacular scripture and polemic. They bore the costs of writing, translating, and defending their works; their exit from the press's logic was constrained by their doctrinal commitment to scripture accessibility. They did not create the press's material properties but adapted their movement to them.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, reformers, payer,
    powerful, biographical, constrained, continental).

% Commercial operators who owned and ran presses. They profited from the surge in demand for vernacular texts, religious polemic, and pamphlets. Their exit was mobile — they could move cities, switch patrons, or change product lines — but their business model depended on the press's material cost advantage over manuscripts.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, printers_publishers, beneficiary,
    organized, biographical, mobile, continental).

% The pre-existing ecclesiastical authority structure forced to respond to mass vernacular scripture circulation. It bore the costs of losing doctrinal control, censorship efficacy, and the Latin liturgy's monopoly. Its exit from the press's logic was constrained by institutional inertia and the impossibility of un-inventing the technology.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, catholic_church_authority, payer,
    institutional, generational, constrained, continental).

% Lay populations gaining access to scripture in their own languages at drastically reduced cost. They benefited from the press's cost reduction but were constrained by literacy rates, availability, and ecclesiastical discouragement. Their exit from the old manuscript-mediated access was effectively closed once printed vernacular Bibles existed.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, vernacular_readers, beneficiary,
    moderate, biographical, constrained, continental).

% Historians and media theorists evaluating the causal claim that the press made the Reformation inevitable. They bear no costs and collect no benefits from the press's operation; they analyze the structural relationship from outside the historical moment.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__technological_determinism_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__technological_determinism_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_reformation_causality__technological_determinism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The printing press solves the coordination problem of high-fidelity, low-cost, large-scale text reproduction — replacing the manuscript bottleneck with a mechanical process that coordinates the production of identical copies across distance and time without centralized scribal control.
% TRANSFER_FUNCTION: The press as physical technology transfers nothing between social actors; it reduces the marginal cost of text reproduction for all users symmetrically. The transfer function belongs to the *content layer* (what gets printed and by whom), not the press itself.
% ABSENT_VOICES: Scribes and manuscript illuminators — their livelihood was structurally displaced by the press but they were not in the room when the press's adoption was decided. Oral tradition communities — their epistemic authority was undermined by text fixation. Women and lower-class populations — their access to vernacular scripture was mediated through male, literate, often clerical intermediaries even after printing.
% DISAPPEARANCE_RATIONALE: If the printing press vanished overnight in 1517, the Reformation as historically realized — mass vernacular scripture, rapid pamphlet wars, synchronized doctrinal dissemination across principalities — would not have occurred. The world would rearrange: manuscript culture would resume, doctrinal control would recentralize, the speed and scale of religious fracture would collapse. The press's physical operation is a load-bearing constraint on the historical outcome.
% FOUNDING_PROBLEM: The founding problem was the manuscript bottleneck: the extreme cost, slowness, and error rate of hand-copying texts, which made mass scripture distribution physically impossible and kept textual authority concentrated in Latin-literate clerical hands.
% FOUNDING_PROBLEM_CORROBORATION: The manuscript bottleneck as a *physical* problem is dead — movable type solved it permanently. No serious historian argues manuscript culture could return as a mass medium. This is corroborated by the universal adoption of print (and later digital) reproduction across all textual cultures. The status 'dead' applies to the *physical* founding problem; the *social* problems of textual authority, access, and control persist in new forms (digital platforms, algorithmic curation, paywalls).
narrative_ontology:disappearance_verdict(technology_reformation_causality__technological_determinism_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__technological_determinism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__technological_determinism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(technology_reformation_causality__technological_determinism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__technological_determinism_reading, 0.05, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__technological_determinism_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, ExtMetricName, E),
    domain_priors:suppression_score(technology_reformation_causality__technological_determinism_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(technology_reformation_causality__technological_determinism_reading),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(technology_reformation_causality__technological_determinism_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(technology_reformation_causality__technological_determinism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics describe the printing press as a physical technology, not as a social arrangement. Extractiveness (0.05) captures only the marginal resource cost of operating presses — ink, paper, labor — not a transfer from one social group to another. Suppression (0.02) reflects that the press itself requires no enforcement to operate; its physics is self-executing. Theater ratio (0.01) is negligible because there is no performative maintenance of the technology's core function. Accessibility collapse (0.92) is high because manuscript culture cannot be restored as a mass medium once movable type exists — the alternative is structurally foreclosed at the material level. Resistance (0.03) is near-zero because the physical process of printing meets no resistance; resistance attaches to the *content* printed, not the press itself. The claimed_type mountain reflects that the press's material operation is a fixed constraint: it would persist regardless of human institution, extracts nothing from social actors, and no party benefits from its operation as a physical fact (though parties benefit from its *outputs*).
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from structural data. For this reading, all human agents are downstream adapters to a mountain. Their exit_options differ (reformers: constrained by doctrinal commitment; printers: mobile commercial operators; Church: constrained by institutional inertia; readers: constrained by literacy and access), but their directionality toward the *press as physical constraint* is symmetric (d ≈ 0.5) — the press subsidizes all by lowering production cost, extracts from none. The seat divergence the prompt template anticipates (payer vs beneficiary) does not apply here because the constraint is not a social arrangement with extraction. The divergence appears instead in the *content layer* — what gets printed — which is a different constraint (or family of constraints).
 *
 * DIRECTIONALITY LOGIC:
 *   No beneficiaries or victims are declared because the printing press as a physical technology extracts from no one and subsidizes everyone equally by lowering the marginal cost of text reproduction. The directionality derivation chain therefore assigns d ≈ 0.5 (symmetric) to all human agents relative to the press-as-mountain. The reformers, printers, Church, and readers all face the same physical fact: text reproduction is now cheap, fast, and high-fidelity. Their divergent interests emerge in how they *use* that fact, not in their structural relationship to the fact itself. This is the core move of the technological determinism reading: the technology is a mountain; the social contest is downstream.
 *
 * MANDATROPHY ANALYSIS:
 *   Not applicable — the press as mountain has no mandate that could atrophy. The mandatrophy question applies to the social arrangements built *on* the press (indulgence systems, censorship regimes, confessional publishing networks), which are separate constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_vs_social_causality,
    'Does the printing press''s material operation alone suffice to make the Reformation inevitable, or do the sibling readings'' claims (strategic deployment, co-constitution) describe necessary causal factors that this reading omits?',
    'Counterfactual historiography: would the Reformation have occurred with the same speed, scale, and doctrinal shape if the press existed but reformers had not strategically used it, or if the press had been restricted to Latin texts? Comparative cases: printing in China/Korea did not produce a Reformation; printing in Ottoman lands was restricted for centuries.',
    'If the sibling readings identify necessary causal factors, this reading''s mountain claim overstates the press''s causal sufficiency — the press would be a necessary but not sufficient condition, reclassifying from mountain to rope or tangled_rope in the broader constraint family. If this reading''s sufficiency claim holds, the sibling readings describe epiphenomenal downstream adaptations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_vs_social_causality, conceptual, 'Whether the press as mountain is causally sufficient for the Reformation, or whether social agency and co-evolution are necessary components of the causal story.').

omega_variable(
    reading_relations_structural_boundary,
    'Does the technological determinism reading''s core premise (technology as mountain, humans as downstream) logically foreclose the beneficiary_agency_reading''s core premise (humans as strategic deployers, technology as tool), or do they coexist as competing frameworks?',
    'Formal analysis of the logical compatibility: can a single historiographical framework hold both that the press''s material properties *alone* determined the outcome AND that reformers'' strategic choices were causally efficacious in ways the press''s material properties do not explain? If yes → coexists_with; if no → forecloses.',
    'If forecloses, the kernel contains a genuine logical fracture between readings; if coexists_with, the kernel hosts a persistent interpretive dispute with no structural resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_relations_structural_boundary, conceptual, 'Structural relationship between this reading and the beneficiary_agency_reading: forecloses or coexists_with.').

omega_variable(
    press_as_mountain_vs_social_arrangement,
    'Is the printing press correctly modeled as a mountain (physical constraint with zero social extraction), or does the press-as-deployed-in-16th-century-Europe inevitably embed social arrangements (guild monopolies, censorship, licensing, capital concentration) that make it a tangled_rope or snare?',
    'Historical analysis of the actual deployment conditions: were presses operated as open physical facts, or were they embedded in regulatory, commercial, and political structures that extracted from some parties and benefited others? The press-as-physics vs press-as-deployed distinction.',
    'If the deployed press was inextricably embedded in extractive social arrangements, this reading''s mountain claim describes a physics abstraction, not the historical constraint that actually operated. The historical constraint would be a different story (or family) with non-zero ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_as_mountain_vs_social_arrangement, empirical, 'Whether the mountain model applies to the physical technology abstractly or to the historically deployed press in its social embedding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__technological_determinism_reading, 1450, 1560).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_reform_det_tr_t1450, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1450, 0.01).
narrative_ontology:measurement(tech_reform_det_tr_t1480, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1480, 0.01).
narrative_ontology:measurement(tech_reform_det_tr_t1517, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1517, 0.01).
narrative_ontology:measurement(tech_reform_det_tr_t1530, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1530, 0.01).
narrative_ontology:measurement(tech_reform_det_tr_t1560, technology_reformation_causality__technological_determinism_reading, theater_ratio, 1560, 0.01).

% Extraction over time
narrative_ontology:measurement(tech_reform_det_be_t1450, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1450, 0.05).
narrative_ontology:measurement(tech_reform_det_be_t1480, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1480, 0.05).
narrative_ontology:measurement(tech_reform_det_be_t1517, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1517, 0.05).
narrative_ontology:measurement(tech_reform_det_be_t1530, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1530, 0.05).
narrative_ontology:measurement(tech_reform_det_be_t1560, technology_reformation_causality__technological_determinism_reading, base_extractiveness, 1560, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tech_reform_det_su_t1450, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1450, 0.02).
narrative_ontology:measurement(tech_reform_det_su_t1480, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1480, 0.02).
narrative_ontology:measurement(tech_reform_det_su_t1517, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1517, 0.02).
narrative_ontology:measurement(tech_reform_det_su_t1530, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1530, 0.02).
narrative_ontology:measurement(tech_reform_det_su_t1560, technology_reformation_causality__technological_determinism_reading, suppression_requirement, 1560, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__technological_determinism_reading, information_standard).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__beneficiary_agency_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, technology_reformation_causality__co_constitution_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, vernacular_scripture_distribution_network).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, indulgence_system_disruption).
narrative_ontology:affects_constraint(technology_reformation_causality__technological_determinism_reading, censorship_regime_adaptation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the technology_reformation_causality kernel. The technological determinism reading models the press as a mountain (physical constraint); the beneficiary_agency_reading models strategic human deployment as primary; the co_constitution_reading models mutual shaping. They share the referent (printing press + Reformation) but author different ε, different beneficiary/victim structures, and different claimed types. Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
