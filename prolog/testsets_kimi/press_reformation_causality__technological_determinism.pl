% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Technological Determinism Reading of Press-Reformation Causality
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   This constraint story models the technological determinism reading of
 *   press-reformation causality: the thesis that the printing press
 *   functioned as an autonomous, mountain-like historical force making
 *   vernacular scripture spread and Reformation success inevitable. Within
 *   this reading, technology sits upstream as a fixed causal engine and human
 *   actorsâprinters, reformers, readersâare downstream responders whose
 *   agency is systematically backgrounded. The story authors the constraint
 *   as a mountain claim because that is the reading's own structural
 *   assertion, while independently authoring metrics that reflect the
 *   contested, partially extractive operation of this historiographical
 *   frame. Beneficiaries are declared to trigger false-summit detection:
 *   identifiable scholarly and institutional actors gain authority from
 *   treating a contested historical thesis as natural law.
 *
 * KEY AGENTS:
 *   - technological_determinist_school: Agenda-setter (institutional/analytical) â administers the determinist paradigm through canon and curriculum
 *   - early_modern_printers: Primary target (moderate/trapped) â historical agency erased by the determinist framing
 *   - reform_movement_leaders: Primary target (powerful/trapped) â theological and political labor subordinated to infrastructure
 *   - revisionist_historians: Excluded voice (organized/constrained) â structurally marginalized by the determinist canon
 *   - contemporary_tech_narrative_institutions: Beneficiary (organized/mobile) â leverages determinist history for present-day tech ideology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.5).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.45).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.5).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Technological Determinism Reading of Press-Reformation Causality").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "history of technology / religious history / media studies").

domain_priors:emerges_naturally(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, '69cdc808-8a05-4b6f-9fd6-a26d0aea1b76').
narrative_ontology:cs_kernel_codification('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', formalized).
narrative_ontology:cs_authority_grounding('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', lineage).
narrative_ontology:cs_interpretation_layer_present('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76').
narrative_ontology:cs_reading_relation('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', press_reformation_causality__co_constitution, coexists_with).
narrative_ontology:cs_axiom('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', foundational, technology_autonomous_historical_agent).
narrative_ontology:cs_axiom_status(technology_autonomous_historical_agent, holdable).
narrative_ontology:cs_axiom_grounding('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', technology_autonomous_historical_agent, empirically_contingent).
narrative_ontology:cs_axiom('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', foundational, social_change_as_technologically_determined).
narrative_ontology:cs_axiom_status(social_change_as_technologically_determined, holdable).
narrative_ontology:cs_axiom_grounding('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', social_change_as_technologically_determined, empirically_contingent).
narrative_ontology:cs_reference_frame('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', autonomous_technological_causation).
narrative_ontology:cs_drift_state('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', post_revisionist_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('69cdc808-8a05-4b6f-9fd6-a26d0aea1b76', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, technological_determinist_school).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, contemporary_tech_narrative_institutions).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, early_modern_printers).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, reform_movement_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Frames the printing press as an autonomous historical agent and enforces this framing through canonical texts, curricula, and peer-review gatekeeping. Benefits from paradigm authority and citation dominance.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, technological_determinist_school, agenda_setter,
    institutional, generational, analytical, global).

% Exercised strategic choice in what to print, where to distribute, and how to evade censorship. Under the determinist reading their agency is dissolved into the autonomous logic of the press; they appear as conduits rather than actors.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, early_modern_printers, payer,
    moderate, biographical, trapped, continental).

% Deployed theological argument and political alliance to build reform movements. The determinist reading subordinates their doctrinal and organizational labor to the technological infrastructure, treating success as mechanically inevitable.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, reform_movement_leaders, payer,
    powerful, generational, trapped, continental).

% Produce evidence of local variation, censorship effectiveness, and reader resistance that undermines inevitability. They are structurally excluded from the determinist canon and must build parallel journals and citation networks.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, revisionist_historians, excluded,
    organized, generational, constrained, global).

% Technology firms, think tanks, and popular history outlets that benefit from framing technological change as autonomous and inevitable, using the Reformation case as precedent.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, contemporary_tech_narrative_institutions, beneficiary,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves historiographical debate by providing a single, unified causal mechanism to explain the rapid geographic spread of Reformation ideas across early modern Europe.
% TRANSFER_FUNCTION: Moves explanatory authority from human actorsâprinters, reformers, and readersâto the technological infrastructure itself, transferring causal credit and narrative focus from early modern agents to the press.
% ABSENT_VOICES: Revisionist historians emphasizing local contingency, Catholic counter-reformation strategists whose successful censorship complicates inevitability, and early modern readers whose selective interpretation shaped textual reception.
% DISAPPEARANCE_RATIONALE: If the technological determinism reading vanished, academic curricula would shift toward agency-based and co-constitutive models, museum exhibits would emphasize reformer strategy over press mechanics, and the citation economy and job market organized around media ecology would reorganize.
% FOUNDING_PROBLEM: The problem of explaining the rapid, geographically dispersed success of Protestant reform movements across early modern Europe despite entrenched Catholic institutional power.
% FOUNDING_PROBLEM_CORROBORATION: Technological determinist scholars attest the problem is still live. Revisionist historians and Catholic historiographers attest the success was fragmented, slow, and heavily dependent on political contingency; they corroborate from outside the beneficiary set that the founding problem as framed is overstated.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.5, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causality__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causality__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.50) reflects the substantial epistemic extraction performed by dissolving human agency into technological inevitability. Suppression (0.45) captures the gatekeeping function of canonical citation networks that marginalize contingency-based scholarship. Theater ratio (0.60) is elevated because much contemporary invocation of 'the printing press caused the Reformation' has become ritual citation divorced from active historiographical debate. Accessibility collapse (0.70) is high: once the determinist frame is adopted, alternatives appear implausible, though not fully collapsed because revisionist evidence is accessible. Resistance (0.55) is moderate-to-high, consistent with a contested mountain claim that meets sustained historiographical pushback. Measurements track the rise and partial decline of determinist extraction across the twentieth-century scholarly interval.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (determinist school) experiences the constraint as a genuine discovery of historical mechanicsâa fixed feature of media history. The payer seats (early modern actors) experience it as an erasure of their agency. The excluded seat (revisionist historians) experiences it as a barrier to publication and citation. These divergences are structurally derived from the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The technological determinist school sits near the beneficiary end (d â 0.1): the constraint subsidizes their paradigm authority and career viability. Contemporary tech narrative institutions sit slightly further out (d â 0.2) because they benefit indirectly through ideological legitimation. Early modern printers and reform movement leaders are full targets (d â 0.9): the constraint extracts their historical agency and transfers causal credit to infrastructure. Revisionist historians are nearer the target end (d â 0.75) because the constraint suppresses their accounts, though they retain some analytical exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by declaring its mandate explicitly in the founding problem: explaining Reformation success. The R5 interview records that this problem is contested and that corroboration comes partly from outside the beneficiary set. If the problem were dead but the constraint persisted, piton or snare classification would be indicated. Here the founding problem is contested rather than dead, and the constraint's active suppression of alternatives keeps it in the mountain-claimed/tangled-rope-suspect zone rather than a degraded piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_frame,
    'Is the printing press''s causal role in the Reformation a structural feature of media history, or a constructed historiographical frame that benefits specific scholarly and institutional actors?',
    'Comparative historiographical analysis controlling for beneficiary incentives: examine whether the determinist thesis survives scrutiny when funding streams, citation networks, and professional authority are transparently modeled.',
    'If constructed, the constraint reclassifies from mountain to tangled_rope or snare; if structural, it retains mountain certification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_frame, conceptual, 'Natural-law versus constructed ambiguity for the determinist thesis').

omega_variable(
    suppressed_agency_ambiguity,
    'Does the technological determinism reading merely background human agency as a methodological convenience, or does it actively extract explanatory credit from historical actors?',
    'Quantitative citation analysis measuring how often early modern printers and reformers are treated as autonomous causal agents versus conduits of technological force in determinist canonical texts.',
    'If mere backgrounding, extraction is lower and the constraint edges toward rope-like coordination; if active extraction of credit, the effective extractiveness is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppressed_agency_ambiguity, empirical, 'Degree to which agency erasure constitutes extraction').

omega_variable(
    kernel_reading_structural_delta,
    'How would classification change if the kernel were read through strategic deployment rather than technological determinism?',
    'Generate the sibling constraint story for strategic_deployment and compare epsilon values and stakeholder surfaces.',
    'Technological determinism reads as mountain; strategic deployment would likely read as tangled_rope with active human beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Sibling reading comparison for this kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 0, 124).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causality__technological_determinism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causality__technological_determinism, theater_ratio, 20, 0.15).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causality__technological_determinism, theater_ratio, 40, 0.2).
narrative_ontology:measurement(pres_tr_t60, press_reformation_causality__technological_determinism, theater_ratio, 60, 0.35).
narrative_ontology:measurement(pres_tr_t80, press_reformation_causality__technological_determinism, theater_ratio, 80, 0.45).
narrative_ontology:measurement(pres_tr_t100, press_reformation_causality__technological_determinism, theater_ratio, 100, 0.55).
narrative_ontology:measurement(pres_tr_t124, press_reformation_causality__technological_determinism, theater_ratio, 124, 0.6).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causality__technological_determinism, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(pres_be_t20, press_reformation_causality__technological_determinism, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(pres_be_t40, press_reformation_causality__technological_determinism, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(pres_be_t60, press_reformation_causality__technological_determinism, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(pres_be_t80, press_reformation_causality__technological_determinism, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(pres_be_t100, press_reformation_causality__technological_determinism, base_extractiveness, 100, 0.6).
narrative_ontology:measurement(pres_be_t124, press_reformation_causality__technological_determinism, base_extractiveness, 124, 0.5).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causality__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the press_reformation_causality kernel, instantiating technological determinism. Sibling readings (strategic_deployment, co_constitution) decompose the same historical domain into structurally distinct constraints with different epsilon values and stakeholder surfaces. Decomposition follows the epsilon-invariance principle: the label 'printing press caused Reformation' conflates autonomous technology (mountain-claimed), strategic weaponization (tangled_rope), and co-constitutive feedback (rope/scaffold).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
