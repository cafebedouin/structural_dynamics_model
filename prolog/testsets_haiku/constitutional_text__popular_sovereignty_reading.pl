% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Authority via Popular Sovereignty
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This constraint is ONE READING of the contested kernel
 *   'constitutional_text'. The popular sovereignty reading asserts that
 *   constitutional authority derives from constituent power — the demos
 *   retains ultimate interpretive authority over constitutional meaning
 *   through amendment, convention, or revolutionary refounding. Neither
 *   courts nor legislatures are supreme in this reading; both are subordinate
 *   to extra-institutional democratic expression. This reading enters direct
 *   structural conflict with judicial_supremacy_reading (courts claim final
 *   authority) and legislative_sovereignty_reading (legislatures claim final
 *   authority). The sibling readings are not alternative measurements of one
 *   constraint; they are genuinely different constraints with different ε
 *   values, different beneficiary/victim structures, and different
 *   institutional winners and losers. Each should be authored as a separate
 *   constraint story and linked via network.affects_constraints. This story
 *   authors the popular sovereignty reading alone.
 *
 * KEY AGENTS:
 *   - Popular movements: Constitutional coalitions, amendment drives, revolutionary assemblies that mobilize constituent power outside formal institutions.
 *   - Constitutional courts: Institutional interpreters subordinated in this reading; they benefit from rule-of-law stability but pay the cost of interpretive constraint.
 *   - Legislative bodies: Institutional policymakers also subordinated to constituent power; they benefit from constitutional legitimacy but lose supremacy.
 *   - Amendment coalitions: Supermajority movements that successfully invoke formal or revolutionary amendment authority; primary beneficiaries of this reading.
 *   - Institutional experts: Judges, scholars, career legislators whose authority rests on expertise; they bear the cost of periodic popular override.
 *   - Excluded minorities: Groups too small to command popular support; they face marginaliz­ation risk under majoritarian popular sovereignty despite rhetorical inclusivity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.31).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.18).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Authority via Popular Sovereignty").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional/political").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, 'd296e083-6284-490e-b5c4-6b61b4006afa').
narrative_ontology:cs_kernel_codification('d296e083-6284-490e-b5c4-6b61b4006afa', fixed_text).
narrative_ontology:cs_authority_grounding('d296e083-6284-490e-b5c4-6b61b4006afa', lineage).
narrative_ontology:cs_interpretation_layer_present('d296e083-6284-490e-b5c4-6b61b4006afa').
narrative_ontology:cs_reading_relation('d296e083-6284-490e-b5c4-6b61b4006afa', constitutional_text__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('d296e083-6284-490e-b5c4-6b61b4006afa', constitutional_text__legislative_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('d296e083-6284-490e-b5c4-6b61b4006afa', foundational, constituent_power_supreme_over_institutions).
narrative_ontology:cs_axiom_status(constituent_power_supreme_over_institutions, holdable).
narrative_ontology:cs_axiom_grounding('d296e083-6284-490e-b5c4-6b61b4006afa', constituent_power_supreme_over_institutions, deontological).
narrative_ontology:cs_axiom('d296e083-6284-490e-b5c4-6b61b4006afa', secondary, institutional_check_on_institutional_power).
narrative_ontology:cs_axiom_status(institutional_check_on_institutional_power, holdable).
narrative_ontology:cs_axiom_grounding('d296e083-6284-490e-b5c4-6b61b4006afa', institutional_check_on_institutional_power, instrumental).
narrative_ontology:cs_reference_frame('d296e083-6284-490e-b5c4-6b61b4006afa', constituent_power_foundational_authority).
narrative_ontology:cs_drift_state('d296e083-6284-490e-b5c4-6b61b4006afa', contemporary_institutional_capture_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d296e083-6284-490e-b5c4-6b61b4006afa', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_movements).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, amendment_coalition).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, popular_mobilization).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, expert_judiciaries).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_continuity).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).
:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.31) because this reading does not describe concentrated benefit extraction in the technical sense — no single actor systematically siphons value from the arrangement. Instead, extractiveness measures the asymmetry between those who benefit from popular sovereignty (democratic movements, amendment coalitions) and those who bear the cost of institutional subordination and periodic override (courts, legislatures, experts). The asymmetry is real but distributed: institutional stability and expertise are diffuse costs, not concentrated losses. Suppression is low (0.18) because popular sovereignty reading does not require coercive suppression of exit — institutional actors can and do resist (courts reassert jurisdiction, legislatures act) and this resistance is visible and substantial. Theater is high-moderate (0.52) because a significant share of public constitutional discourse performs popular sovereignty while actual amendment and institutional override happen rarely. The measurements show extraction and theater rising early (t0–t15) as the reading gains academic and public currency, then plateauing (t15–t40) as it stabilizes as a live alternative to institutional supremacy readings. This is the pattern of a genuinely contested claim finding stable voice in the discourse without yet producing institutional displacement.
 *
 * PERSPECTIVAL GAP:
 *   The popular movement seat and the institutional seats should compute very differently. From the movement's position, this reading is genuine coordinate sovereignty with extra-institutional leverage; they experience low d (beneficiary of the reading's authority claim). From the court or legislature seat, the same reading is subordination and periodic override; they experience high d (targets of constituent power mobilization). The engine computes these divergences from power, exit_options, and beneficiary/victim declarations. The authored claim (tangled_rope) signals that both genuine coordination (constituent power solves the arbitration problem) and asymmetric extraction (institutions lose final authority) are structural features. Courts and legislatures gain the certainty that constitutional text is supreme (coordination benefit) but lose the authority to determine supremacy (extraction cost). This is exactly the tangled-rope structure: shared framework with asymmetric positioning.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are democratic movements, amendment coalitions, and the abstract category 'popular mobilization' — entities that gain authority and leverage from this reading's institutional framework. Victims are institutional stability (the cost of uncertainty and periodic override), expert judiciaries (lose deference and final-say authority), and legislative continuity (lose supremacy claims). Courts and legislatures are payers because they bear the reputational and functional costs of constraint; they are simultaneously beneficiaries because constitutional text's supremacy (which this reading vindicates) provides their legitimacy. The dual roles reflect the tangled-rope structure: genuine coordination benefit (constitutional constraint) coupled with extraction cost (institutional subordination). Institutional experts' exit is constrained because their professional authority depends on institutional settings; they cannot opt out of the constraint without losing their social position. Amendment coalitions have arbitrage-grade exit because they can invoke either formal amendment procedures or revolutionary claim if they command sufficient mobilization.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading has NOT reached mandatrophy. The founding problem (institutional monopolization of constitutional meaning) is live; the reading still performs its founding function by providing an extra-institutional check on institutional self-dealing. Courts and legislatures actively resist this reading (they reassert jurisdiction and invoke institutional supremacy), which means the constraint has not atrophied into purely performative status. Resistance is high (0.71 by measurement), confirming that institutional actors still defend their supremacy claims against the popular sovereignty reading. Theater is elevated (0.52) because public constitutional discourse invokes popular sovereignty rhetorically more often than it mobilizes constituent power institutionally, but the reading has not yet flipped to pure performance — amendment movements do succeed, constitutional conventions do happen, and revolutionary constitutional moments do reshape governance (historical examples: 1919 Weimar, 1947 India, 1989 Eastern Europe). The reading remains contestable and contested, not decayed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constituent_power_empirical_vs_normative,
    'Is constituent power (the demos'' ultimate authority to reshape the constitution) an empirical fact about how constitutions actually change, or a normative claim about how they should change?',
    'Historical analysis: do constitutional amendments and revolutionary moments demonstrate that popular mobilization does override institutional determinations, or do they show that institutions retain ultimate control by setting the rules for amendment/revolution? Comparative constitutional evidence from countries with different readings could clarify whether popular sovereignty readings predict differently on constitutional change.',
    'If constituent power is an empirical fact demonstrated by history, then ε remains ~0.31 and the reading has genuine structural force. If it is primarily normative (aspirational), then the reading might reclassify as aspirational piton — performance of popular authority that institutions maintain theologically but control procedurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituent_power_empirical_vs_normative, empirical, 'Whether constituent power is an observed institutional reality or a normative ideal.').

omega_variable(
    majoritarianism_and_minority_protection,
    'Can a popular sovereignty reading protect minority constitutional rights, or does majoritarianism inherent in democratic mobilization necessarily subordinate minority interests to majority will?',
    'Examine historical constitutional amendments and popular mobilizations that either protected minority rights (e.g., abolition, suffrage, civil rights amendments) or threatened them (e.g., majoritarian constitutional reversals in democracies with weak minority protections). Test whether supermajority amendment requirements provide sufficient protection or whether minorities require institutional (judicial) vetoes on popular will.',
    'If popular sovereignty can reliably protect minorities through institutional design (supermajority thresholds, human rights entrenchment), then the reading sustains as tangled_rope with minority cost being manageable. If majoritarian override systematically harms minorities, the reading becomes more extractive (reclassifies toward snare for minority seats) and the victim category expands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(majoritarianism_and_minority_protection, empirical, 'Whether popular sovereignty is compatible with stable minority protection.').

omega_variable(
    reading_foreclosure_empirical,
    'Does this reading (popular sovereignty) logically foreclose the judicial_supremacy_reading or the legislative_sovereignty_reading within a single constitutional framework, or can all three coexist as live options for different actors?',
    'Examine whether a constitutional system could simultaneously recognize courts as supreme interpreters AND legislatures as supreme AND the demos as supreme, or whether these claims are genuinely logically inconsistent. Test via comparative law: do any constitutional systems claim all three at once, or do they always resolve to one dominant reading?',
    'If the readings genuinely foreclose each other (cannot coexist in one framework), then the relationship is ''forecloses'' and this reading claims ultimate authority that cancels the others. If they coexist across different parties and jurisdictional domains (courts supreme in their sphere, legislatures in theirs, demos in ultimate constitutional revision), then the relationship is ''coexists_with'' and the constraint remains genuinely contestable without resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_empirical, conceptual, 'Whether readings of constitutional authority are mutually exclusive or can coexist.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'When institutions suppress or resist popular sovereignty claims (reassert jurisdiction, deny revolutionary legitimacy, set high amendment thresholds), is this suppression structural (external barriers the movement must overcome) or internalized (the movement itself accepts institutional rules and works within them, internalizing the subordination)?',
    'Observe post-mobilization trajectories: if movements that fail to mobilize constituent power then accept institutional determinations without further challenge, suppression has become internalized. If movements repeatedly mobilize against institutional barriers despite institutional resistance, suppression remains structural. Document the frequency and intensity of extra-institutional pressure versus the frequency of accommodation to institutional rules.',
    'If suppression is primarily structural, the measured suppression value (0.18) is appropriately low — movements face barriers but are not psychologically subordinated. If suppression is significantly internalized, the effective suppression is higher than measured (the movement internalizes institutional supremacy claims even when challenging them), and the reading''s stability depends on this internalization rather than genuine popular authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether institutional suppression of popular sovereignty is structural or internalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cons_tr_t5, constitutional_text__popular_sovereignty_reading, theater_ratio, 5, 0.41).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__popular_sovereignty_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement(cons_tr_t15, constitutional_text__popular_sovereignty_reading, theater_ratio, 15, 0.47).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__popular_sovereignty_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(cons_tr_t25, constitutional_text__popular_sovereignty_reading, theater_ratio, 25, 0.51).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__popular_sovereignty_reading, theater_ratio, 30, 0.52).
narrative_ontology:measurement(cons_tr_t35, constitutional_text__popular_sovereignty_reading, theater_ratio, 35, 0.52).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__popular_sovereignty_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cons_be_t5, constitutional_text__popular_sovereignty_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(cons_be_t10, constitutional_text__popular_sovereignty_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(cons_be_t15, constitutional_text__popular_sovereignty_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement(cons_be_t20, constitutional_text__popular_sovereignty_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement(cons_be_t25, constitutional_text__popular_sovereignty_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(cons_be_t30, constitutional_text__popular_sovereignty_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement(cons_be_t35, constitutional_text__popular_sovereignty_reading, base_extractiveness, 35, 0.31).
narrative_ontology:measurement(cons_be_t40, constitutional_text__popular_sovereignty_reading, base_extractiveness, 40, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cons_su_t5, constitutional_text__popular_sovereignty_reading, suppression_requirement, 5, 0.12).
narrative_ontology:measurement(cons_su_t10, constitutional_text__popular_sovereignty_reading, suppression_requirement, 10, 0.14).
narrative_ontology:measurement(cons_su_t15, constitutional_text__popular_sovereignty_reading, suppression_requirement, 15, 0.16).
narrative_ontology:measurement(cons_su_t20, constitutional_text__popular_sovereignty_reading, suppression_requirement, 20, 0.17).
narrative_ontology:measurement(cons_su_t25, constitutional_text__popular_sovereignty_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(cons_su_t30, constitutional_text__popular_sovereignty_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(cons_su_t35, constitutional_text__popular_sovereignty_reading, suppression_requirement, 35, 0.18).
narrative_ontology:measurement(cons_su_t40, constitutional_text__popular_sovereignty_reading, suppression_requirement, 40, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(constitutional_text__popular_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The 'constitutional_text' kernel admits three distinct constraint readings, each with different ε values and different beneficiary/victim structures. The popular_sovereignty_reading is one instantiation of the kernel; the sibling readings (judicial_supremacy_reading, legislative_sovereignty_reading) are separate constraints. All three stories must be authored to represent the kernel contest accurately. The readings influence each other: popular sovereignty readings create pressure on institutional supremacy claims; institutional supremacy readings suppress popular sovereignty mobilization. These are structural influences, not measurements of a single constraint from different angles. Each reading should be generated as a self-contained constraint story with independent metrics, and the network links should record the competitive/influence relationships between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text__popular_sovereignty_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
