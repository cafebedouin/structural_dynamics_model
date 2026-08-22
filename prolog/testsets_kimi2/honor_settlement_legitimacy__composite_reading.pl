% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__composite_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: honor_settlement_legitimacy__composite_reading
 *   human_readable: Dueling-Based Honor Settlement Legitimacy (Composite Decline Reading)
 *   domain: historical/sociological/legal
 *
 * SUMMARY:
 *   This constraint story captures the European dueling code as a mechanism
 *   of honor settlement legitimacy from the mid-eighteenth to early twentieth
 *   century. Under the composite reading of the honor_settlement_legitimacy
 *   kernel, dueling's decline is understood as overdetermined by multiple
 *   reinforcing causal pathways: the contraction of honor violence into
 *   cultural unthinkability dominates, but is reinforced by legal
 *   criminalization, military professionalization, the rise of bourgeois
 *   commercial norms, and the state's asserted monopoly on legitimate
 *   violence. The constraint was historically a tangled rope â it
 *   coordinated elite masculine society by standardizing conflict, while
 *   asymmetrically extracting death, injury, and compelled compliance from
 *   individual gentlemen and excluding the populace at large. The metric
 *   trajectory tracks its dissolution: extraction and suppression decline
 *   monotonically toward interval end, while theater ratio rises as the
 *   practice becomes performative residue maintained by nostalgic inertia
 *   rather than functional honor settlement.
 *
 * KEY AGENTS:
 *   - gentleman_elite (agenda_setter/beneficiary, powerful/constrained): Aristocratic and upper-class males who authored, administered, and enforced the code of honor, collecting status rents and class boundary maintenance.
 *   - compelled_duelists (payer, moderate/constrained): Individual gentlemen compelled by the code to bear the direct risks of dueling upon affront, with refusal costing social destruction.
 *   - military_officer_corps (agenda_setter, organized/constrained): Preserved dueling culture within the military through courts of honor and regimental solidarity, resisting civilian legal norms longest.
 *   - excluded_populace (excluded, powerless/trapped): Women, commoners, and non-elites excluded from the honor community and its protections, bearing externalized costs without recourse.
 *   - state_legal_apparatus (observer, institutional/analytical): Criminalized dueling and promoted state courts and libel law as alternative dispute resolution, operating outside the honor community.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__composite_reading, 0.12).
domain_priors:suppression_score(honor_settlement_legitimacy__composite_reading, 0.15).
domain_priors:theater_ratio(honor_settlement_legitimacy__composite_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__composite_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_settlement_legitimacy__composite_reading, "Dueling-Based Honor Settlement Legitimacy (Composite Decline Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__composite_reading, "historical/sociological/legal").

domain_priors:requires_active_enforcement(honor_settlement_legitimacy__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__composite_reading, '44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd').
narrative_ontology:cs_kernel_codification('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', fixed_text).
narrative_ontology:cs_authority_grounding('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', practice).
narrative_ontology:cs_interpretation_layer_present('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd').
narrative_ontology:cs_reading_relation('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', honor_settlement_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', honor_settlement_legitimacy__drop_reading, influences).
narrative_ontology:cs_axiom('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', foundational, multi_causal_decline_overdetermination).
narrative_ontology:cs_axiom_status(multi_causal_decline_overdetermination, holdable).
narrative_ontology:cs_axiom_grounding('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', multi_causal_decline_overdetermination, empirically_contingent).
narrative_ontology:cs_axiom('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', foundational, contraction_as_causal_edge).
narrative_ontology:cs_axiom_status(contraction_as_causal_edge, holdable).
narrative_ontology:cs_axiom_grounding('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', contraction_as_causal_edge, empirically_contingent).
narrative_ontology:cs_reference_frame('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', aristocratic_honor_autonomy).
narrative_ontology:cs_drift_state('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', modern_state_formation, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('44ea6f22-c79a-4fcd-a7d3-eb96d98a49dd', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__composite_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__composite_reading, gentleman_elite).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__composite_reading, compelled_duelists).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, aristocratic_autonomy_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__composite_reading, honor_as_social_bond).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored, administered, and enforced the code of honor and dueling etiquette, adjudicating whether affronts demanded satisfaction. As a class, collected status rents, masculine identity validation, and class boundary maintenance; individually, many were compelled to fight or face social ruin.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, gentleman_elite, agenda_setter,
    powerful, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__composite_reading, gentleman_elite, beneficiary).

% Individual gentlemen, often younger, poorer, or provincial, who were compelled by the code to seek or offer satisfaction upon affront. Bore the direct risks of death, injury, legal prosecution, and ruinous expense for seconds, physicians, and travel. Refusal meant ostracism and loss of caste.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, compelled_duelists, payer,
    moderate, biographical, constrained, continental).

% Preserved dueling culture within the military through internal courts of honor and regimental solidarity. Administered the officer corps' honor code, treating dueling as essential to martial character and command authority. Resisted civilian legal suppression longer than other sectors.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, military_officer_corps, agenda_setter,
    organized, generational, constrained, national).

% Women, commoners, bourgeois without sufficient standing, and colonial subjects excluded from the honor community and its protections. Subject to aristocratic violence without recourse to the legitimating framework that structured elite conflict, and unable to demand satisfaction for affronts.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, excluded_populace, excluded,
    powerless, biographical, trapped, continental).

% Criminalized dueling over the long nineteenth century, prosecuted participants, and promoted state courts and libel law as alternative honor-restoration mechanisms. Stood outside the honor community but increasingly penetrated it with legal sanctions and alternative masculinity norms.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__composite_reading, state_legal_apparatus, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a standardized, ritualized, and bounded mechanism for resolving interpersonal affronts among armed elite males, converting potentially open-ended feuds and vendettas into contained encounters with recognized procedures for restoring honor and social standing.
% TRANSFER_FUNCTION: Moved the physical risks of death and injury from the collective gentleman class onto individual compelled duelists; transferred status legitimacy, class boundary maintenance, and masculine identity validation to the gentleman elite and military officer corps; moved jurisdiction over private violence from kin-based retribution to a ceremonial peer-regulated structure.
% ABSENT_VOICES: Women who bore the widowhood and family costs, commoners and bourgeois excluded from honorable status, religious authorities condemning the sinfulness of bloodshed, and the families of killed or maimed duelists â all structurally excluded from the honor community's adjudication of whether an affront warranted mortal risk.
% DISAPPEARANCE_RATIONALE: If the dueling legitimacy structure vanished overnight in, say, 1760, elite masculine socialization, military officer culture, and aristocratic status competition would have reorganized immediately around alternative mechanisms â state courts, libel law, social ostracism, or open feuding. The entire field of honor settlement would have shifted institutional form; the constraint was load-bearing for a specific social configuration.
% FOUNDING_PROBLEM: In a stratified society with weak state monopoly on violence and a culture of arms-bearing among elite males, how to contain retaliatory violence over interpersonal affronts without either degenerating into chaotic blood feud or surrendering elite autonomy to external state jurisdiction.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians documenting the rise of state courts and libel law, historical sociologists analyzing state formation and bureaucratization, and military historians studying officer professionalization attest from outside the benefiting honor community that the problem of elite private violence was solved by alternative institutions. The gentleman elite themselves largely denied the problem was solved, asserting continuing necessity until cultural unthinkability rendered the denial moot.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__composite_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__composite_reading_tests).
:- end_tests(honor_settlement_legitimacy__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness at interval end (0.12) reflects the near-total collapse of dueling as an operative honor mechanism by c.1910; the measurement series declines from 0.72 at T=0 as legal, military, and cultural mechanisms converged to suppress the practice. Suppression declines from 0.85 to 0.15 because the active social enforcement that compelled gentlemen to fight â ostracism, status revocation â eroded as alternative masculinities and legal sanctions replaced them. Theater ratio inverts: rising from 0.15 to 0.85 as the constraint's functional operation atrophied and what remained was largely performative nostalgia and residual military ritual. Accessibility collapse drops from high to 0.25 because alternatives (courts, press, social cut) became abundant; resistance rises to 0.80 because the modernizing state, church, and bourgeois culture actively opposed the residual practice. The claimed type is tangled_rope throughout â the structural nature of the constraint was coordination-plus-extraction â while the metrics honestly describe its terminal dissolution; any engine divergence between claim and computed end-state type is the intended signal of lifecycle drift.
 *
 * PERSPECTIVAL GAP:
 *   The gentleman elite experienced the constraint as identity-constitutive coordination â it structured their community, contained violence, and marked caste boundaries â while compelled duelists experienced it as coerced risk-bearing with asymmetric costs. The state legal apparatus experienced it as a jurisdictional competitor to be suppressed; the excluded populace experienced it as an external violence they could neither access nor escape. These divergent directionalities produce different computed seat classifications: beneficiaries see coordination, payers see extraction, excluded see arbitrary violence.
 *
 * DIRECTIONALITY LOGIC:
 *   The gentleman elite are declared beneficiaries because the code transferred status legitimacy and class boundary maintenance to them; their structural directionality is toward the beneficiary pole despite constrained individual exit, because the constraint subsidized their social standing. Compelled duelists are declared victims because they bore the concentrated risks of death, injury, and financial ruin; their constrained exit and victim status place them near the full-target pole. The military officer corps sits closer to the beneficiary/agenda-setter pole for the military sub-constraint but shares the compelled duelist's vulnerability when individual officers were challenged. State legal authorities have analytical exit and no beneficiary stake, placing them outside the extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â containing retaliatory violence among armed elite males in a weak-state environment â was solved by the development of state courts, libel law, and professional military discipline. The composite reading prevents mislabeling the constraint's persistence as either pure cultural inertia (piton) or pure institutional dismantling (snare collapse). By showing that cultural contraction, legal criminalization, military bureaucratization, and bourgeois normative hegemony were mutually reinforcing, the reading establishes that the mandate outlived its function by multiple independent measures â the definition of overdetermined mandatrophy. The high terminal theater ratio supports the piton-like terminal state, but the structural origin as coordination-plus-extraction keeps the classification from collapsing into pure extraction or pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    material_cultural_mechanism_balance,
    'Does the observed decline of dueling reflect primarily independent material and institutional changes (legal criminalization, military professionalization), or primarily cultural contraction into unthinkability with material changes serving as secondary reinforcement?',
    'Comparative historical analysis across jurisdictions where material and cultural changes desynchronized: e.g., military dueling persisting after civilian criminalization tests material dominance; civilian dueling persisting in regions without strong bourgeois cultural hegemony tests cultural dominance.',
    'If material changes alone sufficed, the constraint''s classification tilts toward snare-dismantled-by-state; if cultural contraction dominated, it tilts toward rope-that-lost-coordination-value. The composite reading sits between these poles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(material_cultural_mechanism_balance, conceptual, 'Balance of material vs cultural causal mechanisms in dueling''s decline').

omega_variable(
    elite_net_position_ambiguity,
    'Was the gentleman elite as a class net beneficiary of the dueling code, or did the code extract net life and wealth from them while providing only diffuse status coordination?',
    'Prosopographic analysis of dueling mortality and cost distribution among elite sub-groups: if younger, poorer, or provincial gentlemen bore disproportionate costs while wealthier cosmopolitans avoided duels, the elite was internally stratified and the class as a whole may have been net victim.',
    'If the elite were net victims, the constraint reclassifies toward snare; if net beneficiaries with internal cost-shifting to compelled subordinates, tangled rope is preserved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_net_position_ambiguity, conceptual, 'Whether the gentleman elite were net beneficiaries or victims').

omega_variable(
    fringe_persistence_status,
    'Do post-1900 residual dueling practices constitute persistence of the honor_settlement_legitimacy constraint, or a functionally distinct behavioral residue without the original legitimating structure?',
    'Ethnographic and historical analysis of late dueling incidents: whether they invoke the full code of honor, seconds, and social restoration, or represent anomic violence without the institutional framework.',
    'If the full structure persists, the drop_reading is supported and the constraint has a longer tail than the composite reading admits; if only behavioral residue, the composite reading''s overdetermination thesis is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fringe_persistence_status, empirical, 'Whether residual dueling is constraint persistence or behavioral residue').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__composite_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t0, honor_settlement_legitimacy__composite_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hono_tr_t30, honor_settlement_legitimacy__composite_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(hono_tr_t60, honor_settlement_legitimacy__composite_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(hono_tr_t90, honor_settlement_legitimacy__composite_reading, theater_ratio, 90, 0.55).
narrative_ontology:measurement(hono_tr_t120, honor_settlement_legitimacy__composite_reading, theater_ratio, 120, 0.72).
narrative_ontology:measurement(hono_tr_t150, honor_settlement_legitimacy__composite_reading, theater_ratio, 150, 0.85).

% Extraction over time
narrative_ontology:measurement(hono_be_t0, honor_settlement_legitimacy__composite_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(hono_be_t30, honor_settlement_legitimacy__composite_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(hono_be_t60, honor_settlement_legitimacy__composite_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(hono_be_t90, honor_settlement_legitimacy__composite_reading, base_extractiveness, 90, 0.38).
narrative_ontology:measurement(hono_be_t120, honor_settlement_legitimacy__composite_reading, base_extractiveness, 120, 0.22).
narrative_ontology:measurement(hono_be_t150, honor_settlement_legitimacy__composite_reading, base_extractiveness, 150, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t0, honor_settlement_legitimacy__composite_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(hono_su_t30, honor_settlement_legitimacy__composite_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(hono_su_t60, honor_settlement_legitimacy__composite_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(hono_su_t90, honor_settlement_legitimacy__composite_reading, suppression_requirement, 90, 0.45).
narrative_ontology:measurement(hono_su_t120, honor_settlement_legitimacy__composite_reading, suppression_requirement, 120, 0.28).
narrative_ontology:measurement(hono_su_t150, honor_settlement_legitimacy__composite_reading, suppression_requirement, 150, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__composite_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
