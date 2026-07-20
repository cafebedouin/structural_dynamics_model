% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: P5 Veto Power â Oligopoly Reading
 *   domain: international_relations/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the oligopoly reading of the UN
 *   Charter Article 27 veto power. It treats the P5 veto not as a
 *   coordination device for collective security but as a structurally
 *   entrenched snare: five states extract ongoing authority rents from a
 *   frozen 1945 power distribution, using Charter immutability (Article 108
 *   amendment rules requiring P5 consent) to suppress institutional evolution
 *   that would redistribute voting weight and permanent membership to
 *   contemporary powers. The kernel is the veto power itself; this reading
 *   isolates the extractive dimension from the coordination and sovereignty
 *   sibling readings.
 *
 * KEY AGENTS:
 *   - p5_states: Primary beneficiary and agenda-setter (institutional/arbitrage) â capture authority rents and block reform
 *   - non_p5_majority: Primary target (organized/trapped) â bear subordination costs with no structural exit
 *   - aspiring_power_states: Secondary target (powerful/constrained) â blocked from institutional upgrade despite contemporary weight
 *   - un_reform_advocates: Excluded voice (moderate/constrained) â procedurally irrelevant to veto decisions
 *   - international_relations_scholars: Analytical observer (analytical/analytical) â map the structural deficit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.82).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.88).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "P5 Veto Power â Oligopoly Reading").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, 'ae1be176-9c81-43d9-83f3-9a50b9dcaa05').
narrative_ontology:cs_kernel_codification('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', formalized).
narrative_ontology:cs_authority_grounding('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', extraction).
narrative_ontology:cs_reading_relation('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', foundational, veto_serves_oligopolic_entrenchment).
narrative_ontology:cs_axiom_status(veto_serves_oligopolic_entrenchment, holdable).
narrative_ontology:cs_axiom_grounding('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', veto_serves_oligopolic_entrenchment, empirically_contingent).
narrative_ontology:cs_axiom('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', secondary, charter_consent_rule_is_extraction_device).
narrative_ontology:cs_axiom_status(charter_consent_rule_is_extraction_device, holdable).
narrative_ontology:cs_axiom_grounding('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', charter_consent_rule_is_extraction_device, conventional).
narrative_ontology:cs_reference_frame('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', p5_oligopolic_order).
narrative_ontology:cs_drift_state('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', contemporary_multipolar_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ae1be176-9c81-43d9-83f3-9a50b9dcaa05', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_majority).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, aspiring_power_states).
narrative_ontology:constraint_vindicates(article_27_veto_power__oligopoly_reading, great_power_primacy_doctrine).
narrative_ontology:constraint_vindicates(article_27_veto_power__oligopoly_reading, charter_immutability_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent seats on the UN Security Council with unilateral veto power over substantive resolutions. Charter amendment requires their consent, creating structural lock-in. Extract authority rents by defining the legitimacy of international enforcement action and preventing any redistribution of Council seats or veto rights.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, p5_states, beneficiary).

% Comprise the vast majority of UN member states bound by a Charter they cannot amend without P5 consent. Pay authority costs by accepting subordinate institutional standing and exclusion from the veto-wielding core despite equal nominal sovereignty.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_majority, payer,
    organized, generational, trapped, global).

% Major regional powers with contemporary economic, demographic, and military weight seeking permanent Security Council membership. Blocked from institutional upgrading by the Charter amendment procedure and P5 refusal to consent to reform.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, aspiring_power_states, payer,
    powerful, generational, constrained, global).

% Coalitions of middle powers and Global South states advocating for Council expansion, veto limitation, or weighted voting. Their proposals are structurally blocked by the amendment rule and their voices are procedurally absent from veto decisions.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_reform_advocates, excluded,
    moderate, generational, constrained, global).

% Analytical observers documenting the democratic deficit and representational skew of the Security Council. They produce structural critiques of veto entrenchment but wield no procedural power to alter the rule.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, international_relations_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__oligopoly_reading, p5_states).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the consolidation and perpetuation of a post-1945 geopolitical hierarchy, ensuring that no redistribution of institutional power can occur without the consent of the incumbent great powers.
% TRANSFER_FUNCTION: Moves authority rentsâagenda control, legitimacy monopoly, and enforcement discretionâfrom the non-P5 UN majority and aspiring powers to the five permanent members, while blocking institutional reform that would dilute P5 privilege.
% ABSENT_VOICES: Non-P5 member states representing the global majority; regional powers with contemporary demographic and economic weight but no procedural pathway to permanent membership; and cosmopolitan legal theorists arguing for sovereign equalityâall structurally excluded because Charter amendment requires P5 consent.
% DISAPPEARANCE_RATIONALE: If the veto and its immutability vanished, Security Council composition and decision rules would be renegotiated, institutional power would redistribute toward contemporary geopolitical weight, and the P5's exclusive enforcement legitimacy monopoly would collapseâthe UN architecture would rearrange around new power equilibria.
% FOUNDING_PROBLEM: The immediate post-World War II need to institutionalize great-power cooperation and prevent unilateral military confrontation among nuclear-armed victors by embedding them in a collective security framework with procedural equality.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from the 1945 San Francisco Conference and the Office of the Historian attest the founding concern was great-power concert. Contemporary corroboration from outside the P5 beneficiary set includes the 2004 High-level Panel on Threats, Challenges and Change and the subsequent Intergovernmental Negotiations on Council reform, both of which acknowledge the founding rationale has been superseded by persistent representational deficits.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the veto extracts institutional authority and agenda control far beyond any proportional contribution to security provision; suppression (0.88) is higher still because reform is structurally impossible without P5 consent, which they withhold. Theater ratio (0.45) reflects that the 'collective security' discourse remains performatively maintained while the actual function is oligopolistic power preservation. Accessibility collapse (0.80) captures that once the Charter amendment rule is understood, exit appears impossible for non-P5 states. Resistance (0.55) reflects persistent but procedurally impotent reform advocacy.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat, the constraint appears as necessary institutional privilege stabilizing international order; from the non-P5 majority seat, it reads as an entrenched extraction mechanism denying sovereign equality. The engine computes this divergence from structural data: P5 states have arbitrage-grade exit (they can act outside the UN when the veto is inconvenient), while non-P5 states are trapped in a body whose rules they cannot change.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states are structural beneficiaries with global scope and arbitrage exit; the engine derives low d (near beneficiary). Non-P5 majority and aspiring powers are victims with trapped or constrained exit and organized-to-powerful standing; the engine derives high d (near target). The effective extraction chi is therefore amplified for the victim seats and damped for the P5 seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing great-power war through institutionalized concertâis dead in the sense that the bipolar and unipolar moments have passed, yet the arrangement persists. The oligopoly reading prevents mislabeling this as a rope or scaffold by showing that the coordination story (preventing war) is cover: the same P5 states routinely bypass the veto framework unilaterally when it suits them, indicating the constraint's primary function is not coordination but entrenchment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the original great-power concert problem definitively dead, or has it transformed into a contemporary multipolar rivalry that the veto still manages?',
    'Comparative historical analysis of militarized interstate disputes involving P5 members before and after 1991, measuring whether veto-constrained institutional channels actually prevent escalation relative to alternative architectures.',
    'If the founding problem is live in transformed shape, the oligopoly reading overstates extraction and the engine may misclassify a tangled_rope as a snare. If dead, the persistence is pure rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the post-1945 great-power concert rationale still operates or is obsolete.').

omega_variable(
    kernel_reading_underdetermination,
    'Does the oligopoly reading foreclose the sovereignty reading within a single legal framework, or can a jurist consistently hold both the extraction critique and the sovereign-consent principle?',
    'Jurisprudential analysis of whether sovereign equality and great-power privilege are logically reconcilable in the same constitutional order.',
    'If foreclosed, the kernel is internally contradictory and the readings are mutually exclusive; if coexistent, the readings are perspectival seats on the same institution and classification must treat them as observer-relative rather than structurally distinct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Logical relationship between oligopoly and sovereignty readings of the veto kernel.').

omega_variable(
    p5_rent_quantification,
    'How much of P5 global authority derives specifically from the veto privilege versus independent military-economic power?',
    'Counterfactual analysis of P5 diplomatic weight in institutional settings where the veto is irrelevant (UNGA, G20) versus Security Council settings.',
    'If P5 authority collapses without the veto, the extraction measure is validated; if it persists, the veto is a weaker snare than the epsilon score suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(p5_rent_quantification, empirical, 'Isolating the veto''s marginal contribution to P5 authority rents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(p5veto_oligopoly_tr_t0, article_27_veto_power__oligopoly_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(p5veto_oligopoly_tr_t10, article_27_veto_power__oligopoly_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(p5veto_oligopoly_tr_t20, article_27_veto_power__oligopoly_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(p5veto_oligopoly_tr_t30, article_27_veto_power__oligopoly_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(p5veto_oligopoly_tr_t40, article_27_veto_power__oligopoly_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(p5veto_oligopoly_tr_t50, article_27_veto_power__oligopoly_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(p5veto_oligopoly_tr_t60, article_27_veto_power__oligopoly_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(p5veto_oligopoly_tr_t70, article_27_veto_power__oligopoly_reading, theater_ratio, 70, 0.44).
narrative_ontology:measurement(p5veto_oligopoly_tr_t80, article_27_veto_power__oligopoly_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(p5veto_oligopoly_be_t0, article_27_veto_power__oligopoly_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(p5veto_oligopoly_be_t10, article_27_veto_power__oligopoly_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(p5veto_oligopoly_be_t20, article_27_veto_power__oligopoly_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(p5veto_oligopoly_be_t30, article_27_veto_power__oligopoly_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(p5veto_oligopoly_be_t40, article_27_veto_power__oligopoly_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(p5veto_oligopoly_be_t50, article_27_veto_power__oligopoly_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement(p5veto_oligopoly_be_t60, article_27_veto_power__oligopoly_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(p5veto_oligopoly_be_t70, article_27_veto_power__oligopoly_reading, base_extractiveness, 70, 0.78).
narrative_ontology:measurement(p5veto_oligopoly_be_t80, article_27_veto_power__oligopoly_reading, base_extractiveness, 80, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(p5veto_oligopoly_su_t0, article_27_veto_power__oligopoly_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(p5veto_oligopoly_su_t10, article_27_veto_power__oligopoly_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(p5veto_oligopoly_su_t20, article_27_veto_power__oligopoly_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(p5veto_oligopoly_su_t30, article_27_veto_power__oligopoly_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(p5veto_oligopoly_su_t40, article_27_veto_power__oligopoly_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(p5veto_oligopoly_su_t50, article_27_veto_power__oligopoly_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(p5veto_oligopoly_su_t60, article_27_veto_power__oligopoly_reading, suppression_requirement, 60, 0.8).
narrative_ontology:measurement(p5veto_oligopoly_su_t70, article_27_veto_power__oligopoly_reading, suppression_requirement, 70, 0.85).
narrative_ontology:measurement(p5veto_oligopoly_su_t80, article_27_veto_power__oligopoly_reading, suppression_requirement, 80, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__coordination_reading).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is the oligopoly reading of the article_27_veto_power kernel. It belongs to a constraint family with coordination_reading and sovereignty_reading. The decomposition follows the epsilon-invariance principle: the natural-language label 'P5 veto' conflates structurally distinct claims with different epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
