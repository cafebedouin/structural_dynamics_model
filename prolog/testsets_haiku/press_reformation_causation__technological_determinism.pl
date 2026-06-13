% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press Technological Determinism (Reformation Causation)
 *   domain: history/technology/religion
 *
 * SUMMARY:
 *   The technological determinism reading treats the printing press as the
 *   primary causal force that made the Reformation inevitable. In this
 *   framing, the press is an exogenous technological innovation (circa 1440)
 *   whose material capability to reproduce texts at scale and low cost
 *   directly undermines the Catholic Church's monopoly on scriptural
 *   interpretation (which depended on Latin manuscripts and clerical
 *   gatekeeping). Vernacular printing becomes structurally unavoidable once
 *   the technology is available; reformers are positioned as beneficiaries of
 *   this technological fact rather than as agents who caused it. The Church's
 *   resistance to printed scripture is treated as futile — resistance to a
 *   material technology that has already proliferated. The constraint is
 *   CLAIMED as a mountain (natural law of technological capability) while the
 *   authored metrics show near-zero extractiveness and suppression: this is
 *   consistent with the mountain claim, as a technological fact does not
 *   extract from participants — it simply alters the possibility space they
 *   inhabit.
 *
 * KEY AGENTS:
 *   - printing_press_technology: The exogenous material capability; treated as non-agent structural fact
 *   - catholic_church_authority: Loses monopoly on scriptural distribution; cannot censor what technology makes inevitable
 *   - reformation_movement: Benefits from technological abundance of distributed texts; agency is secondary to technological enablement
 *   - lay_literacy_expansion: Structural consequence of cost shift toward vernacular texts; no agent drives this
 *   - manuscript_scribes: Economically displaced by the technology; treated as passive victims of technological change
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.12).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.05).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.12).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press Technological Determinism (Reformation Causation)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history/technology/religion").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, '1ba936b5-3a40-4c16-987d-656d86f767b0').
narrative_ontology:cs_kernel_codification('1ba936b5-3a40-4c16-987d-656d86f767b0', distributed).
narrative_ontology:cs_authority_grounding('1ba936b5-3a40-4c16-987d-656d86f767b0', expertise).
narrative_ontology:cs_interpretation_layer_present('1ba936b5-3a40-4c16-987d-656d86f767b0').
narrative_ontology:cs_reading_relation('1ba936b5-3a40-4c16-987d-656d86f767b0', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('1ba936b5-3a40-4c16-987d-656d86f767b0', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('1ba936b5-3a40-4c16-987d-656d86f767b0', foundational, technology_determines_social_possibility_space).
narrative_ontology:cs_axiom_status(technology_determines_social_possibility_space, holdable).
narrative_ontology:cs_axiom_grounding('1ba936b5-3a40-4c16-987d-656d86f767b0', technology_determines_social_possibility_space, empirically_contingent).
narrative_ontology:cs_axiom('1ba936b5-3a40-4c16-987d-656d86f767b0', foundational, human_agency_responsive_to_technological_fact).
narrative_ontology:cs_axiom_status(human_agency_responsive_to_technological_fact, overridden).
narrative_ontology:cs_axiom_grounding('1ba936b5-3a40-4c16-987d-656d86f767b0', human_agency_responsive_to_technological_fact, empirically_contingent).
narrative_ontology:cs_reference_frame('1ba936b5-3a40-4c16-987d-656d86f767b0', technological_determination_of_social_change).
narrative_ontology:cs_drift_state('1ba936b5-3a40-4c16-987d-656d86f767b0', contemporary_historiography, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('1ba936b5-3a40-4c16-987d-656d86f767b0', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, reformation_movement).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_literacy_expansion).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12 at interval end) because a technological capability does not extract from those it affects — it simply changes their constraint set. The technology is morally neutral in the determinism reading; it has no preference for Reformation or Counter-Reformation. Suppression is near-zero (0.05) because resistance to a technology that has already copied itself thousands of times is structurally ineffective — the Church's later efforts to control printing through the Index are reactive damage limitation, not preventive enforcement. Theater is minimal (0.08) because the printing press's function is straightforward material reproduction; there is no performative maintenance cost. Accessibility collapse is very high (0.92) because once printing technology exists and costs drop below manuscript production, alternative modes of textual scarcity become materially impossible — no actor can restore the scarcity without destroying the technology itself. Resistance is near-zero (0.02) because in this reading the press is not a choice anyone made — it is a fact of material history that accumulates inevitably once Gutenberg's innovation exists. The measurement series tracks the gradual proliferation of printed texts (extractiveness/theater rising slowly as printing diffuses) and the Church's ineffectual suppression efforts (suppression requirement staying low because the technology itself cannot be suppressed).
 *
 * PERSPECTIVAL GAP:
 *   From the Church's institutional seat, the printing press appears as an exogenous threat to a working monopoly — a technological force that removes their enforcement capacity. From the reformer's organized seat, it appears as a liberation technology that enables their message to reach scale. From the analytical seat, the printing press is neither threat nor liberation — it is a material fact whose effects follow from its technical properties. The technological determinism reading collapses all three perspectives into a single causal fact: the technology's capability determines the outcome. The engine's per-seat computation should reflect that institutional actors experience high extraction (the loss of their monopoly position) while beneficiary actors experience low extraction (they gain without paying for it), but the underlying constraint is the same material fact — the press exists. Directionality divergence follows from the technological reading's assumption that exogenous material facts have asymmetric effects on differently positioned actors.
 *
 * DIRECTIONALITY LOGIC:
 *   In this reading, directionality is not derived from beneficiary/victim relationships in the usual sense because the constraint is not a human choice or coordination mechanism — it is a technological fact. The Church_authority seat experiences high effective extraction (d near 1.0) because the technology removes their monopoly position and they cannot exit (trapped to defending an increasingly untenable institution). The reformation_movement and lay_literacy seats experience low effective extraction or subsidy (d near 0.0 or negative) because the technology gifts them capability without cost. The analytical observer has d = 0.5 (symmetric, uninvolved). No directionality override is needed because the technological determinism reading treats directionality as a consequence of the technology's material properties, not as a strategic relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The technological determinism reading avoids mandatrophy in the strict sense (a constraint whose original mandate has outlived its function) because the printing press has no mandate — it is not a constructed institution with a founding problem. However, the reading faces a related problem: if the press is treated as a natural/inevitable fact, why does it appear in human history as a contingent invention by Gutenberg rather than as a discovered physical law? The answer — that technological facts are historical contingencies that become inevitable in their consequences once they exist — is itself the reading's core claim. This reading is vulnerable to the charge that it conflates material capability with causal determinism; that vulnerability is captured in the omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_material_contingency,
    'If the printing press is a natural/inevitable fact, why is its invention a historical contingency attributable to Gutenberg? Does treating it as a natural law conflate material capability with historical causation?',
    'Conceptual analysis of whether technological facts can be both historically contingent in their origin and inevitable in their consequences. Examine counterfactual scenarios: would the Reformation have occurred with different printing technology (e.g., earlier technology with lower fidelity)? Would a later inventor have produced the same capability?',
    'If the printing press is a genuinely exogenous technological fact independent of human choice, the determinism reading holds. If its properties and deployment reflect human strategic choices from the moment of its invention, the reading collapses into strategic deployment and mutual shaping — causation becomes distributed rather than technological.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_determinism_vs_material_contingency, conceptual, 'Whether treating the printing press as natural law is coherent given its historical contingency.').

omega_variable(
    monopoly_restoration_possibility,
    'Given that the printing press exists and has diffused, is the Church''s inability to suppress it a material fact (suppression is impossible because the technology is ubiquitous) or a contingent institutional failure (suppression could have worked if attempted earlier/more forcefully)?',
    'Historical reconstruction of printing suppression efforts (Index Librorum Prohibitorum, licensing requirements, destruction of printed books). Counterfactual analysis: could earlier, more draconian suppression in 1450-1480 have prevented the technology''s proliferation before it became economically entrenched?',
    'If suppression was materially impossible once printing reached a critical mass of copies, the mountain claim holds. If institutional suppression could have worked with faster/harsher response, the Church''s failure is strategic rather than technological — moving the causation from the press to institutional choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monopoly_restoration_possibility, empirical, 'Whether the Church''s loss of censorship control was technologically inevitable or institutionally contingent.').

omega_variable(
    reformer_agency_vs_technological_beneficiary,
    'Were the reformers (Luther, Calvin, their printers and networks) passive beneficiaries of technological abundance, or did they strategically exploit and shape how printing technology developed to serve their agenda?',
    'Detailed historical reconstruction of reformer-printer collaboration: who commissioned editions, which texts were chosen for printing, how reformers marketed their works, what distribution networks they built. Compare against non-Reformation texts of the same era to see if printing favored Reformation content or merely enabled all content equally.',
    'Passive beneficiary → technological determinism holds; the Reformation rode inevitable technological change. Strategic exploiter → strategic deployment reading; reformers shaped the technology''s deployment. Mutual shaping → the technology and reformer strategy co-evolved. If reformers'' choices significantly influenced which texts got printed and how printing technology was used, causation is not purely technological.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_agency_vs_technological_beneficiary, empirical, 'Whether reformers were passive beneficiaries of the printing press or active strategic agents in shaping its use.').

omega_variable(
    beneficiary_beneficiary_vs_natural_law_beneficiary,
    'Is declaring reformers and lay literacy as ''beneficiaries'' of a mountain constraint coherent? Natural laws do not have beneficiaries in the usual sense — they simply operate without preference. Does naming beneficiaries here conflate natural technological capability with constructed institutional arrangements?',
    'Conceptual clarification: distinguish between (a) agents positioned to exploit a new possibility space created by technology (reformers), (b) agents who benefit from the institutional consequences of that exploitation (lay readers gaining access to scripture), and (c) agents who benefit from a constructed arrangement designed to extract value (the usual beneficiary sense). Does the printing press create beneficiaries or merely new possibilities?',
    'If the press merely creates possibilities without designed extraction, the beneficiary declarations are misleading — they anthropomorphize the technology. If the technological fact creates predictable winners and losers in a way that resembles extraction, the beneficiary framing is apt. This omega addresses whether the false-summit-mountain signature is correctly triggered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_beneficiary_vs_natural_law_beneficiary, conceptual, 'Whether ''beneficiary'' language is coherent when applied to agents who exploit technological opportunity (as opposed to agents who benefit from constructed extraction).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1440, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1440, press_reformation_causation__technological_determinism, theater_ratio, 1440, 0.01).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causation__technological_determinism, theater_ratio, 1470, 0.03).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.06).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__technological_determinism, theater_ratio, 1517, 0.08).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causation__technological_determinism, theater_ratio, 1530, 0.08).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.08).

% Extraction over time
narrative_ontology:measurement(pres_be_t1440, press_reformation_causation__technological_determinism, base_extractiveness, 1440, 0.02).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causation__technological_determinism, base_extractiveness, 1470, 0.08).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.12).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__technological_determinism, base_extractiveness, 1517, 0.13).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causation__technological_determinism, base_extractiveness, 1530, 0.12).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1440, press_reformation_causation__technological_determinism, suppression_requirement, 1440, 0.02).
narrative_ontology:measurement(pres_su_t1470, press_reformation_causation__technological_determinism, suppression_requirement, 1470, 0.03).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.04).
narrative_ontology:measurement(pres_su_t1517, press_reformation_causation__technological_determinism, suppression_requirement, 1517, 0.05).
narrative_ontology:measurement(pres_su_t1530, press_reformation_causation__technological_determinism, suppression_requirement, 1530, 0.05).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, global_infrastructure).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__technological_determinism, 0.05).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The printing press and Reformation constitute a contested kernel with three structurally distinct readings. This story instantiates the TECHNOLOGICAL_DETERMINISM reading: the technology is exogenous and determines the outcome. The STRATEGIC_DEPLOYMENT reading (sister constraint) treats technology as neutral enabler and reformer agency as primary. The MUTUAL_SHAPING reading treats technology and agency as co-evolving with no primary causation direction. Each reading has a distinct ε, beneficiary structure, and type classification. The three readings are linked by network edges because they dispute the same historical fact under different causal frameworks. The kernel itself — 'did the printing press cause the Reformation?' — is not authored as a constraint; instead, each reading of the kernel is authored as a separate constraint, and the reading_relations in cs_structure declare how they relate to each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
