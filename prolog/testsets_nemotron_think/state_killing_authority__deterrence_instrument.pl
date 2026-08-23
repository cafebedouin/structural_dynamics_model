% ============================================================================
% CONSTRAINT STORY: state_killing_authority__deterrence_instrument
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__deterrence_instrument, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: state_killing_authority__deterrence_instrument
 *   human_readable: Deterrence-Based Capital Punishment Justification
 *   domain: criminal_justice/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story models the deterrence-instrument reading of the
 *   state_killing_authority kernel: capital punishment is justified if and
 *   only if it prevents future murders at acceptable cost. The constraint is
 *   the institutional framework that authorizes executions conditional on a
 *   positive deterrence calculus. It extracts life from condemned persons
 *   (payers) to produce a statistical benefit for future potential victims
 *   (beneficiaries), with the state as agenda-setter defining 'acceptable
 *   cost.' The reading coexists with retributive_desert (different parties
 *   hold each) and influences categorical_abolition (empirical failure of
 *   deterrence strengthens abolitionist position). The claimed type is
 *   tangled_rope — genuine coordination (deterrence) fused with asymmetric
 *   extraction (condemned pay the cost).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, 0.68).
domain_priors:suppression_score(state_killing_authority__deterrence_instrument, 0.72).
domain_priors:theater_ratio(state_killing_authority__deterrence_instrument, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(state_killing_authority__deterrence_instrument, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__deterrence_instrument, tangled_rope).
narrative_ontology:human_readable(state_killing_authority__deterrence_instrument, "Deterrence-Based Capital Punishment Justification").
narrative_ontology:topic_domain(state_killing_authority__deterrence_instrument, "criminal_justice/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(state_killing_authority__deterrence_instrument).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__deterrence_instrument, 'eea6632d-b74d-41e7-aa2c-6f352002d850').
narrative_ontology:cs_kernel_codification('eea6632d-b74d-41e7-aa2c-6f352002d850', formalized).
narrative_ontology:cs_authority_grounding('eea6632d-b74d-41e7-aa2c-6f352002d850', extraction).
narrative_ontology:cs_interpretation_layer_present('eea6632d-b74d-41e7-aa2c-6f352002d850').
narrative_ontology:cs_reading_relation('eea6632d-b74d-41e7-aa2c-6f352002d850', state_killing_authority__retributive_desert, coexists_with).
narrative_ontology:cs_reading_relation('eea6632d-b74d-41e7-aa2c-6f352002d850', state_killing_authority__categorical_abolition, influences).
narrative_ontology:cs_axiom('eea6632d-b74d-41e7-aa2c-6f352002d850', foundational, deterrence_justifies_killing).
narrative_ontology:cs_axiom_status(deterrence_justifies_killing, holdable).
narrative_ontology:cs_axiom_grounding('eea6632d-b74d-41e7-aa2c-6f352002d850', deterrence_justifies_killing, empirically_contingent).
narrative_ontology:cs_axiom('eea6632d-b74d-41e7-aa2c-6f352002d850', secondary, acceptable_cost_threshold_exists).
narrative_ontology:cs_axiom_status(acceptable_cost_threshold_exists, holdable).
narrative_ontology:cs_axiom_grounding('eea6632d-b74d-41e7-aa2c-6f352002d850', acceptable_cost_threshold_exists, conventional).
narrative_ontology:cs_reference_frame('eea6632d-b74d-41e7-aa2c-6f352002d850', classical_punitive_authority).
narrative_ontology:cs_drift_state('eea6632d-b74d-41e7-aa2c-6f352002d850', contemporary_empirical_challenge, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eea6632d-b74d-41e7-aa2c-6f352002d850', '').
narrative_ontology:cs_kernel_id(state_killing_authority__deterrence_instrument, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, future_potential_victims).
narrative_ontology:constraint_beneficiary(state_killing_authority__deterrence_instrument, general_public_safety).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_persons).
narrative_ontology:constraint_victim(state_killing_authority__deterrence_instrument, condemned_families).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, state_monopoly_on_violence).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, consequentialist_penal_theory).
narrative_ontology:constraint_vindicates(state_killing_authority__deterrence_instrument, deterrence_efficacy_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislatures, courts, and executive branches that authorize, adjudicate, and carry out executions. They define 'acceptable cost' thresholds, commission deterrence studies, and control the machinery of death. They benefit from the deterrence framing as it legitimizes their authority to kill. Exit means ceding penal authority to abolitionist or retributive frameworks.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, state_execution_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals sentenced to death under the deterrence framework. They bear the ultimate instrumental cost — their lives — as the mechanism by which deterrence is supposedly produced. No meaningful exit exists once condemned; legal appeals are procedural, not structural alternatives. Their families bear collateral costs.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_persons, payer,
    powerless, immediate, trapped, local).

% Statistical persons whose lives are claimed to be saved by the deterrent effect. They do not exist as identifiable agents at the time of policy; they are a projected beneficiary class. Their 'exit' is the counterfactual world where they are murdered — they cannot opt out of the deterrence calculation that treats their hypothetical survival as the justification.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, future_potential_victims, beneficiary,
    powerless, biographical, constrained, national).

% Families of the executed who bear grief, stigma, financial burden, and the knowledge that their loved one's death was justified by a statistical claim about strangers. They have no voice in the deterrence calculus and no structural exit from the consequences.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, condemned_families, payer,
    powerless, biographical, constrained, local).

% Organizations and activists who reject the deterrence justification entirely (categorical abolition) or dispute its empirical basis. They are structurally excluded from the deterrence calculus — their objection is that no 'acceptable cost' of state killing exists. They operate outside the framework, seeking to dismantle it.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, abolitionist_advocates, excluded,
    organized, generational, mobile, national).

% Empirical researchers who study whether executions actually deter murder. Their findings feed the 'acceptable cost' threshold but they do not set policy. They see the full structure: the deterrence claim, the empirical uncertainty, the instrumental use of condemned lives. Their exit is intellectual — they can change their conclusion without personal cost.
narrative_ontology:constraint_stakeholder(state_killing_authority__deterrence_instrument, criminologists_deterrence_researchers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social order by credibly threatening the ultimate penalty for murder, thereby (putatively) reducing homicide rates through rational deterrence. Solves the collective-action problem of private vengeance by monopolizing lethal punishment under a calculable rule.
% TRANSFER_FUNCTION: Transfers the lives of condemned persons (instrumental cost) to the statistical account of future potential victims (lives putatively saved). The state claims the transfer ratio is favorable — each execution prevents multiple murders — making the net transfer a social gain.
% ABSENT_VOICES: The condemned themselves (silenced by the constraint), the statistical future victims (unborn/unidentified), international human rights bodies that categorically reject state killing, and the families of murder victims who oppose capital punishment — these voices are not represented in the deterrence calculus that weighs 'acceptable cost.'
% DISAPPEARANCE_RATIONALE: If the deterrence justification vanished overnight, capital punishment would not simply continue on retributive grounds in most jurisdictions — the deterrence claim is the primary empirical legitimator for retentionist legislatures and courts. Its disappearance would trigger abolition or moratorium in deterrence-dependent jurisdictions, shift the burden of proof to retributive justifications, and reorganize the penal landscape around life-without-parole as the maximum penalty.
% FOUNDING_PROBLEM: Mid-20th century murder rate spikes, perceived failure of rehabilitation, public demand for 'law and order,' and the Furman v. Georgia (1972) voiding of existing statutes created pressure for a constitutionally defensible, empirically grounded justification for the death penalty. The deterrence framework answered: 'We kill only if it saves more lives than it takes.'
% FOUNDING_PROBLEM_CORROBORATION: The deterrence claim is corroborated as empirically contested by the National Research Council (2012) meta-analysis finding no credible evidence of deterrence, by criminologist surveys (e.g., Radelet & Lacock 2009) showing ~88% of experts reject deterrence, and by the American Law Institute's 2009 withdrawal of the Model Penal Code's death penalty provisions citing unfixable flaws. Law enforcement organizations often assert deterrence but rarely cite empirical support. No non-benefiting party attests the founding problem remains live in its original form.
narrative_ontology:disappearance_verdict(state_killing_authority__deterrence_instrument, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__deterrence_instrument, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__deterrence_instrument, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_killing_authority__deterrence_instrument, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__deterrence_instrument, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__deterrence_instrument_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__deterrence_instrument, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__deterrence_instrument_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the ultimate extraction — life — from a powerless class, moderated by the reading's own claim that the transfer ratio is favorable (multiple lives saved per execution). Suppression (0.72) is high: condemned persons have zero exit, the state monopoly on violence is absolute, and legal process is procedural not substantive alternative. Theater ratio (0.42) captures the growing gap between the deterrence claim and empirical reality — the coordination function is increasingly performative as evidence accumulates against deterrence. Accessibility collapse (0.75) is high because the deterrence framework, once accepted as the constitutional standard (Gregg v. Georgia 1976), structurally marginalizes abolitionist and retributive alternatives. Resistance (0.65) reflects sustained abolitionist litigation, declining execution rates, and international pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the state_execution_authority seat, the constraint appears as genuine coordination: a rational, evidence-based policy that saves net lives. From the condemned_persons seat, it is pure extraction: their life taken for a statistical claim they cannot verify and a benefit they cannot experience. The future_potential_victims seat is a philosophical construct — no actual agent occupies it at decision time. The engine's per-seat classification will reveal this divergence: the same constraint computes as rope-like for the authority, snare-like for the condemned, and mountain-like (naturalized) for the statistical beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   State_execution_authority is structural beneficiary (d near 0.0): it gains legitimacy, penal authority, and political capital from the deterrence framework. Condemned_persons are full targets (d near 1.0): trapped, powerless, bearing the extraction directly. Future_potential_victims are statistical beneficiaries (d near 0.0) but with constrained exit — they cannot opt out of being the justification. Condemned_families are collateral payers (d ~0.7). Abolitionist_advocates are excluded (d undefined by derivation) — their structural position is outside the constraint's coordinate system. Criminologists are analytical observers (d = 0.5 by definition). The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1970s murder spike, constitutional vacuum) is dead or contested — murder rates have declined without executions in abolitionist states, and the deterrence evidence has not strengthened. Yet the constraint persists. This is NOT mandatrophy resolved (the arrangement hasn't been acknowledged as obsolete) — it is active mandatrophy: the original justification is empirically undermined but the constraint survives through institutional inertia, political symbolism, and the retributive_desert reading's shadow support. The theater_ratio rise tracks this: more performance, less functional deterrence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_empirical_uncertainty,
    'Does capital punishment actually deter murder at a rate that meets the ''acceptable cost'' threshold claimed by the reading?',
    'Natural experiments from abolition/retention jurisdictions, panel studies with improved identification strategies, or a definitive meta-analysis accepted by both retentionist and abolitionist criminologists.',
    'If deterrence is empirically falsified (effect size ~0 or negative), the reading''s foundational axiom collapses — the constraint becomes pure extraction (snare) or loses its coordination function entirely. If deterrence is confirmed with a high effect size, the reading gains empirical legitimacy and the extraction may be reclassified as coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrence_empirical_uncertainty, empirical, 'The core empirical claim on which the reading''s justification rests remains unresolved after 50+ years of study.').

omega_variable(
    acceptable_cost_threshold_ambiguity,
    'What constitutes ''acceptable cost'' in the deterrence calculus — how many condemned lives per statistical life saved, and who decides?',
    'Legislative or judicial articulation of a quantitative threshold, or a constitutional ruling that the threshold is inherently unquantifiable and therefore the framework fails strict scrutiny.',
    'If ''acceptable cost'' is defined (e.g., 1:5 ratio), the constraint becomes a measurable policy instrument. If it remains undefined, the reading functions as a blank check — the state kills and claims deterrence without ever demonstrating the ratio. Ambiguity enables the theater_ratio to rise unchecked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(acceptable_cost_threshold_ambiguity, conceptual, 'The reading''s central normative parameter is never operationalized, creating a structural loophole for extraction.').

omega_variable(
    foreclosure_relation_to_abolition,
    'Does the deterrence_instrument reading logically foreclose the categorical_abolition reading within a single legal framework, or do they coexist as competing live positions?',
    'A constitutional ruling that deterrence is the ONLY permissible justification for capital punishment (foreclosing retributive_desert but not abolition), or a ruling that the deterrence framework is incompatible with human dignity (foreclosing this reading).',
    'If forecloses: the kernel collapses to a binary choice between this reading and abolition. If coexists_with: all three readings remain live, creating a three-way contest. If influences: empirical failure of deterrence shifts legitimacy to abolition without logical foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foreclosure_relation_to_abolition, conceptual, 'The structural relationship between this reading and its abolitionist sibling determines whether empirical falsification of deterrence legally eliminates the constraint or merely weakens it politically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__deterrence_instrument, 1972, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1972, state_killing_authority__deterrence_instrument, theater_ratio, 1972, 0.25).
narrative_ontology:measurement(stat_tr_t1985, state_killing_authority__deterrence_instrument, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(stat_tr_t1995, state_killing_authority__deterrence_instrument, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(stat_tr_t2005, state_killing_authority__deterrence_instrument, theater_ratio, 2005, 0.43).
narrative_ontology:measurement(stat_tr_t2015, state_killing_authority__deterrence_instrument, theater_ratio, 2015, 0.41).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__deterrence_instrument, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t1972, state_killing_authority__deterrence_instrument, base_extractiveness, 1972, 0.55).
narrative_ontology:measurement(stat_be_t1985, state_killing_authority__deterrence_instrument, base_extractiveness, 1985, 0.62).
narrative_ontology:measurement(stat_be_t1995, state_killing_authority__deterrence_instrument, base_extractiveness, 1995, 0.68).
narrative_ontology:measurement(stat_be_t2005, state_killing_authority__deterrence_instrument, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement(stat_be_t2015, state_killing_authority__deterrence_instrument, base_extractiveness, 2015, 0.69).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__deterrence_instrument, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1972, state_killing_authority__deterrence_instrument, suppression_requirement, 1972, 0.65).
narrative_ontology:measurement(stat_su_t1985, state_killing_authority__deterrence_instrument, suppression_requirement, 1985, 0.7).
narrative_ontology:measurement(stat_su_t1995, state_killing_authority__deterrence_instrument, suppression_requirement, 1995, 0.73).
narrative_ontology:measurement(stat_su_t2005, state_killing_authority__deterrence_instrument, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(stat_su_t2015, state_killing_authority__deterrence_instrument, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__deterrence_instrument, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__deterrence_instrument, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__retributive_desert).
narrative_ontology:affects_constraint(state_killing_authority__deterrence_instrument, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This constraint is the deterrence_instrument reading of the state_killing_authority kernel. It differs structurally from retributive_desert (which has no beneficiary class — the condemned is a moral debtor, not an instrumental cost) and categorical_abolition (which has no victim class — no one is extracted from because the constraint forbids extraction). The three readings share the same kernel (state authority to kill) but instantiate different constraints with different ε, beneficiaries, victims, and types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__deterrence_instrument, powerless, 0.95).
constraint_indexing:directionality_override(state_killing_authority__deterrence_instrument, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
