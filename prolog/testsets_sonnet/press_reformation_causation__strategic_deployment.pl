% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Strategic Deployment Reading: The Press as Coordination Tool Exploited by Reformers and Printers
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the 'strategic deployment' reading of the
 *   press-Reformation causation kernel: the printing press is treated as
 *   neutral technical capacity, and the Reformation's rapid spread is
 *   attributed to the deliberate, purposeful choices of reformers who wrote
 *   for print and printers who ran a profitable product line. Agency is the
 *   upstream driver; the press is downstream infrastructure that
 *   coordination-minded actors captured and pointed at a market. This is a
 *   distinct constraint from the technological_determinism reading
 *   (press-as-cause, censorship-as-impossible) and the mutual_shaping reading
 *   (co-evolution) — those are separate stories with separate epsilon values,
 *   linked here only through the shared kernel network, never merged into
 *   this one's classification.
 *
 * KEY AGENTS:
 *   - protestant_reformers: primary agenda-setters and beneficiaries who chose print as an instrument
 *   - printer_entrepreneurs: commercial beneficiaries who selected profitable content
 *   - sympathetic_territorial_princes: political beneficiaries who sheltered reformist presses
 *   - catholic_censorship_authorities: payers bearing the cost of an adversarial deployment they could not fully suppress
 *   - unlettered_lay_readers_targeted_by_polemic: payers who received persuasive output without a voice in its design
 *   - printing_press_technology: the inert capacity itself, non-agent, causally passive in this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.42).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.28).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Strategic Deployment Reading: The Press as Coordination Tool Exploited by Reformers and Printers").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '8a6f7352-4256-4f3c-997d-cef889af7347').
narrative_ontology:cs_kernel_codification('8a6f7352-4256-4f3c-997d-cef889af7347', distributed).
narrative_ontology:cs_authority_grounding('8a6f7352-4256-4f3c-997d-cef889af7347', distributed).
narrative_ontology:cs_reading_relation('8a6f7352-4256-4f3c-997d-cef889af7347', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('8a6f7352-4256-4f3c-997d-cef889af7347', press_reformation_causation__mutual_shaping, influences).
narrative_ontology:cs_axiom('8a6f7352-4256-4f3c-997d-cef889af7347', foundational, technology_as_neutral_instrument).
narrative_ontology:cs_axiom_status(technology_as_neutral_instrument, holdable).
narrative_ontology:cs_axiom_grounding('8a6f7352-4256-4f3c-997d-cef889af7347', technology_as_neutral_instrument, conventional).
narrative_ontology:cs_axiom('8a6f7352-4256-4f3c-997d-cef889af7347', foundational, human_intent_as_sufficient_causal_driver).
narrative_ontology:cs_axiom_status(human_intent_as_sufficient_causal_driver, holdable).
narrative_ontology:cs_axiom_grounding('8a6f7352-4256-4f3c-997d-cef889af7347', human_intent_as_sufficient_causal_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('8a6f7352-4256-4f3c-997d-cef889af7347', agency_centered_causal_priority).
narrative_ontology:cs_drift_state('8a6f7352-4256-4f3c-997d-cef889af7347', post_print_culture_historiography, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8a6f7352-4256-4f3c-997d-cef889af7347', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, printer_entrepreneurs).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, sympathetic_territorial_princes).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_censorship_authorities).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, unlettered_lay_readers_targeted_by_polemic).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, technology_as_neutral_instrument_doctrine).
narrative_ontology:constraint_vindicates(press_reformation_causation__strategic_deployment, human_agency_primacy_in_historical_causation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deliberately commissioned pamphlets, translated scripture into vernacular, and cultivated relationships with sympathetic printers to circulate doctrine at scale. Chose print over pulpit-only transmission because it multiplied reach per unit of effort; when one print shop was suppressed, moved operations to a friendlier city or territory. The press is a tool they picked up and pointed.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, protestant_reformers, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, protestant_reformers, beneficiary).

% Ran commercial print shops and treated theological pamphlets as a lucrative product line — cheap to produce, high demand, low capital risk compared to bound liturgical volumes. Selected which reformist texts to run based on expected sales and patron protection, not doctrinal conviction alone. Could relocate shops across city and territorial lines to escape hostile magistrates.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, printer_entrepreneurs, beneficiary,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, printer_entrepreneurs, agenda_setter).

% Granted printing privileges and political protection to reformist presses within their territories, using the resulting religious realignment to consolidate authority against imperial or papal claims. Extracted political leverage from a technology they did not build but chose to shelter and license.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, sympathetic_territorial_princes, beneficiary,
    institutional, generational, arbitrage, national).

% Attempted to license, index, and suppress reformist print runs, but faced an opponent that could relocate production across jurisdictional lines faster than enforcement could coordinate. Bore the reputational and doctrinal cost of a coordinated adversarial deployment they could not fully interdict.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_censorship_authorities, payer,
    institutional, generational, constrained, continental).

% Received simplified, often inflammatory vernacular pamphlets crafted specifically for persuasive effect rather than theological nuance, without the literacy or access to counter-print to evaluate competing claims critically. Consumed the output of a strategy aimed at them, not made by them.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, unlettered_lay_readers_targeted_by_polemic, payer,
    powerless, biographical, trapped, regional).

% The mechanical capacity itself: movable type, presses, ink, paper supply chains. In this reading it is inert capacity — it did nothing until reformers and printers chose to point it at a specific market and message. It has no independent causal role beyond being available for capture.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, printing_press_technology, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__strategic_deployment, printing_press_technology).

% Debate retrospectively whether the press caused the Reformation, co-evolved with it, or was merely deployed by it. This story is authored from the seat that holds the deployment reading; the determinist and mutual-shaping seats are separate constraints, not alternative measurements of this one.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, later_historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Print shops functioning as reformist infrastructure solved a genuine coordination problem for a dispersed, persecuted movement: how to standardize doctrine, synchronize messaging across cities, and reach lay audiences faster than ecclesiastical response could adapt.
% TRANSFER_FUNCTION: Moves religious authority and political legitimacy away from centralized Catholic censorship structures toward reformist networks and the territorial princes who sheltered them; moves commercial profit to printers who correctly bet on demand for polemical vernacular texts; moves persuasive framing (not balanced information) to lay readers who lacked means to evaluate it.
% ABSENT_VOICES: Unlettered lay readers targeted by polemic had no seat in deciding what was printed or how arguments were framed; they are described here as payers but were never in the room where deployment strategy was set. Catholic authorities objected loudly but from a position of chronically lagging enforcement capacity, not absence — they are a payer seat, not an absent one.
% DISAPPEARANCE_RATIONALE: Reformers in this reading insist the movement would have found alternative means (preaching networks, manuscript circulation, oral transmission) to spread doctrine even without the press, since agency and intent are the upstream driver — the press was a force multiplier, not the cause. Historians favoring the determinist or mutual-shaping siblings dispute this counterfactual sharply, arguing print's scale was not substitutable. The verdict is genuinely contested between readings of the same kernel.
% FOUNDING_PROBLEM: A dispersed reform movement facing centralized ecclesiastical censorship needed a way to produce, standardize, and distribute doctrinal material faster and more cheaply than manuscript copying or itinerant preaching alone allowed.
% FOUNDING_PROBLEM_CORROBORATION: Media historians outside the reformist tradition (working from printer account books, guild records, and territorial licensing archives) corroborate that the underlying distribution problem was solved definitively by the 17th century once print infrastructure matured and confessional lines stabilized; no serious contemporary constituency claims the original coordination problem is still live. The determinist and mutual-shaping camps corroborate the same historical endpoint from different causal stories, which is itself evidence the disagreement is about causation, not about whether the problem still exists.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, contested).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).
:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42) because deliberate strategic use of print did concentrate real gains — doctrinal authority, commercial profit, political leverage — in the hands of those who chose to deploy it, at real cost to censorship authorities and to lay readers exposed to one-sided framing. But extraction is well below snare-level because the underlying coordination function (faster, cheaper doctrinal distribution across a fragmented movement) is genuine and not merely cover. Suppression is comparatively low (0.28) because in this reading the press itself imposed no coercive structure; the coercion, where present, belonged to the censorship apparatus reacting to deployment, not to the deployment itself. Theater ratio stays low and roughly flat because deployment-reading advocates emphasize genuine functional use of print, not performative activity.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers, printers, and sympathetic princes are declared beneficiaries because they made the deliberate choice to deploy the capacity and captured the resulting doctrinal, commercial, or political returns — their directionality sits near the beneficiary end, reinforced by mobile/arbitrage exit options that let them relocate operations ahead of enforcement. Catholic censorship authorities and lay readers are victims: the former bear the cost of a coordinated adversarial campaign they structurally could not out-maneuver (institutional power, but constrained exit within a jurisdiction-bound enforcement apparatus), the latter bear the cost of one-directional persuasive material with no counter-access (powerless, trapped).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a dispersed reform movement needing fast, standardized doctrinal distribution against centralized censorship — is genuinely dead by the 17th century: confessional lines stabilized, print infrastructure matured, and the original coordination problem was solved. This reading resists mandatrophy mislabeling by keeping the coordination function analytically separate from the causal claim: acknowledging real coordination benefit does not require crediting the press itself with historical agency, and acknowledging strategic extraction by reformers/printers does not require denying the coordination was genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_substitutability_of_print,
    'Could the Reformation''s doctrinal spread have been achieved at comparable speed and scale through pre-print channels (manuscript circulation, itinerant preaching networks) had reformers simply redirected the same strategic intent?',
    'Comparative case study against pre-print heterodox movements (e.g. Lollardy, Hussite circulation) matched for geographic scope and estimated audience size, controlling for the presence/absence of movable type.',
    'If substitutable, this reading''s agency-primacy claim strengthens and the determinist sibling weakens. If not substitutable at anything like the observed scale, the determinist reading''s causal claim strengthens and this reading''s neutral-capacity premise is undermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_substitutability_of_print, empirical, 'Whether print was replaceable infrastructure or a scale-enabling precondition.').

omega_variable(
    neutral_technology_premise_contestability,
    'Is ''technology as neutral capacity awaiting purposeful use'' a defensible general premise, or does it understate structural affordances (cost curves, distribution speed, error-correction via typesetting) that made certain strategic uses easier than others regardless of intent?',
    'Analysis of whether print''s specific technical properties (replicability, standardization, distribution economics) constrained which strategic deployments were even feasible — if so, the technology was not purely neutral capacity but a capacity with directional affordances.',
    'If affordances materially shaped what strategies were viable, the clean agency-upstream/technology-downstream split this reading depends on weakens, pushing the analysis toward the mutual_shaping sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neutral_technology_premise_contestability, conceptual, 'Whether the neutral-instrument premise survives scrutiny of the press''s specific technical affordances.').

omega_variable(
    beneficiary_selection_bias_in_reading,
    'Does the strategic_deployment reading''s framing — which credits reformers and printers with agency and profit — itself reflect a historiographical tradition partly shaped by later liberal/Whiggish narratives that valorize individual agency over technological or structural causation?',
    'Historiographical review of which scholarly traditions favor strategic-deployment framings versus determinist or co-evolutionary framings, and whether those traditions have identifiable institutional or ideological stakes in agency-centered historical narrative.',
    'If the reading itself has beneficiaries among historians or institutions invested in agency-centered narratives, this constraint could itself be a false-summit candidate at the historiographical level — a separate, higher-order constraint not modeled in this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_selection_bias_in_reading, conceptual, 'Whether the choice of this reading over its siblings is itself an interested historiographical act.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causation__strategic_deployment, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(pres_tr_t0, observed).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causation__strategic_deployment, theater_ratio, 20, 0.13).
narrative_ontology:measurement_basis(pres_tr_t20, observed).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causation__strategic_deployment, theater_ratio, 40, 0.17).
narrative_ontology:measurement_basis(pres_tr_t40, observed).
narrative_ontology:measurement(pres_tr_t60, press_reformation_causation__strategic_deployment, theater_ratio, 60, 0.19).
narrative_ontology:measurement_basis(pres_tr_t60, observed).
narrative_ontology:measurement(pres_tr_t80, press_reformation_causation__strategic_deployment, theater_ratio, 80, 0.2).
narrative_ontology:measurement_basis(pres_tr_t80, observed).
narrative_ontology:measurement(pres_tr_t100, press_reformation_causation__strategic_deployment, theater_ratio, 100, 0.2).
narrative_ontology:measurement_basis(pres_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causation__strategic_deployment, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(pres_be_t0, observed).
narrative_ontology:measurement(pres_be_t20, press_reformation_causation__strategic_deployment, base_extractiveness, 20, 0.31).
narrative_ontology:measurement_basis(pres_be_t20, observed).
narrative_ontology:measurement(pres_be_t40, press_reformation_causation__strategic_deployment, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(pres_be_t40, observed).
narrative_ontology:measurement(pres_be_t60, press_reformation_causation__strategic_deployment, base_extractiveness, 60, 0.4).
narrative_ontology:measurement_basis(pres_be_t60, observed).
narrative_ontology:measurement(pres_be_t80, press_reformation_causation__strategic_deployment, base_extractiveness, 80, 0.41).
narrative_ontology:measurement_basis(pres_be_t80, observed).
narrative_ontology:measurement(pres_be_t100, press_reformation_causation__strategic_deployment, base_extractiveness, 100, 0.42).
narrative_ontology:measurement_basis(pres_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__strategic_deployment, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the press_reformation_causation kernel. strategic_deployment (this file) places agency upstream and treats the press as coordination infrastructure captured by interested actors (rope-flavored, moderate extraction concentrated in identifiable beneficiaries). technological_determinism places the press's technical properties upstream and treats reformist success as a near-inevitable consequence (mountain-flavored, low extraction, high accessibility_collapse). mutual_shaping denies either clean causal ordering and models co-evolution (structurally distinct metrics, likely tangled_rope-flavored given feedback loops between technology and use). Each has its own epsilon; none should be read as a measurement of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
