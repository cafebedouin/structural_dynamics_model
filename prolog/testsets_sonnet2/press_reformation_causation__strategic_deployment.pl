% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   human_readable: The Printing Press as Strategically Deployed Coordination Tool (Reformation)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the 'strategic_deployment' reading of the
 *   press-Reformation-causation kernel: the printing press is treated as a
 *   neutral, pre-existing capacity that reformers and printer-entrepreneurs
 *   deliberately picked up and wielded as one tool among several available
 *   for their independent theological and commercial projects. Under this
 *   reading, causal primacy sits with human agency and strategic choice —
 *   reformers selected print because of its speed/reach advantages over
 *   preaching or manuscript circulation, and printers responded to commercial
 *   incentives by favoring controversialist pamphlets. The press itself,
 *   absent this deliberate uptake, would not have driven reform outcomes; the
 *   technology was capacity awaiting purposeful use. This is distinct from
 *   the sibling readings: 'technological_determinism' claims the press's
 *   inherent properties (rapid reproduction, decentralized production) made
 *   censorship structurally impossible and vernacular scripture inevitable
 *   regardless of who used it; 'mutual_shaping' claims press and reform
 *   agency co-evolved iteratively, each reshaping the other over time.
 *   Because ε (extraction) and beneficiary structure differ meaningfully
 *   across these three claims — this reading assigns moderate extraction
 *   concentrated in deliberate beneficiary choices, not diffuse technological
 *   inevitability — they are authored as three separate constraint files per
 *   the ε-invariance principle, linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - protestant_reformers: primary strategic agent, deliberately selects print as instrument (organized/mobile)
 *   - printer_entrepreneurs: commercial beneficiary, responds to and shapes the market for reform content (moderate/mobile)
 *   - catholic_hierarchy_incumbents: outmaneuvered incumbent, loses doctrinal monopoly through being strategically outcompeted, not technologically overrun (institutional/constrained)
 *   - unlicensed_scribal_copyists: displaced incumbent trade, bears the cost of the strategic pivot with no voice in the decision (powerless/trapped)
 *   - media_historians: analytical observer weighing agency versus affordance evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.42).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.28).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.42).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "The Printing Press as Strategically Deployed Coordination Tool (Reformation)").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '681108ab-8878-40d6-9077-30c47b667783').
narrative_ontology:cs_kernel_codification('681108ab-8878-40d6-9077-30c47b667783', distributed).
narrative_ontology:cs_authority_grounding('681108ab-8878-40d6-9077-30c47b667783', distributed).
narrative_ontology:cs_reading_relation('681108ab-8878-40d6-9077-30c47b667783', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('681108ab-8878-40d6-9077-30c47b667783', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('681108ab-8878-40d6-9077-30c47b667783', foundational, technology_as_neutral_instrument_awaiting_agency).
narrative_ontology:cs_axiom_status(technology_as_neutral_instrument_awaiting_agency, holdable).
narrative_ontology:cs_axiom_grounding('681108ab-8878-40d6-9077-30c47b667783', technology_as_neutral_instrument_awaiting_agency, empirically_contingent).
narrative_ontology:cs_axiom('681108ab-8878-40d6-9077-30c47b667783', foundational, deliberate_actor_choice_is_primary_causal_variable).
narrative_ontology:cs_axiom_status(deliberate_actor_choice_is_primary_causal_variable, holdable).
narrative_ontology:cs_axiom_grounding('681108ab-8878-40d6-9077-30c47b667783', deliberate_actor_choice_is_primary_causal_variable, empirically_contingent).
narrative_ontology:cs_reference_frame('681108ab-8878-40d6-9077-30c47b667783', instrumentalist_agency_primacy).
narrative_ontology:cs_drift_state('681108ab-8878-40d6-9077-30c47b667783', contemporary_media_historiography, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('681108ab-8878-40d6-9077-30c47b667783', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, printer_entrepreneurs).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, territorial_princes_backing_reform).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, catholic_hierarchy_incumbents).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, unlicensed_scribal_copyists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, lay_readers_and_congregations).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, lay_readers_and_congregations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Figures like Luther and his circle deliberately commissioned pamphlets, sermons, and translated scripture for print runs, chose vernacular language for reach, and coordinated with sympathetic printers to flood specific markets ahead of ecclesiastical response. They treat the press as an instrument selected among available options (preaching, manuscript circulation, oral disputation) because it maximized speed and reach for their existing theological project.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, protestant_reformers, beneficiary,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, protestant_reformers, agenda_setter).

% Print shop owners in cities like Wittenberg, Basel, and Strasbourg identify reform pamphlets as a reliably profitable product line — cheap to typeset, fast-selling, low capital risk compared to elaborate liturgical books. They actively solicit reformer manuscripts, negotiate print runs, and shift capacity toward the most commercially successful controversialist authors. Their exit option is simply printing something else if the market shifts.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, printer_entrepreneurs, beneficiary,
    moderate, biographical, mobile, regional).

% Regional rulers who calculate that backing reform serves their consolidation of authority against imperial and papal claims. They license reform printing within their territories, using the press instrumentally to build a portable propaganda infrastructure that also strengthens their own legitimacy claims independent of Rome.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, territorial_princes_backing_reform, beneficiary,
    institutional, generational, mobile, national).

% Bishops and the papal curia find their prior monopoly on doctrinal messaging outcompeted commercially and rhetorically — reform pamphlets simply out-publish counter-reform tracts in the early decades because reformers deployed the medium with more tactical skill and market sense. Their counter-strategy (indulgence-funded print counter-campaigns, condemnations) arrives structurally late; under this reading their loss is a strategic outmaneuvering, not a technological inevitability.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_hierarchy_incumbents, payer,
    institutional, generational, constrained, continental).

% Professional manuscript copyists whose livelihood depended on hand-produced texts see their commissions collapse as reformers and printers jointly redirect demand toward cheap printed vernacular tracts. They have no comparable capital to enter print production and no alternative market for slow, expensive manuscript work once the strategic reallocation of demand takes hold.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, unlicensed_scribal_copyists, payer,
    powerless, biographical, trapped, local).

% Ordinary readers gain unprecedented access to vernacular scripture and controversialist argument, which the reformers deliberately targeted at them as an audience to be won. They also become recipients of one-sided, strategically curated content — pamphlet wars selected for persuasive force rather than balanced exposition, since both reform and counter-reform producers optimized for winning adherents, not informing neutrally.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, lay_readers_and_congregations, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, lay_readers_and_congregations, payer).

% Scholars reconstructing print runs, correspondence between reformers and printers, and commissioning records to assess how much of the Reformation's spread reflects deliberate strategic choice versus emergent technological affordance. They read letters showing reformers explicitly discussing print strategy, pricing, and market targeting as evidence for this reading.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press, under this reading, coordinates a genuine collective-action problem: getting a theological message reproduced accurately and distributed widely faster than any single author or scriptorium could manage alone, at a price ordinary readers could afford. Reformers and printers jointly solved a distribution bottleneck neither could solve individually.
% TRANSFER_FUNCTION: Deliberate strategic use of print moves theological authority and audience allegiance away from the Catholic hierarchy's prior monopoly on doctrinal communication, and moves commercial revenue and market position toward printers and reform-aligned territorial rulers who backed the winning strategy early.
% ABSENT_VOICES: The scribal copyists whose trade the strategic print pivot displaced are not represented in reformer or printer correspondence as a consideration at all — their economic destruction is a side effect of a strategy aimed entirely at theological and commercial ends, never weighed as a cost by the deploying agents.
% DISAPPEARANCE_RATIONALE: If reformers and printers had NOT strategically deployed the press this way — had they relied on preaching and manuscript circulation as before, or had printers not commercially favored controversialist pamphlets — the pace, geographic breadth, and vernacular character of reform's spread would plausibly have looked very different: slower, more elite-clerical, more containable by episcopal authority.
% FOUNDING_PROBLEM: Reformers needed to defeat the Catholic hierarchy's control over doctrinal communication and reach a lay audience directly and quickly, before ecclesiastical or political suppression could contain a regionally confined movement.
% FOUNDING_PROBLEM_CORROBORATION: Media historians outside the reform tradition (analyzing surviving printer account books, correspondence, and comparative print-run data across confessional lines) corroborate that the original communications bottleneck reformers faced no longer exists in any form — the strategic deployment achieved its purpose and the underlying problem (a monopolized, slow doctrinal communication channel) was permanently dissolved by the outcome itself, not merely superseded. No party still treats the original 16th-century bottleneck as live; the arrangement's causal claim survives now purely as a historiographical dispute about how to weight agency versus affordance.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate (0.42 by 1555) because under this reading the coordination function (rapid, affordable distribution of vernacular theological content) is genuine and substantial, but reformers and printers also capture concentrated commercial and political rents from their early strategic positioning — the Catholic hierarchy's late-arriving counter-print campaigns and the permanent collapse of the scribal-copying trade represent real, asymmetric costs borne by parties who did not choose the strategy. Suppression is comparatively low (0.28) because this reading does not depend on active coercion to persist — reformers and printers succeeded through market and rhetorical competition, not through suppressing rivals' access to the press itself (early on, Catholic authorities used the SAME technology; they were simply out-strategized, not blocked). Theater ratio stays low throughout (0.15 by endpoint) because the deployment is functionally substantive: pamphlets, translations, and print runs did the coordination work claimed, with little performative excess. Resistance is moderate (0.45): the Catholic hierarchy did contest the reformers' use of print vigorously, but the resistance targeted content and doctrine, not the printing mechanism's availability.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer/printer seats, the arrangement reads as pure rope: a coordination tool chosen deliberately and used to solve a real distribution problem, with profit and reach as fair returns on strategic initiative. From the catholic_hierarchy_incumbents seat, the same structure reads closer to tangled_rope or even snare-adjacent: they experience being systematically outcompeted through a communications channel their rivals moved into faster and more skillfully, extracting doctrinal authority and lay allegiance that had previously been theirs. From the unlicensed_scribal_copyists seat, there is no coordination benefit at all — only displacement, since their trade was never a party to the strategic calculation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (protestant_reformers, printer_entrepreneurs, territorial_princes_backing_reform) sit near the low end of directionality because they chose the deployment, captured the resulting audience/revenue/legitimacy, and retained exit options (mobile — could have pursued other communication or governance strategies). Victims (catholic_hierarchy_incumbents, unlicensed_scribal_copyists) sit toward the high end: the hierarchy is constrained (large institutional apparatus, cannot simply exit the doctrinal-authority contest) and the copyists are trapped (no capital to re-enter as printers, no alternative market for hand-copied texts). Lay readers are dual-positioned: genuine beneficiaries of access, but also targets of one-sided persuasive material optimized by both sides for winning adherents rather than informing — hence the secondary_role of payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (defeating a monopolized, slow doctrinal communication channel) is authored as dead: the strategic deployment succeeded so completely that the communications bottleneck it was built to overcome no longer exists in any recognizable form. This is not mandatrophy in the sense of an extractive arrangement persisting past its function — the reading describes a completed historical episode, not an ongoing constraint. The disappearance_verdict is world_rearranges because the counterfactual (no strategic deployment) plausibly yields a different pace and geography of reform, which is precisely the causal claim this reading stakes out against the technological_determinism sibling (which would predict the same outcome even without deliberate strategy) and the mutual_shaping sibling (which would predict a different, iteratively co-produced outcome).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_vs_affordance_weighting,
    'How much of the Reformation''s spread is attributable to reformers''/printers'' deliberate strategic choices versus the press''s inherent technical affordances (rapid reproduction, low marginal cost, decentralized production sites)?',
    'Comparative historical analysis: examine cases where the press was available but strategic deployment was absent or weak (e.g., regions with printing capacity but limited reform organizing) versus cases with strong strategic deployment and limited print infrastructure. Convergent versus divergent outcomes would help apportion causal weight.',
    'If affordance dominates even in the absence of matched strategic effort, this reading is displaced toward technological_determinism. If strategic effort proves necessary even where affordance was present, this reading is corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_vs_affordance_weighting, conceptual, 'The kernel''s central agency-versus-technology apportionment question, as it bears on this specific reading.').

omega_variable(
    counterfactual_reform_without_print_strategy,
    'Would a comparably rapid and wide Reformation have occurred if reformers had pursued the same theological program using only preaching and manuscript circulation, without deliberately prioritizing print?',
    'Analysis of pre-print heretical/reform movements (Hussites, Waldensians, Wycliffites) for comparative spread rate and containment, controlling for political and geographic factors, to estimate what a print-strategy-free 16th century reform movement might have achieved.',
    'If pre-print reform movements achieved comparable regional spread under comparable political conditions, the strategic_deployment reading''s causal claim weakens substantially in favor of mutual_shaping or determinism. If pre-print movements were reliably contained while this one was not, it strengthens the claim that deliberate print strategy was the decisive causal variable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_reform_without_print_strategy, empirical, 'Counterfactual test of whether strategic print deployment was causally necessary, not merely present.').

omega_variable(
    reformer_printer_alignment_intentionality,
    'Is the close alignment between reformer theological goals and printer commercial incentives evidence of deliberate joint strategy, or a retrospectively-narrated coincidence of independently-motivated actors whose interests happened to converge?',
    'Close reading of surviving correspondence and contracts between named reformers and printers for explicit strategic language versus purely transactional commissioning records.',
    'Strong evidence of explicit joint strategizing supports the strategic_deployment reading''s beneficiary/agency framing; evidence of purely transactional, uncoordinated convergence would push the story toward mutual_shaping''s iterative co-production framing instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformer_printer_alignment_intentionality, empirical, 'Whether the reformer-printer alignment reflects conscious strategy or convergent independent incentive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__strategic_deployment, theater_ratio, 1517, 0.08).
narrative_ontology:measurement(pres_tr_t1522, press_reformation_causation__strategic_deployment, theater_ratio, 1522, 0.1).
narrative_ontology:measurement(pres_tr_t1527, press_reformation_causation__strategic_deployment, theater_ratio, 1527, 0.12).
narrative_ontology:measurement(pres_tr_t1534, press_reformation_causation__strategic_deployment, theater_ratio, 1534, 0.13).
narrative_ontology:measurement(pres_tr_t1546, press_reformation_causation__strategic_deployment, theater_ratio, 1546, 0.14).
narrative_ontology:measurement(pres_tr_t1555, press_reformation_causation__strategic_deployment, theater_ratio, 1555, 0.15).

% Extraction over time
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__strategic_deployment, base_extractiveness, 1517, 0.22).
narrative_ontology:measurement(pres_be_t1522, press_reformation_causation__strategic_deployment, base_extractiveness, 1522, 0.3).
narrative_ontology:measurement(pres_be_t1527, press_reformation_causation__strategic_deployment, base_extractiveness, 1527, 0.36).
narrative_ontology:measurement(pres_be_t1534, press_reformation_causation__strategic_deployment, base_extractiveness, 1534, 0.4).
narrative_ontology:measurement(pres_be_t1546, press_reformation_causation__strategic_deployment, base_extractiveness, 1546, 0.41).
narrative_ontology:measurement(pres_be_t1555, press_reformation_causation__strategic_deployment, base_extractiveness, 1555, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__strategic_deployment, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__strategic_deployment, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the press_reformation_causation kernel, each authored as a separate file with its own ε and stakeholder structure per the ε-invariance principle: strategic_deployment (this file, claimed as rope, ε=0.42, agency-primary), technological_determinism (claimed as mountain-leaning given the causal-inevitability framing, likely lower ε as no deliberate extraction is claimed), and mutual_shaping (likely tangled_rope, ε intermediate, reflecting iterative co-production with distributed rather than concentrated beneficiary capture). The three do not average into one constraint; they are linked here to preserve the family relationship for contamination/network analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
