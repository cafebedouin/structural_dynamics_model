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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Strategic Deployment of the Press in the Reformation
 *   domain: history of technology/religious history/media studies
 *
 * SUMMARY:
 *   This constraint story instantiates the strategic_deployment reading of
 *   the press_reformation_causation kernel. The historical arrangement is the
 *   coordination network that formed between Reformation leaders, vernacular
 *   printers, and the reading public during the early Protestant Reformation
 *   (roughly 1517â1555). Under this reading, the printing press is treated
 *   as a neutral coordination toolâa ropeârather than as a deterministic
 *   causal force or as a co-evolving system. Reformers supplied ideological
 *   content and strategic direction; printers supplied production capacity
 *   and market distribution; readers supplied demand and material support.
 *   The Catholic clergy and manuscript culture were structurally outside this
 *   network. The authored metrics reflect low extraction and low suppression
 *   because the mechanism operated through voluntary market exchange and
 *   genuine information coordination; the engine will compute per-seat
 *   classifications from this structural data.
 *
 * KEY AGENTS:
 *   - reformation_leaders (agenda_setter/organized): supplied content and direction, experienced the press as a low-extraction coordination tool
 *   - vernacular_printers (beneficiary/moderate): supplied production and distribution, experienced the arrangement as profitable market coordination
 *   - literate_public (beneficiary/moderate): supplied demand and attention, experienced the arrangement as expanded information access
 *   - catholic_clergy (excluded/institutional): structurally outside the coordination network, would have objected to its operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.25).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.15).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.25).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Strategic Deployment of the Press in the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history of technology/religious history/media studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, '20e708d6-4f12-40de-89ae-17d8b97a31a1').
narrative_ontology:cs_kernel_codification('20e708d6-4f12-40de-89ae-17d8b97a31a1', distributed).
narrative_ontology:cs_authority_grounding('20e708d6-4f12-40de-89ae-17d8b97a31a1', expertise).
narrative_ontology:cs_interpretation_layer_present('20e708d6-4f12-40de-89ae-17d8b97a31a1').
narrative_ontology:cs_reading_relation('20e708d6-4f12-40de-89ae-17d8b97a31a1', press_reformation_causation__technological_determinism, forecloses).
narrative_ontology:cs_reading_relation('20e708d6-4f12-40de-89ae-17d8b97a31a1', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('20e708d6-4f12-40de-89ae-17d8b97a31a1', foundational, instrumental_agency_primacy).
narrative_ontology:cs_axiom_status(instrumental_agency_primacy, holdable).
narrative_ontology:cs_axiom_grounding('20e708d6-4f12-40de-89ae-17d8b97a31a1', instrumental_agency_primacy, conventional).
narrative_ontology:cs_axiom('20e708d6-4f12-40de-89ae-17d8b97a31a1', foundational, technological_neutrality_thesis).
narrative_ontology:cs_axiom_status(technological_neutrality_thesis, holdable).
narrative_ontology:cs_axiom_grounding('20e708d6-4f12-40de-89ae-17d8b97a31a1', technological_neutrality_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('20e708d6-4f12-40de-89ae-17d8b97a31a1', instrumentalist_agency_upstream).
narrative_ontology:cs_drift_state('20e708d6-4f12-40de-89ae-17d8b97a31a1', contemporary_sts_challenges, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('20e708d6-4f12-40de-89ae-17d8b97a31a1', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, reformation_leaders).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, literate_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Provided the ideological content and strategic direction for the print campaign. They chose which texts to prioritize, which languages to use, and which theological arguments to disseminate. They benefited from rapid, cross-border distribution that manuscript culture could not match. Their exit option was to return to sermons, epistolary networks, or manuscript dissemination, though these were slower and narrower in reach.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, reformation_leaders, agenda_setter,
    organized, biographical, mobile, continental).

% Operated the physical production and distribution infrastructure. They made strategic commercial decisions about which reform texts to print, how large the runs should be, and where to ship copies. They profited from strong market demand for controversial and vernacular religious material. Their exit option was to return to printing classics, calendars, or other non-religious works, though the Reformation market was unusually lucrative.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, vernacular_printers, beneficiary,
    moderate, biographical, mobile, continental).

% Purchased and read pamphlets, sermons, and vernacular scriptures. Gained unprecedented direct access to competing theological arguments outside parish and university gatekeeping. Their participation was voluntary and market-mediated. Their exit option was to ignore the new printed material and continue receiving religious information through traditional channels.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, literate_public, beneficiary,
    moderate, biographical, mobile, continental).

% Held a monopoly on religious interpretation prior to the press explosion. They were structurally excluded from the reformer-printer coordination network and would have objected to its operation, but their ability to suppress it was fragmented across dozens of independent principalities with varying political interests. They were not participants in the press constraint and did not transfer resources into it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, catholic_clergy, excluded,
    institutional, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved the collective action problem of disseminating dissenting theological ideas across dozens of politically fragmented principalities and cities. It connected three dispersed groupsâcontent producers (reformers), production specialists (printers), and consumers (reading public)âwho lacked a centralized ecclesiastical or political authority to sanction their coordination.
% TRANSFER_FUNCTION: Moved money from the literate public to printers through voluntary market exchange for texts. Moved ideological commitment and political allegiance from the Catholic institutional information monopoly to reform movements. Moved information and strategic direction from reformation leaders to a geographically dispersed audience.
% ABSENT_VOICES: Catholic authorities and manuscript producers would have argued for maintaining the prior information monopoly and guild-based production norms, but they were not inside the reformer-printer-reader coordination network. Their exclusion was structural, not conspiratorial: the network formed precisely outside their control.
% DISAPPEARANCE_RATIONALE: If the strategic deployment of the press had vanished overnight in 1517, the Reformation would have remained a localized academic dispute or a slow, epistolary regional movement. The speed, geographic scope, and political momentum of the Reform would have been drastically reduced; the printer market would not have reoriented toward vernacular religious controversy; and the Catholic information monopoly would have eroded far more slowly.
% FOUNDING_PROBLEM: How to disseminate dissenting theological ideas and coordinate a religious reform movement across a continent of independent principalities and cities with no shared political or ecclesiastical authority to authorize such dissent.
% FOUNDING_PROBLEM_CORROBORATION: The existence of an information bottleneck before the Reformation is corroborated by modern media historians and bibliographers working outside the reform tradition. Reformation leaders' own correspondence attests to the problem from the beneficiary seat. However, the claim that this specific market-coordination arrangement was the only viable solution is contested by historians who emphasize oral and manuscript networks.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.25, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.25) because the arrangement operated through market exchange and voluntary readership rather than coercion or rent extraction. Suppression is very low (0.15) because the rope persisted through participant benefit and demand, not through suppressing alternatives; manuscript culture and oral preaching remained available but were economically outcompeted for mass communication. Theater ratio is minimal (0.10) because the press performed a genuine, non-performative coordination functionâproducing and moving real texts to real readers. Accessibility collapse is moderate (0.35) because manuscript alternatives did not disappear entirely but became non-viable for large-scale rapid dissemination. Resistance is moderate (0.40) because external actors (Catholic authorities) actively resisted the reformers' message, though this resistance targeted content rather than the press mechanism itself.
 *
 * PERSPECTIVAL GAP:
 *   Reformation leaders, printers, and the reading public experience this constraint as a rope: a genuine coordination mechanism with net benefit to all participants. The excluded Catholic clergy would experience the same historical dynamic as a loss of institutional control, but they are not participants in the constraint itselfâthey do not pay into it, nor is their exclusion enforced by it. The engine will compute different directionalities for participants (low d, low effective extraction) versus antagonists outside the arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformation leaders, vernacular printers, and the literate public are all declared beneficiaries with mobile exit options, yielding low directionality toward the beneficiary end. No victims are declared because the constraint operates through coordination and market exchange rather than asymmetric extraction. Catholic clergy are assigned the excluded role and bear no directionality assignment because they are not agents in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâdisseminating dissent across politically fragmented Europeâwas solved by the press network within the historical interval. The arrangement persisted because it continued to deliver genuine coordination benefits: printers profited, reformers reached audiences, and readers gained access. There is no evidence that the coordination function atrophied into performance during the studied period; the press remained a functional, low-theater coordination tool. Mandatrophy is therefore not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_neutrality_contest,
    'Was the printing press a genuinely neutral technology awaiting purposeful use, or did its material affordances (standardization, speed, visual layout, vernacular type) inherently favor certain argumentative forms and genres over others?',
    'Comparative media history and bibliographic analysis examining whether the press structurally amplified specific genres (pamphlets, polemical woodcuts, vernacular Bibles) in ways that were independent of user intent.',
    'If the press was not neutral, the rope characterization weakens and the constraint shifts toward tangled rope or mutual_shaping; if neutral, the strategic_deployment reading is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_neutrality_contest, conceptual, 'Whether the press had inherent directional affordances or was a neutral tool').

omega_variable(
    printer_agency_independence,
    'Did printers exercise independent strategic agency in selecting and shaping Reformation texts, or were they purely instrumental to reformer agendas?',
    'Prosopographical study of printer networks and archival contracts examining whose editorial and commercial choices determined print output.',
    'High printer independence supports the mutual_shaping sibling reading; low independence supports strategic_deployment with reformers as primary agenda-setters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(printer_agency_independence, empirical, 'Degree of independent printer agency in the coordination network').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_ref_sd_tr_t0, press_reformation_causation__strategic_deployment, theater_ratio, 0, 0.05).
narrative_ontology:measurement(press_ref_sd_tr_t10, press_reformation_causation__strategic_deployment, theater_ratio, 10, 0.06).
narrative_ontology:measurement(press_ref_sd_tr_t20, press_reformation_causation__strategic_deployment, theater_ratio, 20, 0.08).
narrative_ontology:measurement(press_ref_sd_tr_t30, press_reformation_causation__strategic_deployment, theater_ratio, 30, 0.1).
narrative_ontology:measurement(press_ref_sd_tr_t40, press_reformation_causation__strategic_deployment, theater_ratio, 40, 0.12).

% Extraction over time
narrative_ontology:measurement(press_ref_sd_be_t0, press_reformation_causation__strategic_deployment, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(press_ref_sd_be_t10, press_reformation_causation__strategic_deployment, base_extractiveness, 10, 0.2).
narrative_ontology:measurement(press_ref_sd_be_t20, press_reformation_causation__strategic_deployment, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(press_ref_sd_be_t30, press_reformation_causation__strategic_deployment, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(press_ref_sd_be_t40, press_reformation_causation__strategic_deployment, base_extractiveness, 40, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__strategic_deployment, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
