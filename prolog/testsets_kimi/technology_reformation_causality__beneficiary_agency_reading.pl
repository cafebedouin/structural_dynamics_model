% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__beneficiary_agency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__beneficiary_agency_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: technology_reformation_causality__beneficiary_agency_reading
 *   human_readable: Reformer-Printer Coalition Print Bypass of Church Authority
 *   domain: history/religious/media
 *
 * SUMMARY:
 *   In the beneficiary-agency reading of the technology-Reformation kernel,
 *   the printing press is not an autonomous cause but a strategic tool
 *   deployed by a reformer-printer coalition to bypass the Catholic Church's
 *   monopoly on religious information. The coalition coordinates mass
 *   production and distribution of vernacular theological texts, extracting
 *   authority and revenue from an institution whose power rested partly on
 *   controlling sacred knowledge. The constraint is the structural
 *   arrangement of this bypass: a mutually beneficial coordination between
 *   content-producing reformers and profit-seeking printers that
 *   asymmetrically extracts from Church authority. This reading forecloses
 *   technological determinism (the press as inevitable cause) and influences
 *   co-constitution readings by centering human strategic choice.
 *
 * KEY AGENTS:
 *   - Reformers (agenda_setter, powerful/constrained): Bear low directionality as beneficiaries of authority bypass, though constrained by theological-political alliances.
 *   - Printers (agenda_setter/beneficiary, moderate/mobile): Bear very low directionality due to mobile exit options and direct revenue capture.
 *   - Church authority (payer, institutional/constrained): Bears high directionality as the primary victim of extracted authority and revenue.
 *   - Lay readership (beneficiary, powerless/constrained): Sits near symmetric, gaining information access while bearing diffuse costs of confessional polarization.
 *   - Rural illiterate (excluded, powerless/trapped): Structurally excluded from the print public, carrying no directional weight in the coalition's operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, 0.68).
domain_priors:suppression_score(technology_reformation_causality__beneficiary_agency_reading, 0.72).
domain_priors:theater_ratio(technology_reformation_causality__beneficiary_agency_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(technology_reformation_causality__beneficiary_agency_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__beneficiary_agency_reading, tangled_rope).
narrative_ontology:human_readable(technology_reformation_causality__beneficiary_agency_reading, "Reformer-Printer Coalition Print Bypass of Church Authority").
narrative_ontology:topic_domain(technology_reformation_causality__beneficiary_agency_reading, "history/religious/media").

domain_priors:requires_active_enforcement(technology_reformation_causality__beneficiary_agency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__beneficiary_agency_reading, 'da63ce8f-34b8-4cdf-a303-0bdc261492ae').
narrative_ontology:cs_kernel_codification('da63ce8f-34b8-4cdf-a303-0bdc261492ae', distributed).
narrative_ontology:cs_authority_grounding('da63ce8f-34b8-4cdf-a303-0bdc261492ae', practice).
narrative_ontology:cs_interpretation_layer_present('da63ce8f-34b8-4cdf-a303-0bdc261492ae').
narrative_ontology:cs_reading_relation('da63ce8f-34b8-4cdf-a303-0bdc261492ae', technology_reformation_causality__technological_determinism_reading, forecloses).
narrative_ontology:cs_reading_relation('da63ce8f-34b8-4cdf-a303-0bdc261492ae', technology_reformation_causality__co_constitution_reading, influences).
narrative_ontology:cs_axiom('da63ce8f-34b8-4cdf-a303-0bdc261492ae', foundational, technology_lacks_independent_causality).
narrative_ontology:cs_axiom_status(technology_lacks_independent_causality, holdable).
narrative_ontology:cs_axiom_grounding('da63ce8f-34b8-4cdf-a303-0bdc261492ae', technology_lacks_independent_causality, empirically_contingent).
narrative_ontology:cs_axiom('da63ce8f-34b8-4cdf-a303-0bdc261492ae', foundational, human_agency_primacy_in_media_deployment).
narrative_ontology:cs_axiom_status(human_agency_primacy_in_media_deployment, holdable).
narrative_ontology:cs_axiom_grounding('da63ce8f-34b8-4cdf-a303-0bdc261492ae', human_agency_primacy_in_media_deployment, empirically_contingent).
narrative_ontology:cs_reference_frame('da63ce8f-34b8-4cdf-a303-0bdc261492ae', strategic_agency_reference_frame).
narrative_ontology:cs_drift_state('da63ce8f-34b8-4cdf-a303-0bdc261492ae', post_materialist_turn, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('da63ce8f-34b8-4cdf-a303-0bdc261492ae', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__beneficiary_agency_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, reformers).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, printers).
narrative_ontology:constraint_victim(technology_reformation_causality__beneficiary_agency_reading, church_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__beneficiary_agency_reading, lay_readership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious reformers who strategically commissioned, authored, and planned the dissemination of vernacular theological texts to bypass ecclesiastical control over doctrine. They set the agenda for what to print, arranged financing through sympathetic patrons, and organized distribution networks across territorial boundaries. Their authority and follower base grew as Church information monopoly eroded.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, reformers, agenda_setter,
    powerful, generational, constrained, continental).

% Master printers and press operators who physically produced reformist tracts and Bibles, often relocating to cities with sympathetic political authorities. They administered production schedules, managed type and paper supply chains, and sold texts through book fairs and covert networks. Profited from high-demand controversial material while bearing legal and physical risk.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, printers, agenda_setter,
    moderate, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__beneficiary_agency_reading, printers, beneficiary).

% The ecclesiastical hierarchy whose monopoly on doctrinal interpretation, authorized text production, and revenue from sacred commerce was bypassed. Bears the cost of diminished doctrinal control, reduced indulgence and authorized-text revenue, and territorial losses as reformist print undermines its legitimacy.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, church_authority, payer,
    institutional, civilizational, constrained, continental).

% Literate urban and bourgeois laypeople who purchased and read vernacular printed texts, gaining direct access to theological arguments previously filtered by clerical gatekeepers. Their information horizon expanded, though their choices were bounded by what the reformer-printer coalition produced and what local authorities permitted.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, lay_readership, beneficiary,
    powerless, biographical, constrained, regional).

% The rural peasantry and illiterate majority structurally excluded from the literate print public. Unable to read Latin or vernacular printed texts, they depended on oral transmission and local priestly mediation. They had no seat in the strategic decisions about what to print or whose theology the press would carry.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__beneficiary_agency_reading, rural_illiterate, excluded,
    powerless, generational, trapped, regional).

narrative_ontology:fixing_cost_class(technology_reformation_causality__beneficiary_agency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production, financing, and trans-European distribution of reformist theological texts between content-producing reformers and commercial printers, solving the collective-action problem of mass religious information dissemination outside authorized Church channels.
% TRANSFER_FUNCTION: Moves doctrinal authority and economic value from the Church's monopoly on sacred texts and authorized interpretation to the reformer-printer coalition, while transferring vernacular theological access to literate lay publics.
% ABSENT_VOICES: The illiterate rural majority, women outside elite or convent contexts, and Catholic communities without vernacular print infrastructure are excluded; they would object that the bypass claims universal spiritual emancipation while speaking only for literate urban factions and reformist theological interests.
% DISAPPEARANCE_RATIONALE: If the reformer-printer coalition and its clandestine distribution networks vanished overnight, the Church's information monopoly would reassert in the medium term, theological dissent would retreat to localized manuscript and oral channels, printer economies in reformist cities would contract sharply, and the confessional geography of Europe would not have formed as it did.
% FOUNDING_PROBLEM: The Catholic Church maintained a bottleneck on religious information through Latin liturgy, scriptoria control, authorized text distribution, and doctrinal gatekeeping, preventing rapid vernacular theological debate and unfiltered lay access to scripture.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Catholic historiography acknowledges the pre-Reformation information monopoly while contesting the legitimacy of the bypass. Modern media historians outside the benefiting parties (e.g., Eisenstein, Febvre and Martin) corroborate the bottleneck's existence, though they dispute the causal weight of human agency versus technology.
narrative_ontology:disappearance_verdict(technology_reformation_causality__beneficiary_agency_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_reformation_causality__beneficiary_agency_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__beneficiary_agency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(technology_reformation_causality__beneficiary_agency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__beneficiary_agency_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_reformation_causality__beneficiary_agency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_reformation_causality__beneficiary_agency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the coalition extracted immense authority-value from the Church's information monopoly. Suppression (0.72) reflects the active suppression of Church alternatives as print networks made manuscript ecclesiastical control obsolete and as the coalition actively evaded the Index and territorial censorship. Theater ratio (0.28) remains moderate-low: the coordination was predominantly functional, though polemical printing carried performative dimensions. Accessibility collapse (0.78) is high because once the print bypass scaled, return to Church manuscript control became nearly impossible for contested theological genres. Resistance (0.85) is very high due to the Church's vigorous countermeasures.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer seat, the constraint appears as necessary coordination to break a corrupt monopoly and spread theological reform. From the printer seat, it appears as a commercially viable production and distribution system with manageable political risk. From the Church seat, it appears as systematic extraction of doctrinal authority and revenue. The engine computes divergent seat classifications from the same structural data: agenda-setter/beneficiary roles for reformers and printers yield coordination-weighted types, while the victim role for the Church yields extraction-weighted type.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are declared beneficiaries. Printers have mobile exit options (relocating to sympathetic jurisdictions), pushing their derived directionality toward the full-beneficiary end. Reformers have constrained exit (bound to theological-political alliances), yielding a slightly higher but still beneficiary-directional value. The Church is the declared victim with constrained exit options, placing it near the full-target end. Lay readership receives coordination benefit without being a declared beneficiary, sitting near neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification prevents mislabeling the coalition as either pure coordination (rope) or pure extraction (snare). The coalition solved a genuine coordination problemâmatching reformist content with mass production and trans-European distributionâthat a rope classification would capture but a snare classification would miss. Conversely, the same structure asymmetrically extracted authority and revenue from the Church, which a rope classification would miss. The active enforcement requirement (clandestine printing, smuggling networks, evasion of the Index, protection-seeking migrations) is the structural marker distinguishing tangled rope from rope: without active maintenance against Church countermeasures, the bypass would have collapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technology_independent_causality,
    'Does the printing press possess independent causal force in driving the Reformation, or is it exclusively an instrument of reformer and printer agency?',
    'Comparative analysis of pre-print reform movements (Lollardy, Hussitism) versus print-era reform; archival study of printer-reformer contracts and strategic decisions.',
    'If technology has independent causality, the constraint''s epsilon derives from technical affordance and the coalition is not the primary locus of extraction; if agency dominates, the current tangled-rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_independent_causality, conceptual, 'Ambiguity over technology''s independent causal status in Reformation historiography.').

omega_variable(
    mutual_extraction_asymmetry,
    'Was the reformer-printer coalition genuinely coordinative for both parties, or did one party extract disproportionately from the other within the coalition?',
    'Economic analysis of printer profit margins on reformist versus Catholic texts; network analysis of who controlled distribution nodes and who bore legal risk.',
    'Asymmetric extraction within the coalition would shift the classification toward snare for the exploited party; mutual benefit maintains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_extraction_asymmetry, empirical, 'Internal coalition extraction asymmetry.').

omega_variable(
    historiographical_framing_underdetermination,
    'Does the surviving evidentiary base sufficiently determine a choice between beneficiary-agency, technological determinism, and co-constitution framings of the same events?',
    'Meta-analysis of the evidentiary base across all three readings; assessment of whether remaining ambiguity is empirical or conceptual.',
    'If underdetermined, the kernel remains a site of irreducible framing contestation and the sibling readings must stand as separate constraints; if determined, one reading should absorb the others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historiographical_framing_underdetermination, conceptual, 'Evidentiary underdetermination of the kernel''s readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__beneficiary_agency_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trcb_tr_t0, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(trcb_tr_t12, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 12, 0.18).
narrative_ontology:measurement(trcb_tr_t24, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(trcb_tr_t36, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 36, 0.26).
narrative_ontology:measurement(trcb_tr_t48, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 48, 0.3).
narrative_ontology:measurement(trcb_tr_t60, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement(trcb_tr_t72, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 72, 0.36).
narrative_ontology:measurement(trcb_tr_t80, technology_reformation_causality__beneficiary_agency_reading, theater_ratio, 80, 0.38).

% Extraction over time
narrative_ontology:measurement(trcb_be_t0, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(trcb_be_t12, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(trcb_be_t24, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(trcb_be_t36, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 36, 0.72).
narrative_ontology:measurement(trcb_be_t48, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 48, 0.74).
narrative_ontology:measurement(trcb_be_t60, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(trcb_be_t72, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 72, 0.66).
narrative_ontology:measurement(trcb_be_t80, technology_reformation_causality__beneficiary_agency_reading, base_extractiveness, 80, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(trcb_su_t0, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(trcb_su_t12, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(trcb_su_t24, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(trcb_su_t36, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 36, 0.8).
narrative_ontology:measurement(trcb_su_t48, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 48, 0.82).
narrative_ontology:measurement(trcb_su_t60, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(trcb_su_t72, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 72, 0.74).
narrative_ontology:measurement(trcb_su_t80, technology_reformation_causality__beneficiary_agency_reading, suppression_requirement, 80, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__beneficiary_agency_reading, resource_allocation).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__beneficiary_agency_reading, co_constitution_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the technology_reformation_causality kernel, decomposed per the epsilon-invariance principle because the natural-language label 'printing press caused the Reformation' conflates structurally distinct claims: technological determinism (technology as autonomous cause), beneficiary agency (technology as tool of strategic actors), and co-constitution (mutual shaping). Each reading carries a distinct epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
