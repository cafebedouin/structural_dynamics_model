% ============================================================================
% CONSTRAINT STORY: reformation_composite__technological_mediation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__technological_mediation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: reformation_composite__technological_mediation_reading
 *   human_readable: Technological Mediation of the Reformation via the Printing Press
 *   domain: historical/epistemological/religious
 *
 * SUMMARY:
 *   This constraint instantiates the technological-mediation reading of the
 *   reformation_composite kernel: the claim that the Reformation is
 *   fundamentally a technological event in which the printing press operates
 *   as a fixed physical affordance transforming local theological dissent
 *   into a continental mass movement. The press is treated here as a
 *   mountain-like constraintâmovable type, paper, and press mechanics
 *   constitute an irreducible physical limit on the speed and scale of
 *   communication in the sixteenth century. Once invented and diffused, the
 *   technology does not depend on any party defending it to continue enabling
 *   mass textual circulation. However, identifiable beneficiaries (reformers,
 *   printers, literate merchants) and payers (the Catholic information
 *   monopoly, manuscript copyists) exist, triggering false-summit evaluation.
 *   The reading does not foreclose political or theological readings but
 *   treats them as downstream effects enabled by the physical possibility of
 *   rapid reproduction.
 *
 * KEY AGENTS:
 *   - protestant_reformers: Primary beneficiaries (organized/constrained) â gain mass audience through the affordances of mechanical reproduction
 *   - urban_printing_consortia: Agenda-setters with secondary beneficiary status (moderate/mobile) â control the physical capital of reproduction and collect revenue from output
 *   - literate_merchant_class: Secondary beneficiaries (moderate/mobile) â gain access to vernacular theological debate previously restricted to Latin-reading clergy
 *   - catholic_doctrinal_monopoly: Primary payers (institutional/constrained) â lose practical control over text reproduction and doctrinal gatekeeping
 *   - manuscript_copyist_guilds: Secondary payers (moderate/trapped) â economically displaced as mechanical reproduction undercuts hand-copying
 *   - illiterate_rural_peasantry: Excluded voices (powerless/trapped) â structurally absent from the print-mediated public sphere
 *   - historians_of_technology: Analytical observers (analytical/analytical) â evaluate causal claims from a post-hoc epistemic position
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__technological_mediation_reading, 0.15).
domain_priors:suppression_score(reformation_composite__technological_mediation_reading, 0.1).
domain_priors:theater_ratio(reformation_composite__technological_mediation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__technological_mediation_reading, mountain).
narrative_ontology:human_readable(reformation_composite__technological_mediation_reading, "Technological Mediation of the Reformation via the Printing Press").
narrative_ontology:topic_domain(reformation_composite__technological_mediation_reading, "historical/epistemological/religious").

domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__technological_mediation_reading, '7f803457-4688-4842-abaf-7de8869e0c71').
narrative_ontology:cs_kernel_codification('7f803457-4688-4842-abaf-7de8869e0c71', distributed).
narrative_ontology:cs_authority_grounding('7f803457-4688-4842-abaf-7de8869e0c71', expertise).
narrative_ontology:cs_reading_relation('7f803457-4688-4842-abaf-7de8869e0c71', reformation_composite__political_realignment_reading, influences).
narrative_ontology:cs_reading_relation('7f803457-4688-4842-abaf-7de8869e0c71', reformation_composite__theological_fragmentation_reading, coexists_with).
narrative_ontology:cs_axiom('7f803457-4688-4842-abaf-7de8869e0c71', foundational, technological_primacy_in_reformation_causality).
narrative_ontology:cs_axiom_status(technological_primacy_in_reformation_causality, holdable).
narrative_ontology:cs_axiom_grounding('7f803457-4688-4842-abaf-7de8869e0c71', technological_primacy_in_reformation_causality, empirically_contingent).
narrative_ontology:cs_axiom('7f803457-4688-4842-abaf-7de8869e0c71', secondary, print_circulation_as_sufficient_explanation_of_geographic_diffusion).
narrative_ontology:cs_axiom_status(print_circulation_as_sufficient_explanation_of_geographic_diffusion, holdable).
narrative_ontology:cs_axiom_grounding('7f803457-4688-4842-abaf-7de8869e0c71', print_circulation_as_sufficient_explanation_of_geographic_diffusion, empirically_contingent).
narrative_ontology:cs_reference_frame('7f803457-4688-4842-abaf-7de8869e0c71', mechanical_reproduction_as_historical_determinant).
narrative_ontology:cs_drift_state('7f803457-4688-4842-abaf-7de8869e0c71', contemporary_historiography, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7f803457-4688-4842-abaf-7de8869e0c71', '').
narrative_ontology:cs_kernel_id(reformation_composite__technological_mediation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, protestant_reformers).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, urban_printing_consortia).
narrative_ontology:constraint_beneficiary(reformation_composite__technological_mediation_reading, literate_merchant_class).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, catholic_doctrinal_monopoly).
narrative_ontology:constraint_victim(reformation_composite__technological_mediation_reading, manuscript_copyist_guilds).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, technological_determinism_thesis).
narrative_ontology:constraint_vindicates(reformation_composite__technological_mediation_reading, mass_literacy_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Theologians and pamphleteers whose critiques of the Catholic Church were reproduced in vernacular print runs reaching tens of thousands. Their theological arguments acquired audience scale impossible through sermon or manuscript alone. Once the reformers invested in print as their primary medium, exiting to local oral or manuscript transmission would have collapsed their audience and influence.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, protestant_reformers, beneficiary,
    organized, biographical, constrained, continental).

% Artisans and entrepreneurs operating presses in Wittenberg, Basel, Strasbourg, and other printing centers. They decided which tracts to set in type, bore the capital cost of presses and type, and collected revenue from sales to local and itinerant booksellers. Their physical infrastructure was the literal site where theological dissent became reproducible object.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, urban_printing_consortia, agenda_setter,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__technological_mediation_reading, urban_printing_consortia, beneficiary).

% Urban commercial classes with literacy in vernacular languages and disposable income for pamphlets and broadsheets. They gained access to theological debate previously restricted to Latin-reading clergy, and formed the readership that made large print runs economically viable.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, literate_merchant_class, beneficiary,
    moderate, biographical, mobile, continental).

% The Roman Curia, diocesan structures, and university theological faculties that had controlled the reproduction and interpretation of sacred texts through Latin manuscript culture and ecclesiastical authorization. They lost the practical ability to control theological discourse as vernacular print circulated outside the monastic and university scriptoria they dominated.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, catholic_doctrinal_monopoly, payer,
    institutional, generational, constrained, global).

% Scribes and illuminators organized in urban guilds whose livelihood depended on hand-copying religious and legal texts. Their labor-intensive product was undercut in price and surpassed in speed by press output, and their guild privileges eroded as print shops multiplied outside traditional craft regulation.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, manuscript_copyist_guilds, payer,
    moderate, biographical, trapped, regional).

% The majority of the European population who could not read vernacular texts and had no access to urban book markets. They were structurally absent from the print-mediated public sphere; theological ideas reached them only through secondary oral transmission, sermon, or coercion, not through the direct textual engagement that characterized the Reformation's elite discourse.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, illiterate_rural_peasantry, excluded,
    powerless, generational, trapped, local).

% Modern scholars who analyze publication counts, type provenance, and literacy rates to assess the causal weight of printing technology in the Reformation. They sit outside the historical constraint's operation and evaluate competing causal claims from an epistemic position that does not depend on the Reformation's outcome.
narrative_ontology:constraint_stakeholder(reformation_composite__technological_mediation_reading, historians_of_technology, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rapid, standardized, wide-area dissemination of theological arguments, solving the practical problem of synchronizing dissent and building a translocal community of belief without centralized travel or oral messenger networks.
% TRANSFER_FUNCTION: Moves doctrinal influence and economic revenue from ecclesiastical manuscript centers and the Catholic information monopoly toward urban printing houses, literate mercantile networks, and vernacular theological entrepreneurs who control the new means of reproduction.
% ABSENT_VOICES: Illiterate rural majorities are structurally excluded from the print-mediated public sphere; their theological preferences and interpretations are not represented in the mass-printed discourse that defines the Reformation's written record. Women in conventual manuscript cultures and non-Latin speakers without vernacular print access are likewise absent from the conversation.
% DISAPPEARANCE_RATIONALE: If the printing press and its output vanished overnight in 1517-1555, theological dissent would revert to local, slow, controllable manuscript and oral circulation. The continental mass movement would dissolve into isolated pockets because the speed, scale, and standardization of coordination depend entirely on mechanical reproduction. The political and theological downstream effects described by sibling readings would not achieve the critical mass necessary to become mass phenomena.
% FOUNDING_PROBLEM: How to coordinate theological dissent across hundreds of principalities, language boundaries, and geographic distances before centralized road or telecommunication networks exist, when the Catholic Church maintains a practical monopoly on text reproduction and interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the book trade and historians of technology attest to the coordination problem from outside the beneficiary seats. Catholic polemicists of the era corroborate the problem from the opposing side by identifying the press as the engine of their loss of control, confirming that the constraint solved a problem of geographic scale that manuscript culture could not address.
narrative_ontology:disappearance_verdict(reformation_composite__technological_mediation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__technological_mediation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__technological_mediation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_composite__technological_mediation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__technological_mediation_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__technological_mediation_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_composite__technological_mediation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_composite__technological_mediation_reading),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_composite__technological_mediation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_composite__technological_mediation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the printing press itself is a physical technology that does not inherently extract surplus; it is a tool whose mechanical properties are fixed. Suppression is low (0.10) because the constraint does not operate through coercion but through physical possibilityâalternatives such as manuscript culture do not need to be actively suppressed to collapse, they simply become non-competitive for mass communication. Accessibility collapse is high (0.88) because once mechanical reproduction exists at scale, the alternative of local, hand-copied transmission collapses almost completely for large-audience communication. Resistance is near-zero (0.08) because the physics of movable type cannot be resisted through institutional action; the Church attempted censorship but could not inhibit the underlying technology. Theater ratio is minimal (0.05) because there is little performative maintenanceâpresses either work or they do not. The flat measurement series confirm the mountain-like stability of the constraint over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats diverge sharply because the same physical affordance is liberation from one position and displacement from another. Reformers experience the press as the removal of a prior constraint (the Catholic information monopoly), while the Catholic hierarchy experiences it as the sudden impossibility of maintaining its previous gatekeeping function. Copyists experience technological unemployment. The engine computes this divergence from structural position: agenda-setters and beneficiaries sit near the low-directionality end because the technology subsidizes their reach, while payers sit near the high-directionality end because the same technology extracts their previous institutional advantages.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to agents whose structural position is improved by the existence of mass textual reproduction: protestant_reformers gain audience scale, urban_printing_consortia gain revenue and productive function, and literate_merchants gain access to elite discourse. Victim declarations map to agents whose position depends on the scarcity that printing eliminates: the catholic_doctrinal_monopoly loses its reproduction monopoly, and manuscript_copyist_guilds lose their craft viability. The excluded seat (illiterate_rural_peasantry) is not a victim of extraction by the press itself but is structurally bypassed by the new medium, receiving neither benefit nor cost directly from the technology.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this constraint as pure extraction (snare) because the press has a genuine coordination function that requires no cover story: it objectively solves the problem of transmitting complex theological arguments across hundreds of miles and language boundaries faster than oral or manuscript culture permits. It is not a scaffold because it carries no sunset clause and was not designed as transitional. It is not a piton because its function has not atrophied; the physical mechanism continues to work. Mountain is claimed because the constraint's persistence depends on mechanical physics, not on human enforcement. The presence of beneficiaries triggers the false-summit mechanism so the engine can test whether this mountain claim is merely a naturalization of constructed advantage.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_determinism_vs_social_construction,
    'Is the printing press a physical constraint operating independently of social arrangements, or does its causal power depend on specific social, economic, and political configurations?',
    'Comparative historiography showing whether the press produces similar effects in all social contexts or only in the specific configuration of 16th-century Europe.',
    'If purely physical and technological, the mountain classification holds; if socially contingent, the constraint is better read as a tangled rope or scaffold whose extraction is historically specific.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_determinism_vs_social_construction, conceptual, 'Natural-law vs constructed ambiguity for print technology').

omega_variable(
    extractiveness_of_mass_dissemination,
    'Does the printing press extract from excluded parties as an intrinsic feature of mechanical reproduction, or is the displacement of Catholic and manuscript institutions a side effect of separate political choices?',
    'Analyze whether the press inherently decentralizes authority or whether political agents channeled the technology toward extraction.',
    'If intrinsic, the mountain contains embedded extraction and the FSM override is appropriate; if side-effect, extraction belongs to downstream political constraints rather than the technological constraint itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractiveness_of_mass_dissemination, conceptual, 'Whether extraction is intrinsic to the press or a side effect').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__technological_mediation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reform_tech_tr_t0, reformation_composite__technological_mediation_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(reform_tech_tr_t8, reformation_composite__technological_mediation_reading, theater_ratio, 8, 0.04).
narrative_ontology:measurement(reform_tech_tr_t16, reformation_composite__technological_mediation_reading, theater_ratio, 16, 0.05).
narrative_ontology:measurement(reform_tech_tr_t24, reformation_composite__technological_mediation_reading, theater_ratio, 24, 0.05).
narrative_ontology:measurement(reform_tech_tr_t32, reformation_composite__technological_mediation_reading, theater_ratio, 32, 0.06).
narrative_ontology:measurement(reform_tech_tr_t40, reformation_composite__technological_mediation_reading, theater_ratio, 40, 0.05).

% Extraction over time
narrative_ontology:measurement(reform_tech_be_t0, reformation_composite__technological_mediation_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(reform_tech_be_t8, reformation_composite__technological_mediation_reading, base_extractiveness, 8, 0.11).
narrative_ontology:measurement(reform_tech_be_t16, reformation_composite__technological_mediation_reading, base_extractiveness, 16, 0.12).
narrative_ontology:measurement(reform_tech_be_t24, reformation_composite__technological_mediation_reading, base_extractiveness, 24, 0.12).
narrative_ontology:measurement(reform_tech_be_t32, reformation_composite__technological_mediation_reading, base_extractiveness, 32, 0.13).
narrative_ontology:measurement(reform_tech_be_t40, reformation_composite__technological_mediation_reading, base_extractiveness, 40, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(reformation_composite__technological_mediation_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__technological_mediation_reading, reformation_composite__theological_fragmentation_reading).

% DUAL FORMULATION NOTE:
% The reformation_composite kernel decomposes into three structurally distinct claims about the Reformation's primary driver. This reading treats the printing press as a physical constraint (mountain) that enables the political and theological dynamics described by the sibling readings. The epsilon values differ: this constraint claims low extraction because it models physical technology; the sibling readings model social and political dynamics with higher extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
