% ============================================================================
% CONSTRAINT STORY: technology_reformation_causality__co_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_reformation_causality__co_constitution_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: technology_reformation_causality__co_constitution_reading
 *   human_readable: Printing Press and Reformation as Co-Constituted Coordination Mechanism
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the co-constitution reading of the contested
 *   kernel about printing-press causality in the Reformation: technology
 *   enabled but did not determine outcomes, and reform actors actively shaped
 *   what the press produced rather than merely deploying a neutral tool. The
 *   extractiveness trajectory is modest and rises slowly across 1450-1600 as
 *   the coordination function (cheap, scaled reproduction) gradually accrued
 *   an interpretive-authority-transfer component (reformist clergy and
 *   printers capturing narrative control at the expense of scribal guilds and
 *   the traditional hierarchy) — the ε here is read as arising from the
 *   INTERACTION TERM between technology and social agency, not from either
 *   factor alone, which is the structural delta this reading commits to
 *   relative to its siblings.
 *
 * KEY AGENTS:
 *   - reformist_clergy: primary co-shaping agent (organized/constrained) — shaped press output, bore dependency costs
 *   - printing_guild_operators: coordination-function beneficiary (organized/mobile) — profited from and jurisdiction-shopped the mechanism
 *   - vernacular_literate_laity: diffuse beneficiary (moderate/constrained) — fed demand back into production choices
 *   - traditionalist_clergy_networks: primary payer (institutional/constrained) — lost interpretive monopoly
 *   - manuscript_copyist_guilds: displaced payer (moderate/trapped) — non-transferable skill loss
 *   - print_technology_itself: analytical non-agent — the enabling artifact, deliberately not treated as sole cause
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_reformation_causality__co_constitution_reading, 0.28).
domain_priors:suppression_score(technology_reformation_causality__co_constitution_reading, 0.22).
domain_priors:theater_ratio(technology_reformation_causality__co_constitution_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(technology_reformation_causality__co_constitution_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_reformation_causality__co_constitution_reading, rope).
narrative_ontology:human_readable(technology_reformation_causality__co_constitution_reading, "Printing Press and Reformation as Co-Constituted Coordination Mechanism").
narrative_ontology:topic_domain(technology_reformation_causality__co_constitution_reading, "history_of_technology/religious_history/media_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_reformation_causality__co_constitution_reading, 'dd86b6d3-22cc-4063-b930-be676d44971c').
narrative_ontology:cs_kernel_codification('dd86b6d3-22cc-4063-b930-be676d44971c', distributed).
narrative_ontology:cs_authority_grounding('dd86b6d3-22cc-4063-b930-be676d44971c', distributed).
narrative_ontology:cs_reading_relation('dd86b6d3-22cc-4063-b930-be676d44971c', technology_reformation_causality__technological_determinism_reading, influences).
narrative_ontology:cs_reading_relation('dd86b6d3-22cc-4063-b930-be676d44971c', technology_reformation_causality__beneficiary_agency_reading, influences).
narrative_ontology:cs_axiom('dd86b6d3-22cc-4063-b930-be676d44971c', foundational, causality_is_interactive_not_decomposable).
narrative_ontology:cs_axiom_status(causality_is_interactive_not_decomposable, holdable).
narrative_ontology:cs_axiom_grounding('dd86b6d3-22cc-4063-b930-be676d44971c', causality_is_interactive_not_decomposable, empirically_contingent).
narrative_ontology:cs_axiom('dd86b6d3-22cc-4063-b930-be676d44971c', secondary, reformers_shaped_press_output_not_merely_used_it).
narrative_ontology:cs_axiom_status(reformers_shaped_press_output_not_merely_used_it, holdable).
narrative_ontology:cs_axiom_grounding('dd86b6d3-22cc-4063-b930-be676d44971c', reformers_shaped_press_output_not_merely_used_it, empirically_contingent).
narrative_ontology:cs_reference_frame('dd86b6d3-22cc-4063-b930-be676d44971c', confessional_print_equilibrium).
narrative_ontology:cs_drift_state('dd86b6d3-22cc-4063-b930-be676d44971c', post_tridentine_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dd86b6d3-22cc-4063-b930-be676d44971c', '').
narrative_ontology:cs_kernel_id(technology_reformation_causality__co_constitution_reading, technology_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, reformist_clergy).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, printing_guild_operators).
narrative_ontology:constraint_beneficiary(technology_reformation_causality__co_constitution_reading, vernacular_literate_laity).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, traditionalist_clergy_networks).
narrative_ontology:constraint_victim(technology_reformation_causality__co_constitution_reading, manuscript_copyist_guilds).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, co_evolutionary_causality_thesis).
narrative_ontology:constraint_vindicates(technology_reformation_causality__co_constitution_reading, technology_as_enabling_not_determining).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reform-minded clergy and theologians (Luther and successors) actively selected which texts to print, in which vernacular, at which rhetorical register, and negotiated with printers over format and distribution. They shaped what the press produced as much as the press shaped their reach; their exit from the print economy was constrained by dependence on printer capital and by the fact that pulpit-only preaching had become comparatively ineffective once the press had normalized pamphlet circulation.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, reformist_clergy, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, reformist_clergy, beneficiary).

% Printers and their guilds supplied the physical coordination mechanism — movable type, standardized runs, distribution networks across fairs and trade routes — and profited from religious pamphlet demand. They were not neutral conduits: printers chose which manuscripts to typeset, sequenced print runs for market demand, and could relocate operations across jurisdictional lines when one city's authorities cracked down, giving them real exit relative to clergy or laity.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, printing_guild_operators, beneficiary,
    organized, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(technology_reformation_causality__co_constitution_reading, printing_guild_operators, agenda_setter).

% Newly literate or semi-literate townspeople gained access to vernacular scripture and polemic at lower cost than commissioned manuscripts, and their reading practices and demand for particular genres (catechisms, woodcut broadsides) fed back into what printers chose to produce. Their exit options were regional — mobility to more permissive print markets existed but was costly for most.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, vernacular_literate_laity, beneficiary,
    moderate, generational, constrained, regional).

% The established ecclesiastical hierarchy lost interpretive monopoly as vernacular print circulated readings the hierarchy had not authorized and could not fully suppress once print runs multiplied faster than confiscation could keep pace. They bore the cost of eroded authority and fragmented congregations; their exit was constrained because abandoning the print contest entirely ceded the field to reformers, while engaging it meant legitimizing the very medium eroding their monopoly.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, traditionalist_clergy_networks, payer,
    institutional, generational, constrained, continental).

% Scribal copying workshops saw their economic function displaced as print reduced the marginal cost of text reproduction by orders of magnitude. Their skill was specific and non-transferable to the new print trades on comparable terms; exit meant downward occupational mobility, not lateral movement.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, manuscript_copyist_guilds, payer,
    moderate, biographical, trapped, regional).

% The press as an artifact: a coordination technology whose capacities (type-reuse, run scaling, format standardization) were real and enabling, but whose actual deployment, content, and pace were continuously shaped by the social actors using it. Listed here as a non-agent entity for narrative completeness only; it neither collects nor pays.
narrative_ontology:constraint_stakeholder(technology_reformation_causality__co_constitution_reading, print_technology_itself, observer,
    analytical, civilizational, analytical, continental).
narrative_ontology:stakeholder_non_agent(technology_reformation_causality__co_constitution_reading, print_technology_itself).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_reformation_causality__co_constitution_reading, diffuse).
narrative_ontology:fixing_cost_class(technology_reformation_causality__co_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Printing solved a genuine reproduction-and-distribution problem: it let a single authored text be replicated at scale and moved through trade networks faster and cheaper than scribal copying allowed, coordinating dispersed readers around shared texts without requiring each to access a scriptorium.
% TRANSFER_FUNCTION: Interpretive and economic authority moved from scribal and ecclesiastical monopolists toward printers, reform-minded clergy, and a broader literate laity; the copyist guilds and the traditional hierarchy's interpretive control were the sides from which this authority moved.
% ABSENT_VOICES: Illiterate rural populations, who remained outside both the manuscript and print economies, are largely absent from the historical record of this exchange despite bearing the downstream religious and political consequences of the conflicts print amplified.
% DISAPPEARANCE_RATIONALE: Historians dispute this directly: some argue reform movements without mass print would have remained localized heresies suppressed as previous ones were (world_rearranges if press vanished retroactively); others argue the social and theological pressures were independently mounting and would have found another vector — pulpit networks, existing manuscript circuits, court patronage (world_unchanged). The co-constitution reading holds that asking the counterfactual in isolation is itself the wrong frame: press and reform movement were not separable enough for either counterfactual to cleanly resolve.
% FOUNDING_PROBLEM: Neither press nor reform movement was 'built to solve' a single founding problem in the way an institution is founded — the press addressed a reproduction-cost problem (manuscript scarcity, cost, and copying error), while reform theology addressed a doctrinal-legitimacy problem (perceived corruption and Latin-literacy gatekeeping). This reading's claim is that the two problems became entangled and mutually reshaped their own definitions once print and reform intersected.
% FOUNDING_PROBLEM_CORROBORATION: Book historians (e.g. the Annales-school print culture tradition) attest the reproduction-cost problem was real and substantially resolved by print technology alone, independent of religious content. Reformation theologians and their institutional heirs attest the doctrinal-legitimacy problem remains partially live in ongoing debates over vernacular scripture access and clerical authority. Neither corroborating source is a direct beneficiary of the co-constitution reading itself, though both have stakes in the sibling readings this story deliberately does not adjudicate.
narrative_ontology:disappearance_verdict(technology_reformation_causality__co_constitution_reading, contested).
narrative_ontology:founding_problem_status(technology_reformation_causality__co_constitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_reformation_causality__co_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_reformation_causality__co_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_reformation_causality__co_constitution_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_reformation_causality__co_constitution_reading_tests).
:- end_tests(technology_reformation_causality__co_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low and rising (0.08 to 0.28) because this reading treats the transfer of interpretive and economic authority as a genuine but secondary product of coordination success, not as the primary function of either institution. Suppression is low-moderate (0.22) because print's own coordination logic (movable type, distributed print shops) made comprehensive suppression structurally difficult regardless of ecclesiastical intent — this is a feature of the technology's coordination properties, not of any single actor's choice, and per the framework's rule, suppression is NOT scaled by scope or power in the engine's computation; it is authored as a raw structural fact about how hard it was to close off the mechanism once running. Theater ratio rises to 0.42 by the end of the interval, reflecting how much of the later print-religious conflict (polemical broadsides, propaganda pamphlets) became performative contest for legitimacy rather than doctrinal substance — a genuine drift, not tuned to any target classification.
 *
 * PERSPECTIVAL GAP:
 *   From the reformist-clergy and printer seats, the arrangement reads as productive coordination they actively built and steered — genuine rope. From the traditionalist-clergy and copyist-guild seats, the same arrangement reads as an erosion mechanism they could not resist once print's distribution logic outran confiscation capacity, closer to piton (their prior interpretive monopoly atrophied without any single actor's directed extraction). This divergence is exactly what the engine should compute from the structural data, not something this reading resolves in advance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (reformist clergy, printers, literate laity) are declared with low-to-moderate d because the coordination function subsidizes their reach and authority; the reformist clergy sit closer to symmetric than pure beneficiary because they bore real dependency costs on printer capital. Victims (traditionalist clergy, copyist guilds) are declared with high d because the same mechanism that coordinated reformers' output eroded their prior monopoly positions asymmetrically — the copyist guilds especially, given trapped exit and non-transferable skills.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem fields are authored as contested and dual (a reproduction-cost problem AND a doctrinal-legitimacy problem) precisely to avoid mislabeling this bidirectional co-evolution as either pure technological necessity (which would erase reformer agency) or pure strategic tool-use (which would erase the press's independent coordination contribution). Neither the press's reproduction function nor the reform movement's theological grievance had fully resolved by 1600 — status is genuinely contested rather than resolved, which blocks a premature mandatrophy verdict in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interaction_term_decomposability,
    'Is the causal contribution of the printing press to the Reformation genuinely decomposable into a technology component and a social-agency component, or is the co-constitution reading correct that only their interaction term is meaningful?',
    'Comparative historical analysis of regions with print access but limited reform uptake (e.g. parts of Italy, Iberia) versus regions with strong reform movements and constrained print access (early Hussite Bohemia) would test whether either factor alone predicts outcomes, or whether only their conjunction does.',
    'If decomposable, this reading collapses toward either the determinism or beneficiary-agency reading depending on which component dominates; if genuinely non-decomposable, the co-constitution reading''s structural claim (ε as an interaction effect) is vindicated and the three readings remain genuinely distinct rather than one being a special case of another.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interaction_term_decomposability, conceptual, 'Whether technology and agency contributions to the Reformation are separable or only meaningful jointly.').

omega_variable(
    counterfactual_reform_without_print,
    'Would a Reformation-scale doctrinal challenge have emerged and sustained itself without mass print, through pulpit networks, existing manuscript circuits, or court patronage?',
    'Study of pre-print heretical movements (Waldensians, Lollards, Hussites) that achieved regional persistence without print, compared against their eventual suppression rates and geographic containment relative to the printed Reformation''s spread.',
    'If pre-print movements show comparable persistence, the press''s causal contribution is more modest than either this reading or the determinism reading assumes; if pre-print movements were reliably contained while print-era reform was not, it strengthens the case that print''s coordination function was load-bearing rather than merely amplifying.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_reform_without_print, empirical, 'Whether the Reformation''s scale and durability required print as a load-bearing coordination mechanism.').

omega_variable(
    committer_framing_choice,
    'Is the co-constitution reading the most defensible framing of this kernel, or does treating technology and reformer-agency as symmetric co-shapers understate cases where one clearly dominates (e.g. Gutenberg''s own indifference to religious content versus Luther''s active print-strategy choices)?',
    'Case-by-case attribution analysis: for each major reform-print event (95 Theses circulation, vernacular Bible print runs, pamphlet wars), assess whether printer-side or reformer-side agency was the proximate driver of a given text''s production and distribution decisions.',
    'If case analysis consistently shows one side dominating specific decisions, this reading''s symmetric bidirectional framing may need to be split further into event-level readings rather than one continental-scale co-constitution claim; if genuinely mixed and context-dependent, it supports keeping this as a single reading at this level of granularity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_choice, conceptual, 'Whether the symmetric co-constitution framing is the right grain, or whether it averages over cases better split into determinism-dominant and agency-dominant sub-cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_reformation_causality__co_constitution_reading, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t1450, technology_reformation_causality__co_constitution_reading, theater_ratio, 1450, 0.1).
narrative_ontology:measurement_basis(tech_tr_t1450, observed).
narrative_ontology:measurement(tech_tr_t1480, technology_reformation_causality__co_constitution_reading, theater_ratio, 1480, 0.15).
narrative_ontology:measurement_basis(tech_tr_t1480, observed).
narrative_ontology:measurement(tech_tr_t1510, technology_reformation_causality__co_constitution_reading, theater_ratio, 1510, 0.3).
narrative_ontology:measurement_basis(tech_tr_t1510, observed).
narrative_ontology:measurement(tech_tr_t1540, technology_reformation_causality__co_constitution_reading, theater_ratio, 1540, 0.4).
narrative_ontology:measurement_basis(tech_tr_t1540, observed).
narrative_ontology:measurement(tech_tr_t1570, technology_reformation_causality__co_constitution_reading, theater_ratio, 1570, 0.42).
narrative_ontology:measurement_basis(tech_tr_t1570, observed).
narrative_ontology:measurement(tech_tr_t1600, technology_reformation_causality__co_constitution_reading, theater_ratio, 1600, 0.42).
narrative_ontology:measurement_basis(tech_tr_t1600, observed).

% Extraction over time
narrative_ontology:measurement(tech_be_t1450, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement_basis(tech_be_t1450, observed).
narrative_ontology:measurement(tech_be_t1480, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1480, 0.12).
narrative_ontology:measurement_basis(tech_be_t1480, observed).
narrative_ontology:measurement(tech_be_t1510, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1510, 0.2).
narrative_ontology:measurement_basis(tech_be_t1510, observed).
narrative_ontology:measurement(tech_be_t1540, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1540, 0.26).
narrative_ontology:measurement_basis(tech_be_t1540, observed).
narrative_ontology:measurement(tech_be_t1570, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1570, 0.28).
narrative_ontology:measurement_basis(tech_be_t1570, observed).
narrative_ontology:measurement(tech_be_t1600, technology_reformation_causality__co_constitution_reading, base_extractiveness, 1600, 0.28).
narrative_ontology:measurement_basis(tech_be_t1600, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(technology_reformation_causality__co_constitution_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_reformation_causality__co_constitution_reading, information_standard).
narrative_ontology:boltzmann_floor_override(technology_reformation_causality__co_constitution_reading, 0.05).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, technological_determinism_reading).
narrative_ontology:affects_constraint(technology_reformation_causality__co_constitution_reading, beneficiary_agency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the technology_reformation_causality kernel. technological_determinism_reading attributes primary causal weight to the press as an independent variable (higher ε attributed to technology alone). beneficiary_agency_reading attributes primary causal weight to reformer/printer strategic choice (technology as neutral instrument, ε attributed to agent deployment). This co_constitution_reading denies either attribution is complete, authoring ε as arising from the interaction term between technology and social agency. All three share the same underlying historical episode but are structurally distinct claims per the ε-invariance principle, each with its own beneficiary/victim structure and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
