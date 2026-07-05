% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press as Deterministic Cause of the Reformation
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the technological-determinism reading of the
 *   press-Reformation kernel: the claim that Gutenberg's movable type made
 *   censorship mechanically infeasible and vernacular scripture diffusion
 *   inevitable, independent of any reformer's strategy or the Church's
 *   countermeasures. Under this reading the press functions as an exogenous,
 *   upstream mountain — a fact of physical/economic reproduction cost that no
 *   institutional actor chose and none could reverse. Reformers, printers,
 *   and newly literate laypeople are downstream beneficiaries of a capacity
 *   they did not construct through agency; the Church's suppression campaigns
 *   are futile against a structural fact rather than an out-maneuvered
 *   opponent. This is one of three readings of the same kernel
 *   (press_reformation_causation); the sibling readings —
 *   strategic_deployment (technology as neutral tool reformers exploited) and
 *   mutual_shaping (technology and agency co-evolving) — are separate
 *   constraint stories with their own ε and stakeholder structures. The
 *   determinist reading is not more or less 'true' by construction; it is the
 *   reading whose kernel_id/reading_id pair this file instantiates.
 *
 * KEY AGENTS:
 *   - protestant_reformers: downstream beneficiaries of exogenous print capacity (organized/arbitrage)
 *   - printer_publishers: mechanical replicators whose commercial choices are epiphenomenal to the underlying cost curve (moderate/mobile)
 *   - vernacular_literate_laity: recipients of scripture access as a structural byproduct (powerless/constrained)
 *   - catholic_church_hierarchy: institutional resistor whose suppression efforts arrive structurally too late (institutional/trapped)
 *   - manuscript_scribal_workshops: displaced incumbents with no voice in the determinist account (powerless/trapped)
 *   - media_historians: analytical observers adjudicating the kernel contest (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.15).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.1).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press as Deterministic Cause of the Reformation").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8').
narrative_ontology:cs_kernel_codification('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', distributed).
narrative_ontology:cs_authority_grounding('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', distributed).
narrative_ontology:cs_reading_relation('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', press_reformation_causation__mutual_shaping, influences).
narrative_ontology:cs_axiom('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', foundational, technology_possesses_autonomous_causal_force).
narrative_ontology:cs_axiom_status(technology_possesses_autonomous_causal_force, holdable).
narrative_ontology:cs_axiom_grounding('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', technology_possesses_autonomous_causal_force, empirically_contingent).
narrative_ontology:cs_axiom('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', secondary, institutional_resistance_to_diffusion_is_structurally_futile).
narrative_ontology:cs_axiom_status(institutional_resistance_to_diffusion_is_structurally_futile, holdable).
narrative_ontology:cs_axiom_grounding('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', institutional_resistance_to_diffusion_is_structurally_futile, empirically_contingent).
narrative_ontology:cs_reference_frame('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', print_technology_as_exogenous_capacity_shock).
narrative_ontology:cs_drift_state('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', post_media_ecology_critique_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f7e73bb8-5c8e-4b0a-95e9-45c12f0d78b8', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, printer_publishers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, vernacular_literate_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, catholic_church_hierarchy).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, technological_autonomy_thesis).
narrative_ontology:constraint_vindicates(press_reformation_causation__technological_determinism, media_ecology_determinism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, reformers did not create the conditions for mass vernacular scripture and pamphlet circulation — they arrived downstream of a print capacity that had already made centralized censorship structurally impossible. They ride the exogenous capability; their theological program succeeds because the technology has already removed the Church's ability to control text replication, not because of any strategic genius in deploying it.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    organized, generational, arbitrage, continental).

% Operate presses that mechanically multiply the copies any single authority could suppress. Under the determinist reading their commercial choices are epiphenomenal to the underlying mathematics of replication — once movable type exists, the cost of producing N copies falls in a way no single seizure or ban can reverse. They profit, but the profit is a byproduct of a capacity they did not have to strategize into being.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printer_publishers, beneficiary,
    moderate, biographical, mobile, regional).

% Gain access to scripture and theological argument in their own languages as a structural consequence of falling reproduction costs, not because any institution granted it to them. Their new access is treated, in this reading, as an inevitable output of the technology's diffusion curve rather than a hard-won political or pedagogical achievement.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, vernacular_literate_laity, beneficiary,
    powerless, generational, constrained, regional).

% Attempts indices of forbidden books, licensing regimes, and local seizures, but under this reading every enforcement action arrives structurally too late — the replication mathematics of movable type means any single suppressed edition is already outrun by copies made elsewhere. The hierarchy's resistance is real but, in this framing, futile against an exogenous technical fact rather than a contest it could plausibly have won with different tactics.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, catholic_church_hierarchy, payer,
    institutional, civilizational, trapped, continental).

% The prior hand-copying economy is displaced without being consulted or compensated. Their labor and its associated verification/quality-control function (scriptoria supervision, error-catching) simply cease to matter once mechanical replication undercuts their cost structure; the determinist reading has no seat for their loss because it treats the shift as a fact of nature rather than a policy outcome anyone chose.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, manuscript_scribal_workshops, excluded,
    powerless, biographical, trapped, local).

% Debate whether the press's causal role is autonomous (this reading) or mediated by human strategic choice (the sibling readings). Their scholarship is the arena in which the kernel contest over 'what caused the Reformation' is adjudicated, and their citation practices determine which reading dominates textbook accounts.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, media_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None in the ordinary sense — this is not a coordination mechanism but a claim about technological causation. Insofar as a coordination function exists, it is the press's mechanical standardization of textual reproduction, which lets distant readers coordinate around an identical text without any negotiated agreement.
% TRANSFER_FUNCTION: Under this reading, nothing is 'transferred' in an extractive sense; capability moves from no-one-in-particular to everyone with press access, as an emergent property of the replication technology rather than a redistribution enacted by an agent.
% ABSENT_VOICES: Scribal workshops and the manuscript economy they depended on have no voice in the determinist account — their displacement is narrated as technological inevitability rather than as a choice with losers, and the strategic-deployment and mutual-shaping readings are the only places their agency (or the reformers'/Church's agency toward them) could reenter the story.
% DISAPPEARANCE_RATIONALE: If the printing press's causal role were shown to be non-deterministic (i.e., the Reformation could plausibly have occurred, or failed to occur, independent of print technology), the entire explanatory edifice built on this reading collapses into the strategic_deployment or mutual_shaping accounts — historians disagree sharply on how much of the Reformation's spread was print-caused versus print-enabled-but-agent-driven, which is why this is contested rather than settled either way.
% FOUNDING_PROBLEM: The reading was built to explain why the Reformation could not be suppressed the way earlier heterodoxies (Hussites, Lollards, Cathars) had been contained: something structural had changed between the 14th and 16th centuries, and the press is the most visible technological discontinuity available.
% FOUNDING_PROBLEM_CORROBORATION: Media theorists (Eisenstein's print-culture thesis) and some historians of science attest the causal-determinist reading from outside any Reformation-partisan seat. However, social and cultural historians (e.g., scholars emphasizing preacher networks, patronage, and political alliance-building) dispute it from equally external positions, arguing the press was necessary but not sufficient and that strategic human choices about what to print, translate, and distribute did the causal work usually credited to the machine alone.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, contested).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.15, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is kept low (0.15) and rising only slightly over the century because the determinist reading treats the press's operation as a natural-law-like diffusion process, not an extraction mechanism — no party is described as taking a cut through the constraint's operation. Suppression is very low (0.1) because determinism denies the Church's suppression campaigns any real efficacy — the metric measures the CONSTRAINT's own coercive force, and this reading's core claim is precisely that no coercive force could hold against the reproduction mathematics. Accessibility collapse is high (0.8): once the press exists and diffuses, alternatives (centralized manuscript control, effective licensing) become nearly unavailable to the Church, which is exactly the determinist claim. Resistance is present but modest (0.25) — historically the Church tried hard (indices, licensing, book burnings), but under this reading that resistance is causally inert, so it registers as attempted-but-structurally-irrelevant rather than a genuine check on the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers, printers, and the vernacular laity are declared beneficiaries because the reading treats their gains as flowing automatically from the technology's diffusion, independent of their own strategic effort — this is what distinguishes the determinist reading from strategic_deployment, where the same actors would be agents rather than recipients. The Church sits as payer/target because its institutional monopoly on textual authority is what the exogenous capacity erodes, and under this reading it has essentially no directional leverage to prevent that erosion. Scribal workshops are excluded rather than payers proper — their loss is real but the determinist frame does not register it as an extraction from them by an agent, only as displacement by an impersonal process, which is itself a feature of this reading worth flagging as morally thin.
 *
 * MANDATROPHY ANALYSIS:
 *   The determinism claim resists mandatrophy analysis in the usual sense because it asserts no mandate at all — the press is framed as brute technological fact, not an institution whose founding purpose could be outlived. The founding_problem/status/corroboration trio is included here as the R5 genealogy of the READING itself (why historians built this explanatory frame), which is a different object from the mandate of a governing institution; the contested status reflects live disagreement among media historians and social historians outside any Reformation-partisan interest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_as_natural_law_or_constructed_narrative,
    'Is the printing press''s causal role in the Reformation a genuine structural/technological fact independent of human strategy (this reading), or is ''technological determinism'' itself a retrospective narrative constructed by later media theorists and reformist historiography to naturalize what were actually contingent strategic choices by reformers and printers?',
    'Comparative historical analysis of regions/periods where print technology existed but Reformation-style religious rupture did not occur (e.g. print flourished in Catholic Italy and France without equivalent doctrinal fracture), and counter-analysis of whether pre-print heterodoxies (Hussite Bohemia) achieved comparable persistence through non-print means. Convergent evidence for print-independent variation would undermine the determinist claim.',
    'If resolved toward constructed-narrative, this reading''s mountain classification is unsound and the constraint more properly belongs to the strategic_deployment or mutual_shaping family — the beneficiaries listed here would be recast as agents rather than downstream recipients, and the FSM signature would likely be warranted (naturalized technology story serving the historiographical interests of print-culture theorists and Protestant-sympathetic historiography).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_as_natural_law_or_constructed_narrative, conceptual, 'Whether the determinist reading describes brute technological causation or naturalizes a contingent, agent-driven historical process.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the three kernel readings (technological_determinism, strategic_deployment, mutual_shaping) disagree — is it about the DEGREE of technological autonomy, or about the unit of analysis (technology-as-cause vs. technology-as-tool vs. technology-as-co-constituted-with-practice)?',
    'Formal comparison of the three constraint files'' beneficiary/victim structures and cs_structure.axioms: determinism attributes causal weight to the artifact itself; strategic_deployment attributes it to reformer/printer intent; mutual_shaping attributes it to the interaction. The disagreement is locatable in whether ''the Church''s resistance was futile'' is a technical claim (this reading) or a claim about the Church''s tactical failures relative to what a differently-strategizing Church might have achieved (sibling readings).',
    'Determines whether the three readings can in principle be reconciled into a single mutual_shaping account or remain genuinely incompatible framings requiring separate historiographical traditions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the precise structural disagreement between the three sibling readings of the press-Reformation kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.05).
narrative_ontology:measurement(pres_tr_t1470, press_reformation_causation__technological_determinism, theater_ratio, 1470, 0.08).
narrative_ontology:measurement(pres_tr_t1490, press_reformation_causation__technological_determinism, theater_ratio, 1490, 0.12).
narrative_ontology:measurement(pres_tr_t1510, press_reformation_causation__technological_determinism, theater_ratio, 1510, 0.16).
narrative_ontology:measurement(pres_tr_t1530, press_reformation_causation__technological_determinism, theater_ratio, 1530, 0.18).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.2).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement(pres_be_t1470, press_reformation_causation__technological_determinism, base_extractiveness, 1470, 0.1).
narrative_ontology:measurement(pres_be_t1490, press_reformation_causation__technological_determinism, base_extractiveness, 1490, 0.12).
narrative_ontology:measurement(pres_be_t1510, press_reformation_causation__technological_determinism, base_extractiveness, 1510, 0.14).
narrative_ontology:measurement(pres_be_t1530, press_reformation_causation__technological_determinism, base_extractiveness, 1530, 0.15).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__technological_determinism, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language claim 'the printing press caused the Reformation.' Each reading is generated as a clean, ε-invariant constraint per the ε-invariance principle: technological_determinism (this file, ε=0.15, mountain-claimed, press as exogenous upstream cause), strategic_deployment (reformers/printers as strategic agents, technology as neutral tool), and mutual_shaping (technology and agency co-evolving, feedback loop). The three are linked via affects_constraints rather than merged, because their beneficiary structures, causal attributions, and ε values differ in ways that would corrupt a single classification if forced into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
