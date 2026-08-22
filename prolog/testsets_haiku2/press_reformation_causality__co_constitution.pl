% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__co_constitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__co_constitution, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: press_reformation_causality__co_constitution
 *   human_readable: Print-Religious Agency Co-Constitution in the Reformation
 *   domain: history/technology/religious
 *
 * SUMMARY:
 *   The Reformation (1517–1648) occurred in a world that had acquired
 *   printing technology (Gutenberg, mid-1400s) and inherited 70 years of
 *   print-market development. This constraint story models the
 *   co-constitutional reading: technology and religious agency together
 *   created feedback loops that neither could generate alone. Printers
 *   printed reform content because it sold; reformers wrote for print markets
 *   because their message reached farther; lay readers demanded vernacular
 *   scripture because it was available; the institutional Church was forced
 *   to suppress print because its authority structure was structurally
 *   dependent on manuscript gatekeeping. The constraint operated through
 *   mutual constitution — each agent's agency was constituted-through and
 *   constitutive-of the others' — rather than through unilateral causation
 *   (determinism reading) or pure instrumental strategy (strategic-deployment
 *   reading). The claim/metric gap is intentional: this reading is authored
 *   as tangled_rope (coordination + asymmetric extraction across stakeholder
 *   seats), not as a pure coordination rope, because the constraint's
 *   operation benefited some (printers, reformers, lay readers) while
 *   extracting from others (scribes, institutional Church) — the coordination
 *   function produced distributed extraction.
 *
 * KEY AGENTS:
 *   - Printer entrepreneurs: organized actors with capital and technology; motivated by market but structurally enabling the feedback loop
 *   - Reform clergy (Luther, Zwingli, Calvin): theological innovators whose authority grew through print but was constituted-through printer distribution
 *   - Manuscript scribes: displaced labor, bearing cost of technological obsolescence
 *   - Catholic institutional authority: trapped by its own manuscript-dependent architecture; forced into costly suppression
 *   - Lay readers: beneficiaries of access but stratified by literacy and geography
 *   - Political authorities: observers/instrumentalizers navigating religious-economic terrain
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__co_constitution, 0.58).
domain_priors:suppression_score(press_reformation_causality__co_constitution, 0.62).
domain_priors:theater_ratio(press_reformation_causality__co_constitution, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, extractiveness, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(press_reformation_causality__co_constitution, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__co_constitution, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__co_constitution, "Print-Religious Agency Co-Constitution in the Reformation").
narrative_ontology:topic_domain(press_reformation_causality__co_constitution, "history/technology/religious").

domain_priors:requires_active_enforcement(press_reformation_causality__co_constitution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__co_constitution, '6a51f624-c98b-4ead-86d9-5559e4e30d7c').
narrative_ontology:cs_kernel_codification('6a51f624-c98b-4ead-86d9-5559e4e30d7c', distributed).
narrative_ontology:cs_authority_grounding('6a51f624-c98b-4ead-86d9-5559e4e30d7c', practice).
narrative_ontology:cs_interpretation_layer_present('6a51f624-c98b-4ead-86d9-5559e4e30d7c').
narrative_ontology:cs_reading_relation('6a51f624-c98b-4ead-86d9-5559e4e30d7c', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('6a51f624-c98b-4ead-86d9-5559e4e30d7c', press_reformation_causality__strategic_deployment, influences).
narrative_ontology:cs_axiom('6a51f624-c98b-4ead-86d9-5559e4e30d7c', foundational, mutual_constitution_not_unilateral_determination).
narrative_ontology:cs_axiom_status(mutual_constitution_not_unilateral_determination, holdable).
narrative_ontology:cs_axiom_grounding('6a51f624-c98b-4ead-86d9-5559e4e30d7c', mutual_constitution_not_unilateral_determination, deontological).
narrative_ontology:cs_axiom('6a51f624-c98b-4ead-86d9-5559e4e30d7c', foundational, feedback_loops_generate_emergent_properties).
narrative_ontology:cs_axiom_status(feedback_loops_generate_emergent_properties, holdable).
narrative_ontology:cs_axiom_grounding('6a51f624-c98b-4ead-86d9-5559e4e30d7c', feedback_loops_generate_emergent_properties, empirically_contingent).
narrative_ontology:cs_reference_frame('6a51f624-c98b-4ead-86d9-5559e4e30d7c', printing_technology_neutral_tool).
narrative_ontology:cs_drift_state('6a51f624-c98b-4ead-86d9-5559e4e30d7c', mid_sixteenth_century_reformation_peak, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6a51f624-c98b-4ead-86d9-5559e4e30d7c', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__co_constitution, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, printer_entrepreneurs).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, reform_clergy).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, manuscript_scribes).
narrative_ontology:constraint_victim(press_reformation_causality__co_constitution, catholic_institutional_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(press_reformation_causality__co_constitution, lay_readers).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, agency_technology_mutual_constitution).
narrative_ontology:constraint_vindicates(press_reformation_causality__co_constitution, feedback_loop_causality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invested capital in printing technology and business infrastructure. Benefited from surging demand for religious texts, controversial treatises, and vernacular scripture. Simultaneously enablers of the Reformation's spread and rational economic actors pursuing markets — their interest in printing reform content is inseparable from their profit motive. They set publication schedules, selected which texts to print, and shaped the information environment through market decisions.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, printer_entrepreneurs, agenda_setter,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, printer_entrepreneurs, beneficiary).

% Developed theological positions and sought to propagate them. Found in printing technology an unprecedented distribution mechanism. Their authority and visibility grew through print; their theological innovations could reach thousands instead of staying within manuscript tradition. The same technology that enabled their message also restructured their role — they became authors for print markets, not just clerics.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, reform_clergy, agenda_setter,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__co_constitution, reform_clergy, beneficiary).

% Their labor was rendered economically obsolete by printing. Income from manuscript copying declined as printers undercut their rates and volume. The constraint operates against them through market displacement rather than direct coercion — they bore the cost of the printing economy's success without participating in its gains.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, manuscript_scribes, payer,
    moderate, biographical, constrained, regional).

% Faced undermined authority as printing enabled rapid dissemination of competing theological claims and vernacular scripture. Their monopoly on biblical interpretation, sustained by manuscript control and Latin-literacy gatekeeping, was structurally broken by print. Suppression (censorship, book banning, Inquisition machinery) was their active-enforcement response — the constraint's persistence required institutional investment in detecting and destroying print materials.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, catholic_institutional_authority, payer,
    institutional, civilizational, trapped, universal).

% Gained access to religious texts in vernacular languages at affordable prices. Direct reading of scripture became possible; participation in theological controversy moved into vernacular publics. They were coordinated by the print economy (common texts, shared interpretations) and stratified by it (those who could read/afford were included; others were not).
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, lay_readers, beneficiary,
    powerless, biographical, mobile, regional).

% Observed the constraint's operation from seats that navigated between Catholic and Reformed camps. Used printing to advance their own legitimacy claims and religious preferences. Neither simple beneficiaries nor victims — they instrumentalized the constraint's dynamics for political advantage.
narrative_ontology:constraint_stakeholder(press_reformation_causality__co_constitution, political_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__co_constitution, printer_entrepreneurs).
narrative_ontology:fixing_cost_class(press_reformation_causality__co_constitution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Printing technology coordinated geographically dispersed theological communities around shared texts and rapid feedback cycles: a sermon could be printed and circulated within weeks, generating published responses that themselves circulated, creating an emergent discourse network. This feedback loop enabled theological innovation at scale — the Reformation as a coordinated intellectual event became structurally possible only through print's coordination of author, printer, reader, and respondent.
% TRANSFER_FUNCTION: The constraint moves authority, economic surplus, and interpretive power: from manuscript gatekeepers (scribes, Latin clergy) to printers and reformers; from Catholic institutional monopoly on scripture interpretation to lay readers with affordable vernacular texts; from dispersed local churches to a transnational print-mediated community. Printers extract profit; reformers extract authority; lay readers extract access; institutional Church loses interpretive monopoly.
% ABSENT_VOICES: Illiterate populations and non-Latin-reading women were structurally excluded from the print-enabled theological public sphere despite potential inclusion in lay-reader beneficiaries. Indigenous peoples and non-Christian communities had no voice in the Reformation's religious stakes. Print entrepreneurs from regions without capital formation (Eastern Europe, non-urban areas) were absent from the printing innovation networks that concentrated in urban commercial centers.
% DISAPPEARANCE_RATIONALE: If printing had not been invented or adopted, the Reformation as a continental-scale religious revolution would not have occurred in the 16th century. Theological critique existed before — but its scope was constrained by manuscript transmission. Without print, the constraint ceases to operate: no feedback loop between printer investment and reformer amplification, no rapid circulation enabling transnational theological coordination, no market-driven translation of scripture into vernacular languages. The religious landscape would reorganize around localized clerical authority and manuscript scarcity.
% FOUNDING_PROBLEM: In the 1440s–1480s, Christian theology was contested (councils disputed doctrine, mystics challenged institutional authority) but disputes were local, slow to spread, and resolved by institutional power rather than intellectual persuasion. Manuscript copying was expensive, slow, and controlled by the Church. Reformers had theological innovations but no infrastructure to spread them beyond their immediate regions. Printers had capital and technology but no stable market. These three elements — theological discontent, communication bottleneck, and capital seeking application — were structural preconditions waiting for a feedback loop.
% FOUNDING_PROBLEM_CORROBORATION: Historians of print (Eisenstein, Ong) and Reformation scholars (Pettegree, Wiesner-Hanks) outside the benefiting parties attest the feedback-loop reading: printing enabled Reformation scale and speed that theology alone could not generate. Defenders of determinism-reading (some historians) attest the press as autonomous cause. Strategic-deployment readers attest deliberate weaponization by reformers. The corroboration splits because the founding problem is the question under contest: was the constraint a solution to a real bottleneck (feedback-loop reading wins), an inevitable outcome of technology (determinism reading), or a calculated tool (strategic-deployment reading)?
narrative_ontology:disappearance_verdict(press_reformation_causality__co_constitution, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__co_constitution, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__co_constitution, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__co_constitution, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__co_constitution, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__co_constitution_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__co_constitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__co_constitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (early stage, when printing existed but Reformation was not yet underway) to 0.58 (established constraint, when the feedback loop was mature and extraction patterns were locked in). Suppression rises sharply from 0.28 to 0.62 by mid-interval, reflecting the Catholic Church's escalating censorship, Inquisition, and book-burning machinery — as the constraint's coordination function became more visible, institutional resistance required more active enforcement. Suppression plateaus by 1570s–1580s: the Church had built powerful censorship infrastructure (Index Librorum Prohibitorum, the printing censorate), but suppression could not eliminate print's coordination function — only manage its proliferation. Theater ratio rises to 0.41 by end-interval, indicating that a growing share of the Church's enforcement activity became performative (book burnings, condemnations) rather than functionally suppressing the flow — the constraint had matured into a stable standoff where each side's enforcement was partly ceremonial. The measurements are authored on one shared time grid: every metric is valued at every examined time point (t=0,10,20,40,60,80), enabling alignment-free temporal analysis.
 *
 * PERSPECTIVAL GAP:
 *   From the printer-reformer seats, this constraint is genuine coordination: a feedback loop solving real problems (distributing theological innovation, accessing scripture, creating intellectual markets). From the institutional Church seat, the same structure operates as a coordinated assault on its authority — enforcement required because the constraint's logic systematically undermines its power. From the scribe's position, it is pure extraction (displacement without compensation). The engine computes these divergences from the structural data: organized beneficiaries with low d, institutional target with high d, displaced moderate actors with constrained exit. The authored metrics (high suppression, moderate theater) describe what happens when the beneficiary seats win the coordination coordination function but the target seat refuses to accept the outcome — enforcement becomes the binding constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Printers and reformers occupy beneficiary seats with high agency: they set the agenda (choose what to print, what to preach), collect rents (printers profit, reformers gain authority), and have organized power. Their directionality is low (near 0.0) — they are subsidized by the constraint's operation. Institutional Church occupies target seat: trapped by the constraint's structural logic (cannot choose to un-adopt manuscript gatekeeping without admitting its fragility), forced into expensive enforcement, extractive overhead mounted against them. Their directionality is high (near 1.0) — the constraint extracts from their authority. Lay readers are symmetric-to-beneficiary: they gain access (coordination function) but also carry diffuse costs (literacy requirements stratify them, printer control of texts diverts them from alternative literacies). Manuscript scribes are targets: economically displaced by a coordination function they did not participate in designing.
 *
 * MANDATROPHY ANALYSIS:
 *   The co-constitutional reading avoids a common false-positive: classifying the Reformation-printing link as pure rope (genuine coordination benefiting all) or pure snare (technology weaponized against the Church). The tangled-rope classification captures the real structure: coordination function (feedback loop between printers and reformers; access for lay readers) is genuine and benefits those seats; but the same structure extracts from institutional Church (monopoly on interpretation broken) and scribes (labor obsolete). The constraint persists because the beneficiary seats have the power and motivation to maintain it; active enforcement (suppression) is required because the target seats (especially Church) refuse the coordinate outcome and would revert the constraint if able. Mandatrophy is not present: the founding problem (distributing theological innovation, accessing scripture) is still live at interval end, not dead, so the constraint persists for its original function, not theatrical inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feedback_loop_vs_instrumental_causation,
    'Is the printing-Reformation linkage a genuine feedback loop (each agent''s agency constituted-through the other) or a sequence of instrumental decisions by strategically rational actors using technology as a tool?',
    'Comparative analysis of reformer intentions and printer motivations: if independent sources show shared intentionality toward religious revolution, strategic-deployment reading gains support; if printers are motivated primarily by profit and reformers by theology, with alignment emerging from market incentives rather than coordination, feedback-loop reading holds.',
    'Strategic-deployment reading would reframe the constraint as snare (coordinated weaponization against institutional Church); feedback-loop reading keeps it tangled_rope (mutual constitution across multiple agent interests).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feedback_loop_vs_instrumental_causation, empirical, 'Whether agents deliberately coordinated to achieve Reformation or coordination emerged from independent rational interests.').

omega_variable(
    technology_necessity_question,
    'Was printing technology necessary (no Reformation without it) or merely enabling (Reformation could have occurred through other channels, slower or smaller)?',
    'Counterfactual historical analysis: models of religious innovation spread through pre-print networks, theological change in regions without early printing, and analysis of what theological resources (argumentation, textual authority) existed pre-print and could have supported reformation without printing.',
    'If necessary, determinism reading gains support (technology is causal priority); if enabling, co-constitutional reading holds (technology co-constitutes with agency but is not sufficient alone).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_necessity_question, conceptual, 'Whether printing technology was necessary precondition or contingent enabling infrastructure for Reformation-scale religious change.').

omega_variable(
    institutional_church_agency_constraint,
    'Was the institutional Church''s loss of interpretive monopoly a structural consequence of printing technology that it could not have prevented, or a failure of institutional strategy that different leadership could have resisted?',
    'Analysis of contemporary Church responses: if Church leaders attempted to control printing and failed despite committed effort (technological constraint), technology-constraint reading wins; if Church failed to attempt control, or attempted half-heartedly, institutional-agency reading wins.',
    'If constraint was structural (Church could not prevent), determinism reading gains credibility; if constraint was institutional failure, co-constitutional reading holds (Church''s choice to suppress rather than adapt was a constitutive agent action).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_church_agency_constraint, empirical, 'Whether the institutional Church''s loss of monopoly was technologically forced or institutionally chosen.').

omega_variable(
    co_constitution_operationalization,
    'What observable phenomena would distinguish co-constitutional feedback loops from either deterministic technology unfolding OR strategic instrumental coordination?',
    'Specification of distinguishing signals: co-constitutional feedback loops should show (1) iterative cycles where reformer publications trigger printer investments trigger lay-reader demand trigger reformer responses; (2) feedback-loop instability (oscillations in publication rates, theological positions, suppression intensity); (3) emergent properties not-fully-foreseeable by any single agent; (4) mutual adaptation by multiple agent types; (5) unintended consequences (e.g., lay readers asking questions reformers did not anticipate).',
    'Clear specification enables future research to test which causal model (determinism, instrumental strategy, co-constitution) better fits the empirical record. This omega documents the reading''s operationalizability — a conceptual reading that cannot be tested is a literary claim, not a causal model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(co_constitution_operationalization, conceptual, 'What would it take to operationalize and test the co-constitutional claim empirically.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__co_constitution, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causality__co_constitution, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(pres_tr_t0, projected).
narrative_ontology:measurement(pres_tr_t10, press_reformation_causality__co_constitution, theater_ratio, 10, 0.21).
narrative_ontology:measurement_basis(pres_tr_t10, observed).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causality__co_constitution, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(pres_tr_t20, observed).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causality__co_constitution, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(pres_tr_t40, observed).
narrative_ontology:measurement(pres_tr_t60, press_reformation_causality__co_constitution, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(pres_tr_t60, observed).
narrative_ontology:measurement(pres_tr_t80, press_reformation_causality__co_constitution, theater_ratio, 80, 0.41).
narrative_ontology:measurement_basis(pres_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causality__co_constitution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(pres_be_t0, projected).
narrative_ontology:measurement(pres_be_t10, press_reformation_causality__co_constitution, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(pres_be_t10, observed).
narrative_ontology:measurement(pres_be_t20, press_reformation_causality__co_constitution, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(pres_be_t20, observed).
narrative_ontology:measurement(pres_be_t40, press_reformation_causality__co_constitution, base_extractiveness, 40, 0.56).
narrative_ontology:measurement_basis(pres_be_t40, observed).
narrative_ontology:measurement(pres_be_t60, press_reformation_causality__co_constitution, base_extractiveness, 60, 0.57).
narrative_ontology:measurement_basis(pres_be_t60, observed).
narrative_ontology:measurement(pres_be_t80, press_reformation_causality__co_constitution, base_extractiveness, 80, 0.58).
narrative_ontology:measurement_basis(pres_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causality__co_constitution, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(pres_su_t0, projected).
narrative_ontology:measurement(pres_su_t10, press_reformation_causality__co_constitution, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(pres_su_t10, observed).
narrative_ontology:measurement(pres_su_t20, press_reformation_causality__co_constitution, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(pres_su_t20, observed).
narrative_ontology:measurement(pres_su_t40, press_reformation_causality__co_constitution, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(pres_su_t40, observed).
narrative_ontology:measurement(pres_su_t60, press_reformation_causality__co_constitution, suppression_requirement, 60, 0.63).
narrative_ontology:measurement_basis(pres_su_t60, observed).
narrative_ontology:measurement(pres_su_t80, press_reformation_causality__co_constitution, suppression_requirement, 80, 0.62).
narrative_ontology:measurement_basis(pres_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__co_constitution, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__co_constitution, 0.12).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__co_constitution, press_reformation_causality__strategic_deployment).

% DUAL FORMULATION NOTE:
% This constraint is one reading (co-constitutional) of a contested kernel (press_reformation_causality) with two sibling readings: technological_determinism (printing press as autonomous cause) and strategic_deployment (reformers and printers as deliberate strategists). All three readings share the same historical events but disagree on causal structure. They are linked via network.affects_constraints. The co-constitutional reading treats the constraint as tangled_rope; determinism would treat it as rope or mountain; strategic-deployment would treat it as snare. The three constraints together model the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causality__co_constitution, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
