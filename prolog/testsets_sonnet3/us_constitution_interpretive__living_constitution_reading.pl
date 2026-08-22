% ============================================================================
% CONSTRAINT STORY: us_constitution_interpretive__living_constitution_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_interpretive__living_constitution_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: us_constitution_interpretive__living_constitution_reading
 *   human_readable: Living Constitution: Evolving Interpretive Authority
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story authors the living-constitution reading of the
 *   interpretive-authority kernel: the claim that constitutional meaning
 *   legitimately evolves with societal values, and that interpretive
 *   authority derives from reasoned judicial adaptation to contemporary
 *   conditions rather than from fidelity to a fixed original meaning or
 *   exclusively from democratic contestation. Under this reading, the federal
 *   judiciary holds broad interpretive power to expand federal authority (via
 *   an evolving reading of the Commerce Clause and implied powers) and to
 *   recognize unenumerated rights (privacy, dignity, equal personhood) as
 *   social consensus shifts. This is a genuine coordination mechanism — it
 *   solves the real problem of an 18th-century text governing a 21st-century
 *   society without perpetual constitutional convention — but it also
 *   transfers interpretive authority and substantive protection
 *   asymmetrically: federal power expands at the expense of state authority,
 *   and beneficiary groups gain enforceable protection while textualist
 *   jurists and federalism advocates experience a one-directional erosion of
 *   their preferred doctrine's governing weight every time courts invoke
 *   evolving standards.
 *
 * KEY AGENTS:
 *   - federal_judiciary: administers the doctrine, decides how far to extend it (institutional/analytical)
 *   - civil_rights_expansion_claimants, reproductive_autonomy_advocates, lgbtq_rights_claimants: beneficiaries whose legal protections depend on the evolving reading
 *   - federal_regulatory_agencies: institutional beneficiary of expanded implied-powers doctrine
 *   - states_rights_advocates, original_meaning_textualists: bear the doctrinal and authority loss
 *   - entities_constrained_by_expanded_federal_reach: bear compliance costs from expanded federal jurisdiction
 *   - popular_political_movements: excluded — their influence must route through judicial recognition to count
 *   - constitutional_law_scholars: analytical observers of doctrinal drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, 0.52).
domain_priors:suppression_score(us_constitution_interpretive__living_constitution_reading, 0.44).
domain_priors:theater_ratio(us_constitution_interpretive__living_constitution_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_interpretive__living_constitution_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_interpretive__living_constitution_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_interpretive__living_constitution_reading, "Living Constitution: Evolving Interpretive Authority").
narrative_ontology:topic_domain(us_constitution_interpretive__living_constitution_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(us_constitution_interpretive__living_constitution_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_interpretive__living_constitution_reading, '2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e').
narrative_ontology:cs_kernel_codification('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', fixed_text).
narrative_ontology:cs_authority_grounding('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', lineage).
narrative_ontology:cs_interpretation_layer_present('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e').
narrative_ontology:cs_reading_relation('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', us_constitution_interpretive__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', us_constitution_interpretive__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', foundational, constitutional_meaning_tracks_societal_evolution).
narrative_ontology:cs_axiom_status(constitutional_meaning_tracks_societal_evolution, holdable).
narrative_ontology:cs_axiom_grounding('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', constitutional_meaning_tracks_societal_evolution, instrumental).
narrative_ontology:cs_axiom('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', foundational, judicial_reasoning_from_contemporary_conditions_is_legitimate_authority).
narrative_ontology:cs_axiom_status(judicial_reasoning_from_contemporary_conditions_is_legitimate_authority, holdable).
narrative_ontology:cs_axiom_grounding('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', judicial_reasoning_from_contemporary_conditions_is_legitimate_authority, conventional).
narrative_ontology:cs_reference_frame('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', textual_fidelity_baseline).
narrative_ontology:cs_drift_state('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', contemporary_rights_jurisprudence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2eb68ef1-d0d3-4c9c-96f0-54939d51dd6e', '').
narrative_ontology:cs_kernel_id(us_constitution_interpretive__living_constitution_reading, us_constitution_interpretive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants).
narrative_ontology:constraint_beneficiary(us_constitution_interpretive__living_constitution_reading, federal_regulatory_agencies).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists).
narrative_ontology:constraint_victim(us_constitution_interpretive__living_constitution_reading, entities_constrained_by_expanded_federal_reach).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal courts, especially the Supreme Court, determine what contemporary conditions and societal values require of constitutional text. They administer the doctrine by selecting which precedents to extend, which historical facts count as changed circumstances, and how far implied powers and unenumerated rights reach. They neither collect money nor bear direct costs from the doctrine but control its application entirely.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Groups seeking recognition of rights not enumerated in the constitutional text — racial equality claimants historically, disability rights advocates, and others — depend on courts reading the document as capable of extending protection as social understanding changes. Without the living reading, their claims would need to route through the amendment process, which is slow and often practically foreclosed.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, civil_rights_expansion_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Advocates for reproductive rights relied on the doctrine of evolving unenumerated rights (privacy, bodily autonomy) to secure protection not found in the constitutional text. Their legal position is directly tied to the durability of the living-constitution framework; when the doctrine's grip loosens, their protections become vulnerable to reversal.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, reproductive_autonomy_advocates, beneficiary,
    organized, biographical, constrained, national).

% Marriage equality, anti-discrimination, and dignity-based claims were recognized through the living-constitution's capacity to read equal protection and due process against contemporary social understanding rather than 1868 or 1791 meaning. This population has no alternative textual hook without the evolving-meaning framework.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, lgbtq_rights_claimants, beneficiary,
    organized, biographical, constrained, national).

% Agencies operating under an expansive reading of the Commerce Clause and implied federal powers gain regulatory jurisdiction over activity the framers could not have contemplated. Their institutional scope and budgetary authority track directly with how far courts are willing to stretch enumerated powers doctrine.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, federal_regulatory_agencies, beneficiary,
    institutional, civilizational, arbitrage, national).

% State governments and advocates for federalism experience the expanding Commerce Clause and implied-powers doctrine as a one-directional transfer of regulatory authority from states to the federal government. They can litigate and lobby but cannot exit the constitutional system; their remedy is confined to appointing sympathetic judges or amending the text, both slow and uncertain.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, states_rights_advocates, payer,
    organized, generational, constrained, national).

% Judges, scholars, and litigants committed to original-meaning interpretation experience the living-constitution doctrine as displacing their preferred interpretive method from governing authority whenever courts adopt the evolving-values frame. Their objection is that the doctrine allows judges to substitute contemporary policy preference for textual constraint, and they bear the professional and doctrinal cost of losing interpretive ground each time it is invoked.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, original_meaning_textualists, payer,
    organized, civilizational, constrained, national).

% Businesses, individuals, and local institutions newly subject to federal regulation under an expanded Commerce Clause or implied-powers reading bear compliance costs and lost autonomy that would not exist under a narrower interpretation. They typically learn the constraint applies to them only after a court extends doctrine to reach their conduct.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, entities_constrained_by_expanded_federal_reach, payer,
    moderate, biographical, constrained, national).

% Social and political movements that shape constitutional meaning through mobilization, legislation, and electoral contestation are structurally secondary under this reading — their influence must be laundered through judicial recognition before it counts as constitutional change. A movement that fails to persuade the judiciary, however popular, has no independent constitutional voice under this framework.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, popular_political_movements, excluded,
    organized, generational, constrained, national).

% Academic commentators analyze doctrinal drift, track which interpretive theory dominates in a given era, and assess whether the living-constitution framework is being applied as principled adaptation or as unconstrained judicial policymaking. They do not hold power to change the doctrine directly but shape which theory judges cite as legitimate.
narrative_ontology:constraint_stakeholder(us_constitution_interpretive__living_constitution_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_interpretive__living_constitution_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_interpretive__living_constitution_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for a centuries-old constitutional text to remain applicable to circumstances the framers could not foresee — new technologies, changed social understanding of equality and dignity, and federal governance needs unimagined in 1787 — without requiring formal amendment for every adaptation.
% TRANSFER_FUNCTION: Moves interpretive authority from the amendment process (Article V, requiring supermajority consensus) and from state governments to the federal judiciary; moves substantive protection to previously unrecognized claimant groups by reading rights and powers into text that does not name them explicitly.
% ABSENT_VOICES: Popular political movements and state legislatures that would prefer constitutional change to occur through democratic contestation or formal amendment rather than judicial reinterpretation are structurally routed through the courts to have any effect — their political victories carry no independent constitutional weight unless a court also adopts them.
% DISAPPEARANCE_RATIONALE: If the living-constitution doctrine disappeared overnight and courts adopted a strictly fixed-meaning approach, decades of precedent grounded in evolving interpretation — desegregation doctrine as currently reasoned, substantive due process privacy protections, expansive Commerce Clause federal regulatory authority, and unenumerated dignity-based rights — would lose their doctrinal foundation and become vulnerable to reversal or require re-derivation from different textual hooks; federal agency jurisdiction would contract; state regulatory authority would expand correspondingly.
% FOUNDING_PROBLEM: A written constitution ratified in 1787 (and amended sporadically thereafter) needed to remain a workable governing document for a society transformed by industrialization, civil rights struggles, technological change, and shifting moral consensus, without triggering constant formal amendment battles that the Article V supermajority threshold makes extremely difficult to win.
% FOUNDING_PROBLEM_CORROBORATION: Sitting and former federal judges across the ideological spectrum, including some originalist-leaning jurists, acknowledge that some degree of doctrinal adaptation has always occurred in practice even under textualist rhetoric; constitutional historians outside any advocacy position document that formal amendment has in fact been used successfully for major changes (13th-15th, 19th Amendments), which originalist scholars cite as evidence the founding problem is overstated and the living approach substitutes judicial preference for the deliberately difficult democratic amendment process the framers designed.
narrative_ontology:disappearance_verdict(us_constitution_interpretive__living_constitution_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_interpretive__living_constitution_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_interpretive__living_constitution_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_interpretive__living_constitution_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_interpretive__living_constitution_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_interpretive__living_constitution_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_interpretive__living_constitution_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_interpretive__living_constitution_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at present) because the doctrine performs real coordination work (adapting an old text to new conditions) alongside a genuine asymmetric transfer (federal power and judicially-recognized rights expand at the direct expense of state authority and textualist interpretive legitimacy). Suppression is moderate (0.44) — the doctrine does not physically coerce dissenters, but it does displace competing interpretive methods from governing authority whenever invoked, and stare decisis makes reversal costly. Theater ratio is low-moderate (0.28) — the doctrine is substantively applied, not merely performed, though critics argue some 'evolving values' reasoning masks results-oriented decision-making. Accessibility collapse is moderate (0.4): the amendment process remains formally available as an alternative but is practically very difficult (Article V supermajority), so alternatives are only partially foreclosed. Resistance is high (0.72): originalist jurisprudence, federalism litigation, and academic textualist critique represent substantial, organized, sustained resistance to this reading — it has never achieved uncontested dominance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary groups (civil rights claimants, reproductive autonomy advocates, LGBTQ+ claimants, federal agencies) sit near the beneficiary end of directionality — the doctrine is the enabling mechanism for their substantive gains, and without it their legal position would need alternative, likely weaker, textual grounding. Victim groups (states' rights advocates, original-meaning textualists, entities newly reached by federal regulation) sit near the target end — they experience concrete loss of authority, doctrinal legitimacy, or regulatory autonomy each time the doctrine is invoked to expand federal reach or recognize new rights. The federal judiciary itself is the agenda-setter: it does not 'benefit' in an extraction sense but administers and expands the doctrine's application, which is why it is coded as agenda_setter rather than beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — an 18th-century text needing to remain workable for radically changed circumstances — is genuinely contested rather than resolved or extinct: it remains partially live (constitutional text still requires interpretation to reach modern circumstances) but the corroboration record shows disagreement about whether judicial reinterpretation is the only viable mechanism or whether the amendment process (used successfully historically) and popular constitutionalism could substitute. This is not classified as a snare because the coordination function is real and independently corroborated by non-beneficiary sources (constitutional historians, cross-ideological judicial acknowledgment of interpretive adaptation in practice); it is not classified as a pure rope because the asymmetric costs to states' rights advocates and textualists are real, structural, and recur every time the doctrine is invoked rather than being incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_reading_vs_sibling_readings_location,
    'Where exactly does the living-constitution reading''s core premise diverge from the originalist and popular-constitutionalism readings, and is that divergence located in WHO holds interpretive authority (judiciary vs. framers'' fixed text vs. popular movements) or in WHAT COUNTS as legitimate constitutional change (judicial reasoning about contemporary conditions vs. textual fidelity vs. democratic mobilization)?',
    'This is a committer-structure question, not an empirical one within this story — it is recorded here per Rule 2 rather than folded into the metrics. Resolution would require comparative analysis across all three sibling constraint files documenting where their core premises actually conflict versus merely differ in emphasis.',
    'If the divergence is primarily about WHO holds authority, this reading and originalism may be in a forecloses relationship (judicial supremacy over evolving meaning vs. fixed-meaning judicial constraint cannot both govern the same case). If the divergence is primarily about degree of judicial deference to contemporary values, the readings may coexist as competing methodologies applied inconsistently across cases and eras.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(living_reading_vs_sibling_readings_location, conceptual, 'Where the living-constitution reading''s premise structurally diverges from its sibling readings within the interpretive-authority kernel.').

omega_variable(
    genuine_adaptation_vs_result_oriented_reasoning,
    'Is ''reasoned adaptation to contemporary conditions'' a genuine, principled interpretive methodology, or is it, in practice, a post-hoc justification for judges reaching outcomes they prefer on policy grounds and then constructing an evolving-values narrative to support them?',
    'Empirical study of judicial opinion patterns: does invocation of ''evolving standards'' correlate more strongly with predictable ideological outcomes than invocation of fixed-meaning reasoning does, across a large sample of constitutional cases and multiple judicial eras?',
    'If evolving-values reasoning is shown to correlate strongly with judicial policy preference rather than principled adaptation, this reading''s coordination-function claim weakens substantially and the constraint moves toward the extraction end of the tangled-rope classification, potentially toward snare. If genuinely principled and cross-ideologically consistent, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_adaptation_vs_result_oriented_reasoning, empirical, 'Whether the doctrine''s core interpretive method is principled or result-oriented in practice.').

omega_variable(
    amendment_process_viability,
    'Is the Article V amendment process a genuinely viable alternative mechanism for constitutional adaptation (meaning the living-constitution doctrine substitutes for a workable process), or is it so practically foreclosed by supermajority requirements that judicial reinterpretation is the only realistic adaptation mechanism available?',
    'Historical frequency analysis of successful amendments relative to proposed amendments and relative to the scale of underlying social change each era faced; comparison to peer democracies'' constitutional amendment rates.',
    'If amendment is genuinely viable but underused, the living-constitution doctrine displaces a legitimate democratic alternative, weighting the classification toward extraction. If amendment is functionally foreclosed, the doctrine''s coordination function is closer to necessary, weighting toward genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_viability, empirical, 'Whether the formal amendment process constitutes a real available alternative to judicial reinterpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_interpretive__living_constitution_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_interpretive__living_constitution_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_interpretive__living_constitution_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_interpretive__living_constitution_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_interpretive__living_constitution_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement(us_c_tr_t80, us_constitution_interpretive__living_constitution_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement(us_c_tr_t100, us_constitution_interpretive__living_constitution_reading, theater_ratio, 100, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(us_c_be_t20, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(us_c_be_t40, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(us_c_be_t60, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 60, 0.47).
narrative_ontology:measurement(us_c_be_t80, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 80, 0.5).
narrative_ontology:measurement(us_c_be_t100, us_constitution_interpretive__living_constitution_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(us_c_su_t20, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 20, 0.32).
narrative_ontology:measurement(us_c_su_t40, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 40, 0.36).
narrative_ontology:measurement(us_c_su_t60, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 60, 0.4).
narrative_ontology:measurement(us_c_su_t80, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 80, 0.42).
narrative_ontology:measurement(us_c_su_t100, us_constitution_interpretive__living_constitution_reading, suppression_requirement, 100, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
