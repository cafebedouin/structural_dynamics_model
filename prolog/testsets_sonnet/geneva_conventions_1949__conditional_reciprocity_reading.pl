% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions 1949 — Conditional Reciprocity Reading
 *   domain: international_humanitarian_law/military_ethics
 *
 * SUMMARY:
 *   This story instantiates the conditional-reciprocity reading of the Geneva
 *   Conventions 1949 kernel: the conventions function as reciprocal
 *   restraints binding fully only where adversaries also comply, and
 *   non-compliance by irregular forces licenses proportional degradation of
 *   the protections a state would otherwise extend. Under this reading, POW
 *   status is gated on Article 4 criteria — organized command, distinctive
 *   insignia, open carriage of arms — and civilian immunity, while formally
 *   preserved, is narrowed through proportionality calculations that expand
 *   as adversary irregularity increases. This is a genuinely distinct
 *   constraint from its siblings, not a different observation angle on the
 *   same one: the humanitarian_ceiling_reading asserts unconditional minimums
 *   regardless of reciprocity (a materially lower extraction profile, closer
 *   to rope/mountain), and the security_maximization_reading treats the
 *   conventions as suspendable aspirations under operational necessity (a
 *   materially higher extraction profile, closer to snare). The three
 *   readings have different beneficiary/victim structures and different
 *   epistemic status; they are linked here only through
 *   network.affects_constraints and the shared kernel_id, per the
 *   ε-invariance decomposition rule.
 *
 * KEY AGENTS:
 *   - state_militaries: agenda_setter/beneficiary (institutional/arbitrage) — interprets and applies Article 4
 *   - regular_uniformed_combatants: beneficiary (organized/constrained) — automatically satisfies formal criteria
 *   - irregular_combatants: payer (powerless/trapped) — structurally cannot meet the formal test
 *   - civilians_in_asymmetric_conflict_zones: payer (powerless/trapped) — absorbs expanded proportionality tolerance
 *   - captured_militia_members_denied_pow_status: payer (powerless/trapped) — bears the direct cost of classification
 *   - detaining_power_legal_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — administers status tribunals
 *   - international_humanitarian_organizations: excluded (organized/constrained) — advocacy without adjudicative power
 *   - legal_scholars_and_tribunals: observer (analytical/analytical) — comparative jurisprudence across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.52).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.58).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions 1949 — Conditional Reciprocity Reading").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/military_ethics").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a').
narrative_ontology:cs_kernel_codification('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', fixed_text).
narrative_ontology:cs_authority_grounding('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', lineage).
narrative_ontology:cs_interpretation_layer_present('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a').
narrative_ontology:cs_reading_relation('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', geneva_conventions_1949__security_maximization_reading, influences).
narrative_ontology:cs_axiom('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', foundational, protection_conditioned_on_reciprocal_compliance).
narrative_ontology:cs_axiom_status(protection_conditioned_on_reciprocal_compliance, holdable).
narrative_ontology:cs_axiom_grounding('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', protection_conditioned_on_reciprocal_compliance, conventional).
narrative_ontology:cs_axiom('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', secondary, article_4_formal_criteria_as_neutral_gate).
narrative_ontology:cs_axiom_status(article_4_formal_criteria_as_neutral_gate, holdable).
narrative_ontology:cs_axiom_grounding('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', article_4_formal_criteria_as_neutral_gate, instrumental).
narrative_ontology:cs_reference_frame('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', interstate_conventional_war_reciprocity_regime).
narrative_ontology:cs_drift_state('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', post_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1ed4f9ef-ce69-4ebf-9cb1-93de65e5c76a', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, regular_uniformed_combatants).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, detaining_power_legal_apparatus).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, captured_militia_members_denied_pow_status).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, reciprocal_restraint_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, article_4_qualification_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies Article 4 qualification criteria to determine who receives POW protections. Retains legal latitude to classify captured fighters as unlawful combatants when they lack organized command structure, distinctive insignia, or open carriage of arms. Benefits from a reading that narrows its own reciprocal obligations whenever an adversary is judged non-compliant, while retaining the convention's legitimacy claim for its own uniformed forces.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, state_militaries, beneficiary).

% Serve within a recognized command hierarchy, wear distinctive insignia, and carry arms openly — automatically satisfying Article 4 criteria. Receive full POW protection upon capture as a structural consequence of meeting the formal test the reading enforces. Their protection is the convention's clearest success case and the strongest evidence offered for the reading's legitimacy.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, regular_uniformed_combatants, beneficiary,
    organized, immediate, constrained, national).

% Fight without formal command insignia or open-carry norms, often because insurgent or guerrilla warfare structurally cannot meet those criteria without exposing fighters to immediate destruction. Upon capture, classified as unlawful combatants and denied POW status, interrogation protections, and repatriation guarantees. Cannot alter their structural position without abandoning the tactical logic that makes asymmetric resistance possible at all.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_combatants, payer,
    powerless, immediate, trapped, regional).

% Live in zones where the state applies proportionality calculations to determine acceptable collateral harm when countering irregular forces. The reading's narrowed civilian immunity permits higher tolerated collateral damage than the humanitarian-ceiling reading would, on the theory that adversary non-compliance justifies proportional degradation of restraint. Have no institutional voice in how proportionality is calculated and cannot exit the conflict zone in most cases.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_asymmetric_conflict_zones, payer,
    powerless, biographical, trapped, local).

% Detained after capture and processed through a legal apparatus that determines Article 4 compliance retroactively. Bear the direct cost of the reading's central mechanism: denial of POW status converts detention from protected internment into a legally ambiguous status subject to domestic criminal or indefinite security detention. Have essentially no leverage to contest the classification from inside detention.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, captured_militia_members_denied_pow_status, payer,
    powerless, immediate, trapped, national).

% Administers tribunals and status determinations that decide who qualifies as a POW. Operates the discretionary machinery that the conditional-reciprocity reading depends on, and benefits from retaining that discretion rather than ceding it to an unconditional humanitarian floor.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, detaining_power_legal_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, detaining_power_legal_apparatus, beneficiary).

% Advocate for unconditional humanitarian minimums (the sibling humanitarian-ceiling reading) and document harm to detained irregulars and affected civilians, but have no binding authority over the classifying state's Article 4 determinations. Their objections are received as advocacy, not as a legal check on the classification process.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% Analyze how the conditional-reciprocity reading has been applied across conflicts, comparing it to the humanitarian-ceiling and security-maximization readings, and produce jurisprudence and commentary that shapes — but does not control — how states apply the standard.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, legal_scholars_and_tribunals, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a formal, verifiable test (organized command, distinctive insignia, open carriage of arms) that lets opposing regular militaries identify who is bound by combatant norms and therefore entitled to reciprocal protection — solving the genuine problem of distinguishing combatants from civilians in mixed conflict environments.
% TRANSFER_FUNCTION: Moves legal protection and procedural certainty toward regular state militaries that can readily satisfy the formal criteria, and moves the cost of ambiguity — indefinite detention, denial of POW status, expanded collateral-damage tolerance — onto irregular combatants and the civilian populations among whom asymmetric conflict is fought.
% ABSENT_VOICES: Irregular combatants and the civilian populations bearing proportionality-based collateral risk have no seat in the tribunals or command decisions that apply the Article 4 test; international humanitarian organizations raise these concerns but hold no adjudicative authority under this reading.
% DISAPPEARANCE_RATIONALE: If the conditional-reciprocity reading disappeared and states instead operated under an unconditional humanitarian floor, detained irregulars would receive POW protections regardless of Article 4 compliance, proportionality calculations affecting civilians would tighten, and states would lose the discretionary classification apparatus that currently lets them modulate restraint based on adversary conduct.
% FOUNDING_PROBLEM: Post-1949 drafters needed a workable test to distinguish protected combatants from unprotected civilians and to give states an incentive to comply with humanitarian norms by tying protection to reciprocal compliance, in an era dominated by state-to-state conventional warfare.
% FOUNDING_PROBLEM_CORROBORATION: States applying the reading attest the reciprocity logic remains necessary to prevent irregular forces from exploiting protections while non-compliant. Independent legal scholars, the ICRC, and international tribunals outside the benefiting states attest that the founding problem — distinguishing combatants in conventional interstate war — has been substantially superseded by asymmetric and insurgency-dominated conflict, for which the reciprocity test produces systematic under-protection rather than the intended incentive effect.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) reflects a genuinely moderate reading: the reciprocity gate does real coordination work (distinguishing combatants from civilians, incentivizing compliance) but systematically transfers protection away from those least able to satisfy formal criteria — irregular forces fighting materially superior state militaries, for whom open insignia and hierarchical command are often tactically suicidal. Suppression (0.58) is substantial because the classification apparatus is backed by detention authority with limited external review. Theater ratio (0.34) is moderate: much of the tribunal apparatus performs genuine adjudicative function, but a growing share (rising from 0.18 in 1949 to 0.34 by 2025) has become post-hoc justification for detention decisions already made on security grounds — the temporal drift documents Article 4 review hardening from good-faith classification toward compliance theater as asymmetric conflict became the dominant conflict form. Accessibility collapse (0.45) is moderate-low: alternative readings remain genuinely contestable in legal and diplomatic fora, unlike a true mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and the detaining-power legal apparatus sit near the beneficiary end: they retain interpretive discretion over the qualification standard and benefit from a reading that lets them modulate their own obligations based on adversary conduct. Regular uniformed combatants benefit structurally without needing to exercise any discretion — the formal test was built around their operational profile. Irregular combatants, civilians in asymmetric zones, and captured militia members sit near the full-target end: they bear the classification's costs, cannot alter their structural position without sacrificing tactical viability (for irregulars) or relocating (for civilians, often impossible), and have essentially no institutional recourse once classified.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing combatants from civilians to enable reciprocal restraint in interstate conventional war — is genuinely contested as still-live: it retains force in state-vs-state conflict but has been substantially overtaken by asymmetric and insurgency warfare, where the formal test's core assumptions (visible command hierarchy, open-carry norms) no longer track actual battlefield conditions. This is not a simple mandatrophy case (dead problem, live enforcement) but a genealogical mismatch: the same instrument, largely unmodified, now operates in a conflict environment its founding logic did not anticipate, systematically disadvantaging the class of combatant (irregular forces) that has become dominant in contemporary conflict. The tangled_rope classification captures this: real coordination function persists (state militaries do need a workable test), but it now operates with structurally asymmetric extraction that the founding drafters may not have intended as the primary mode of application.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_reading_kernel_indeterminacy,
    'Does the Geneva Conventions kernel itself specify that protections are conditional on adversary reciprocity, or does the conditional-reciprocity reading impose an interpretive gloss the text does not require (Common Article 1''s ''in all circumstances'' language cuts toward the humanitarian-ceiling reading)?',
    'Textual and travaux-préparatoires analysis of Common Article 1 and Article 4 against state practice; comparison with ICRC customary law study conclusions and International Criminal Tribunal jurisprudence on reciprocity defenses.',
    'If the kernel text does not support conditional reciprocity, this reading is better characterized as a state-serving interpretive drift away from an unconditional kernel commitment rather than a legitimate alternative reading — strengthening the case that this reading''s extraction is interpretive capture rather than genuine textual ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_reading_kernel_indeterminacy, conceptual, 'Whether the kernel text itself licenses the conditional-reciprocity reading or whether this reading is an interpretive overlay.').

omega_variable(
    article_4_criteria_tactical_impossibility,
    'Is the Article 4 formal test (organized command, distinctive insignia, open carriage of arms) achievable by irregular forces in genuinely asymmetric conflicts without near-certain immediate destruction, or does the test structurally exclude an entire mode of warfare regardless of the fighters'' underlying conduct?',
    'Empirical study of irregular force survival rates and operational outcomes when attempting to satisfy Article 4 criteria in documented asymmetric conflicts.',
    'If the test is tactically impossible to satisfy for irregulars facing overwhelming state power, the reading''s formal neutrality (the test applies equally to all combatants) is exposed as substantively asymmetric — reinforcing the tangled_rope classification''s victim declaration rather than treating the exclusion as a fair, conduct-based outcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_4_criteria_tactical_impossibility, empirical, 'Whether Article 4 criteria are neutral or structurally exclude irregular combatants regardless of conduct.').

omega_variable(
    reciprocity_reading_kernel_framing_choice,
    'Is the conditional-reciprocity reading better modeled as a genuine sibling reading among three coequal contested framings of the kernel, or as the historically dominant state-practice reading that the humanitarian-ceiling reading has been contesting from a position of comparative institutional weakness?',
    'Survey of which reading dominates actual state tribunal practice versus which reading dominates ICRC commentary and international humanitarian law scholarship, tracked over the interval.',
    'If conditional-reciprocity is the operationally dominant reading despite scholarly preference for humanitarian-ceiling, the extraction measured here may understate the reading''s real-world weight relative to its contested legitimacy — this is a conceptual, not merely descriptive, choice about which framing the SCOPE manifest privileges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_reading_kernel_framing_choice, conceptual, 'Whether this reading should be modeled as coequal with siblings or as the dominant operational reading contested by a weaker rival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.18).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1980, 0.23).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.29).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2010, 0.32).
narrative_ontology:measurement(gene_tr_t2025, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2025, 0.34).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.28).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1965, 0.34).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1980, 0.39).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.47).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(gene_be_t2025, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.35).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1980, 0.44).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.53).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement(gene_su_t2025, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2025, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, security_maximization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the geneva_conventions_1949 kernel, decomposed per the ε-invariance principle because the natural-language label 'the Geneva Conventions' conflates structurally distinct claims with materially different ε values. conditional_reciprocity_reading (this file, ε≈0.52, tangled_rope) sits between humanitarian_ceiling_reading (lower ε, closer to rope — unconditional minimums) and security_maximization_reading (higher ε, closer to snare — suspendable aspirations). Each sibling is authored as its own file with its own beneficiaries, victims, and classification; all three are linked here and in the sibling files via affects_constraints so contamination and comparative analysis can trace the kernel-level contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
