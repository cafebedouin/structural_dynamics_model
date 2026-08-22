% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Applicability Threshold (State-Centric Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This story instantiates the state-centric reading of the Common Article 3
 *   scope kernel: CA3's minimum humanitarian protections apply only once a
 *   confrontation crosses judicially and doctrinally elaborated intensity and
 *   organization thresholds (following, roughly, the Tadic factors). Below
 *   that line, states characterize violence as law enforcement rather than
 *   armed conflict, retaining full discretion over use of force and
 *   detention. The coordination function is genuine — a workable line between
 *   IHL and domestic policing is administratively necessary — but the
 *   threshold's location and its unilateral, state-controlled application
 *   produce asymmetric extraction: irregular fighters and civilians in
 *   ambiguous violence lose guaranteed protections while security
 *   institutions gain operational flexibility. This is one of three linked
 *   constraints on the same kernel; the expansive_human_rights_reading and
 *   icrc_customary_reading instantiate the same textual commitment (CA3's
 *   applicability) with materially different beneficiary/victim structures
 *   and different ε values, because each reading answers a different question
 *   about who controls the classification decision.
 *
 * KEY AGENTS:
 *   - state_militaries: primary agenda-setter and beneficiary (institutional/arbitrage) — sets and applies the threshold test
 *   - national_security_ministries: doctrinal beneficiary (institutional/arbitrage) — shapes legal opinions defining the bar
 *   - irregular_combatants_below_threshold: primary target (powerless/trapped) — loses CA3 protections if classified sub-threshold
 *   - civilians_in_unclassified_violence: secondary target (powerless/trapped) — loses humanitarian access leverage
 *   - humanitarian_organizations: excluded advocate (organized/constrained) — argues for lower thresholds, no binding say
 *   - international_courts_and_tribunals: analytical observer (institutional/analytical) — adjudicates after the fact, often years later
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.81).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Applicability Threshold (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '94e3d165-1a8c-409e-8384-daa5946d0720').
narrative_ontology:cs_kernel_codification('94e3d165-1a8c-409e-8384-daa5946d0720', fixed_text).
narrative_ontology:cs_authority_grounding('94e3d165-1a8c-409e-8384-daa5946d0720', lineage).
narrative_ontology:cs_interpretation_layer_present('94e3d165-1a8c-409e-8384-daa5946d0720').
narrative_ontology:cs_reading_relation('94e3d165-1a8c-409e-8384-daa5946d0720', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('94e3d165-1a8c-409e-8384-daa5946d0720', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('94e3d165-1a8c-409e-8384-daa5946d0720', foundational, sovereign_discretion_over_conflict_classification).
narrative_ontology:cs_axiom_status(sovereign_discretion_over_conflict_classification, holdable).
narrative_ontology:cs_axiom_grounding('94e3d165-1a8c-409e-8384-daa5946d0720', sovereign_discretion_over_conflict_classification, conventional).
narrative_ontology:cs_axiom('94e3d165-1a8c-409e-8384-daa5946d0720', foundational, threshold_gate_necessary_to_preserve_domestic_policing_authority).
narrative_ontology:cs_axiom_status(threshold_gate_necessary_to_preserve_domestic_policing_authority, holdable).
narrative_ontology:cs_axiom_grounding('94e3d165-1a8c-409e-8384-daa5946d0720', threshold_gate_necessary_to_preserve_domestic_policing_authority, instrumental).
narrative_ontology:cs_reference_frame('94e3d165-1a8c-409e-8384-daa5946d0720', id_1949_diplomatic_conference_sovereignty_compromise).
narrative_ontology:cs_drift_state('94e3d165-1a8c-409e-8384-daa5946d0720', post_war_on_terror_classification_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('94e3d165-1a8c-409e-8384-daa5946d0720', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_militaries).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, national_security_ministries).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_unclassified_violence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines, in the first instance, whether a given confrontation crosses the intensity and organization thresholds that trigger CA3 obligations. Retains operating discretion to treat sub-threshold violence as ordinary law enforcement, applying domestic criminal law and use-of-force rules instead of the laws of armed conflict. Benefits directly from the narrower classification because it constrains fewer operations and avoids obligations toward detained fighters, wounded persons, and populations in contested areas.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_militaries, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, state_militaries, beneficiary).

% Draft the doctrine and legal opinions that set the intensity/organization bar. A higher bar means fewer situations are legally 'armed conflict,' which preserves policy space for counterinsurgency, policing powers, and detention regimes outside IHL's minimum guarantees.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, national_security_ministries, beneficiary,
    institutional, generational, arbitrage, national).

% Fighters in loosely organized or lower-intensity armed groups fall outside CA3's protections under this reading. If captured, they receive no guaranteed minimum treatment standard tied to CA3 (no prohibition on cruel treatment, no guarantee of judicial guarantees) unless separately covered by domestic or human rights law, both of which the state controls and can suspend under emergency powers. They have no forum to contest the classification decision before it is applied to them.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, regional).

% Civilians caught in violence that a state characterizes as below the CA3 threshold lose the interpretive leverage CA3 would give humanitarian actors to demand access, protection, and accountability. They bear the practical cost of the classification dispute without any voice in how it is resolved.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_unclassified_violence, payer,
    powerless, immediate, trapped, regional).

% ICRC and similar bodies argue for a lower, protection-maximizing threshold and seek access based on functional need rather than formal classification. Under the state-centric reading their assessments carry persuasive but non-binding weight; the state's classification controls access negotiations and legal characterization.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, humanitarian_organizations, excluded,
    organized, biographical, constrained, global).

% Adjudicate individual cases and may apply differing intensity/organization tests (e.g., Tadic factors) after the fact, sometimes years after the relevant violence, which can validate or reject a state's contemporaneous classification but does not change the operational discretion exercised at the time.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, state_militaries).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a workable, administrable line between situations governed by the laws of armed conflict and situations governed by ordinary domestic law, so that states and courts are not required to apply full IHL machinery to every riot, gang conflict, or isolated terrorist attack.
% TRANSFER_FUNCTION: Moves protective legal obligations away from states and toward the classifying authority's discretion: the benefit of avoided IHL obligations (operational flexibility, narrower detainee protections, unconstrained use of force below the threshold) flows to security ministries and militaries; the cost (loss of guaranteed minimum humane treatment, no independent forum) is borne by persons caught on the wrong side of the threshold determination.
% ABSENT_VOICES: Irregular combatants and affected civilians have no procedural role in the threshold determination — it is made unilaterally by the state actor with the strongest incentive to keep the bar high. Humanitarian organizations advocate for lower thresholds but their assessments are advisory only under this reading.
% DISAPPEARANCE_RATIONALE: If the intensity/organization threshold test disappeared and any organized armed violence triggered CA3 automatically (the expansive reading), states would lose the discretion to characterize confrontations as ordinary policing, detained persons in sub-threshold conflicts would gain guaranteed minimum treatment standards, and humanitarian access negotiations would shift from state-controlled classification disputes to needs-based triggers.
% FOUNDING_PROBLEM: Common Article 3 was drafted to extend minimal humanitarian protections into non-international armed conflicts without requiring recognition of belligerency or full application of the Geneva Conventions, while preserving state sovereignty concerns that made states reluctant to characterize internal unrest as 'war.'
% FOUNDING_PROBLEM_CORROBORATION: States and military legal advisors attest the threshold remains necessary to prevent IHL from displacing ordinary policing in low-level unrest. The ICRC's own commentary, independent IHL scholars, and international tribunals applying the Tadic intensity/organization test from outside any state's benefiting interest attest that the threshold as operationally applied by states is frequently set higher than the customary law test warrants, and functions in practice to delay or deny protection rather than merely distinguish conflict from crime.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high-moderate (0.68 by 2024) because the threshold, as operationally applied, has drifted toward requiring more sustained, more organized violence than the founding customary-law test contemplated — a trend documented by tribunal decisions correcting state classifications after the fact. Suppression is authored higher still (0.81) because the classification decision is made unilaterally, ex ante, by the party with the strongest interest in a narrow reading, and there is no contemporaneous forum for affected persons to contest it. Theater ratio (0.42) reflects that a meaningful share of the doctrinal apparatus around 'intensity and organization' functions to perform legal rigor around decisions substantially driven by policy convenience. Accessibility collapse (0.6) is moderate rather than extreme because customary law and treaty-body jurisprudence still exist as partial correctives; resistance (0.58) reflects sustained pushback from the ICRC, human rights bodies, and international courts.
 *
 * PERSPECTIVAL GAP:
 *   From the state agenda-setter seat, the threshold test is a defensible coordination device preventing IHL from swallowing ordinary policing. From the payer seats — irregular combatants and civilians — the same threshold operates as an enforced exclusion mechanism controlled entirely by the party benefiting from narrow classification. The engine should compute a tangled_rope or snare-leaning classification from the payer seat's structural position even though the agenda_setter's own framing would compute closer to a rope; this divergence is the intended signal, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries and security ministries are declared beneficiaries because the narrower classification directly expands their operational discretion and reduces their compliance burden — this maps to low d. Irregular combatants below threshold and civilians in unclassified violence are declared victims because they lose the concrete protective floor CA3 would otherwise guarantee, and they are trapped (no exit from the classifying jurisdiction, no procedural standing) — this maps to high d, amplified by their powerless power atom and immediate time horizon. Humanitarian organizations are excluded rather than victimized directly, since their institutional interest is advocacy rather than personal exposure to the violence.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — distinguishing genuine armed conflict from ordinary unrest to calibrate the correct legal regime — remains partially live (some sub-threshold violence really is ordinary crime warranting only domestic law). But the corroboration record shows the threshold's operational application has drifted from that founding function toward a discretionary shield: tribunals applying the Tadic factors independently of state interest have repeatedly found CA3 applicable to situations states classified as sub-threshold. This is not full mandatrophy (the problem is not dead) but a documented capture-drift pattern — the founding_problem_status is authored as contested rather than dead precisely because state and independent-tribunal corroboration diverge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_legitimacy,
    'Is the intensity/organization threshold, as operationally applied by states under this reading, a good-faith administrable line consistent with customary international law, or has it drifted into a discretionary shield that exceeds what the Tadic factors and state practice actually support?',
    'Comparative analysis of state classification decisions against subsequent international tribunal rulings (ICTY, ICC, regional human rights courts) applying the same intensity/organization factors independently; a persistent pattern of tribunals finding CA3 applicable where states denied it would indicate drift.',
    'If drift is established, the state-centric reading''s claimed coordination function is substantially cover for extraction, supporting reclassification toward snare; if the threshold tracks tribunal findings closely, the tangled_rope classification (genuine coordination plus contested application) is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_legitimacy, empirical, 'Whether the operationally applied threshold exceeds the customary-law baseline it claims to implement.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This story is one reading (state_centric_reading) of the common_article_3_scope kernel. The sibling readings — expansive_human_rights_reading and icrc_customary_reading — locate applicability control differently: the expansive reading removes the threshold gate entirely in favor of a protection floor triggered by any organized violence; the customary reading locates the threshold in evolving multilateral state practice rather than unilateral state classification. Where exactly does the disagreement sit?',
    'The disagreement is located at a single structural element: WHO controls the applicability determination and WHEN it is made — unilaterally and prospectively by the state involved (this reading) vs. universally and floor-setting regardless of classification (expansive reading) vs. collectively through accumulated state practice assessed after the fact (customary reading). A sibling reading adopting the expansive premise would eliminate the excluded victim class entirely (irregular_combatants_below_threshold ceases to exist as a category), which this reading''s core premise (that a threshold is a legitimate and necessary gate) directly contests.',
    'If the expansive reading were adopted as the governing interpretation, this story''s declared victim class and its associated extraction and suppression values would not exist — the constraint modeled here would be replaced entirely, not merely adjusted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locates the inter-reading disagreement in control-of-classification and timing, not in the underlying textual commitment.').

omega_variable(
    beneficiary_capture_vs_sovereignty_interest,
    'Is the state''s benefit from narrow CA3 applicability best understood as illegitimate capture of a humanitarian instrument for operational convenience, or as a legitimate sovereignty interest the treaty drafters intentionally preserved in 1949?',
    'Historical analysis of the 1949 Diplomatic Conference travaux préparatoires: did drafters intend a high, state-controlled threshold, or a low, protection-maximizing one that states have since eroded through practice?',
    'If the drafters intended a high threshold, the current state-centric application is closer to fidelity than drift, weakening the extraction framing; if drafters intended broader protection eroded by subsequent state practice, the extraction framing is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_vs_sovereignty_interest, conceptual, 'Whether narrow state control over CA3 applicability reflects original treaty intent or subsequent erosion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__state_centric_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__state_centric_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__state_centric_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(comm_tr_t2001, common_article_3_scope__state_centric_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__state_centric_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__state_centric_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__state_centric_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__state_centric_reading, base_extractiveness, 1977, 0.45).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__state_centric_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(comm_be_t2001, common_article_3_scope__state_centric_reading, base_extractiveness, 2001, 0.6).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__state_centric_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__state_centric_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__state_centric_reading, suppression_requirement, 1949, 0.55).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__state_centric_reading, suppression_requirement, 1977, 0.62).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__state_centric_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(comm_su_t2001, common_article_3_scope__state_centric_reading, suppression_requirement, 2001, 0.74).
narrative_ontology:measurement(comm_su_t2010, common_article_3_scope__state_centric_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__state_centric_reading, suppression_requirement, 2024, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This constraint, expansive_human_rights_reading, and icrc_customary_reading are three readings of the single common_article_3_scope kernel (the textual commitment that CA3 governs non-international armed conflicts). Each reading answers 'who controls the applicability determination and when' differently, producing different beneficiary/victim structures and different ε. This story (state_centric_reading) authors the highest suppression and the narrowest victim-protective scope of the three; expansive_human_rights_reading is expected to author near-zero suppression of application and no excluded sub-threshold victim class; icrc_customary_reading is expected to sit between the two, gated by accumulated state practice rather than unilateral state discretion or a universal floor.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
