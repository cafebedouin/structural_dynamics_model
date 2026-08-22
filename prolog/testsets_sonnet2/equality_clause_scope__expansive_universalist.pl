% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Equality Clause — Expansive Universalist Reading
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates the expansive universalist reading of the
 *   equality clause kernel: the claim that equality was always a self-evident
 *   universal truth, that historical exclusions (of enslaved people, women,
 *   non-property-holders) were hypocritical departures from the clause's own
 *   terms rather than evidence of its intended narrow scope, and that courts
 *   may correct this hypocrisy through interpretation without waiting for
 *   legislative amendment. This is a Tangled Rope: it genuinely coordinates
 *   rights-recognition around a single textual anchor (avoiding the need for
 *   separate amendments for every excluded group) while also transferring
 *   authority from legislatures to courts and imposing transition costs on
 *   parties who built arrangements around the previously settled, narrower
 *   scope. The sibling readings — restrictive_originalist (equality bound to
 *   18th-century propertied-white-male social contract) and
 *   progressive_textualist (equality principle exists in text but expands
 *   only through amendment, not judicial reinterpretation) — are separate
 *   constraint stories with their own ε and stakeholder sets, not alternative
 *   measurements of this one.
 *
 * KEY AGENTS:
 *   - historically_excluded_groups: primary beneficiaries of judicial inclusion under this reading
 *   - constitutional_courts: agenda-setting institution exercising low-threshold interpretive authority
 *   - settled_expectation_holders: bear the transition costs of judicially-driven scope expansion
 *   - legislative_majorities: displaced from their preferred forum for resolving scope questions
 *   - originalist_jurists: excluded from controlling the outcome, present only as dissenting voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.42).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.38).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.42).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Equality Clause — Expansive Universalist Reading").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '6d79a440-ca8a-4ade-a540-25a20f82c554').
narrative_ontology:cs_kernel_codification('6d79a440-ca8a-4ade-a540-25a20f82c554', fixed_text).
narrative_ontology:cs_authority_grounding('6d79a440-ca8a-4ade-a540-25a20f82c554', lineage).
narrative_ontology:cs_interpretation_layer_present('6d79a440-ca8a-4ade-a540-25a20f82c554').
narrative_ontology:cs_reading_relation('6d79a440-ca8a-4ade-a540-25a20f82c554', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('6d79a440-ca8a-4ade-a540-25a20f82c554', equality_clause_scope__progressive_textualist, influences).
narrative_ontology:cs_axiom('6d79a440-ca8a-4ade-a540-25a20f82c554', foundational, equality_principle_is_self_evident_and_universal).
narrative_ontology:cs_axiom_status(equality_principle_is_self_evident_and_universal, holdable).
narrative_ontology:cs_axiom_grounding('6d79a440-ca8a-4ade-a540-25a20f82c554', equality_principle_is_self_evident_and_universal, deontological).
narrative_ontology:cs_axiom('6d79a440-ca8a-4ade-a540-25a20f82c554', foundational, judicial_interpretation_is_legitimate_vehicle_for_scope_correction).
narrative_ontology:cs_axiom_status(judicial_interpretation_is_legitimate_vehicle_for_scope_correction, holdable).
narrative_ontology:cs_axiom_grounding('6d79a440-ca8a-4ade-a540-25a20f82c554', judicial_interpretation_is_legitimate_vehicle_for_scope_correction, conventional).
narrative_ontology:cs_reference_frame('6d79a440-ca8a-4ade-a540-25a20f82c554', universal_moral_personhood_at_founding).
narrative_ontology:cs_drift_state('6d79a440-ca8a-4ade-a540-25a20f82c554', contemporary_rights_jurisprudence, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('6d79a440-ca8a-4ade-a540-25a20f82c554', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, rights_expansion_litigants).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, constitutional_courts).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, settled_expectation_holders).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, legislative_majorities).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, moral_universality_of_personhood).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, constitutional_aspiration_over_original_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Were categorically excluded from the equality guarantee's original operation (enslaved people, women, non-property-holders, racial minorities). Under this reading, the text's universal language was always the true commitment and the historical exclusion was hypocrisy — a betrayal of the clause's own terms, not evidence of its narrower original scope. They benefit directly when courts read the clause to include them without waiting for amendment.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    powerless, generational, trapped, national).

% Bring test cases asking courts to extend equal-protection reasoning to new classifications (sexual orientation, disability, immigration status) by analogy to the clause's underlying universal principle. Their strategy depends entirely on courts accepting that the text's plain universal language controls over historical practice.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, rights_expansion_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Interpret the equality clause's scope through judicial reasoning rather than deferring to either the amendment process or 18th-century social contract limits. Under this reading they hold low-threshold authority to expand the beneficiary class whenever they find the text's universal language compels it, which enlarges judicial power over who counts as a rights-holder.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, constitutional_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Built institutions, contracts, and social arrangements around the clause's previously settled (narrower) scope — employers with seniority systems calibrated to prior classifications, states with statutory schemes premised on earlier equal-protection boundaries. When courts expand the clause's reach, their reliance interests are overridden without a vote; they bear the transition cost of a reinterpretation they did not choose.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, settled_expectation_holders, payer,
    organized, biographical, constrained, national).

% Represent constituencies who might have preferred to resolve scope questions through statute or amendment, preserving democratic control over pace and terms. Under the expansive universalist reading, courts can settle the scope question first, converting a live legislative choice into a constitutional floor and displacing the majoritarian process before it acts.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, legislative_majorities, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, legislative_majorities, excluded).

% Hold that the clause's scope is fixed by its ratification-era understanding and that expansion belongs to the amendment process, not judicial reinterpretation. Their framework is treated by this reading as having mistaken a moral failure (exclusion) for a legal boundary (original meaning); they are structurally present in the debate but this reading's operation proceeds without needing to persuade them.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, originalist_jurists, excluded,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__expansive_universalist, diffuse).
narrative_ontology:fixing_cost_class(equality_clause_scope__expansive_universalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single constitutional principle courts can invoke to resolve equal-treatment disputes without requiring each excluded group to win a separate constitutional amendment, coordinating rights recognition around one textual anchor.
% TRANSFER_FUNCTION: Moves recognition, legal standing, and the practical benefits of protected-class status from groups whose exclusion was previously treated as settled toward newly included groups, and shifts the authority to decide who is included from legislatures to courts.
% ABSENT_VOICES: Originalist jurists and legislative majorities who would prefer the amendment process are present in doctrinal debate but structurally bypassed once a court accepts the universalist premise — their objection is that the decision was made in the wrong forum, not necessarily against the substantive outcome.
% DISAPPEARANCE_RATIONALE: If courts abandoned the universalist reading overnight, pending and future expansions of equal-protection scope would revert to the amendment process; groups relying on judicial extension of the clause (e.g., through analogy-based reasoning) would lose their fastest and often only viable path to recognition, and litigation strategy across civil rights law would have to shift toward legislatures.
% FOUNDING_PROBLEM: The equality clause was adopted after a war fought substantially over whether the nation's founding equality language actually meant what it said; the founding problem was reconciling a stated universal principle with a legal and social order that had systematically excluded most of the population from it.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Reconstruction Congress attest that framers of the equality amendment intended broader application than prior practice, supporting the universalist reading from outside current litigants. Originalist legal scholars and some legislative historians dispute the corroboration, arguing contemporaneous debates show a narrower intended scope than the universalist reading claims — the corroboration itself is contested, not merely the conclusion drawn from it.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).
:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the reading's dominant effect is inclusion, not extraction in the conventional rent-seeking sense — but it is non-trivial because reliance interests of settled_expectation_holders and the institutional prerogative of legislative_majorities are overridden without their consent, which is a real transfer even if morally motivated. Suppression is moderate (0.38) and falls over time (0.5 to 0.38) as the universalist reading becomes normalized in doctrine and meets less active resistance from courts and legislatures that adopt it as settled law. Theater ratio stays low (0.22 by t=60) because the coordination function (a single textual anchor for rights claims) remains substantively operative rather than becoming performative. Accessibility collapse is low (0.3) because alternative interpretive frameworks (originalist, textualist-amendment) remain live and contested — this is precisely why the constraint is authored as a kernel reading rather than settled fact.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of historically_excluded_groups and rights_expansion_litigants, the constraint operates as long-overdue correction of hypocrisy — a Rope-like unlocking of a promise always textually present. From the seat of settled_expectation_holders and legislative_majorities, the same structural move operates as an imposed reallocation of authority and reliance costs without their consent — a Tangled-Rope-like extraction riding on the coordination story. The engine computes these divergent seat classifications from the same structural data; this story does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically_excluded_groups and rights_expansion_litigants are beneficiaries with derived low d — the constraint (as read this way) subsidizes their inclusion. Constitutional_courts sit as agenda_setter with institutional power and arbitrage-like exit (they can select which cases to hear and how broadly to reason), giving them a directionality that reflects control rather than either subsidy or extraction. Settled_expectation_holders and legislative_majorities are payers: their reliance interests and institutional prerogatives are the cost side of the same interpretive move that benefits the included groups. Originalist_jurists are excluded rather than victimized in the transfer sense — their objection is procedural (wrong forum) rather than about being extracted from directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling a stated universal equality principle with a historically exclusionary practice) is authored as contested rather than resolved: the universalist reading holds the problem remains live wherever any group is excluded from the clause's operation, while originalist and textualist readings would hold the problem was resolved by specific amendments and further expansion requires further amendments. Because founding_problem_status is contested rather than dead, this reading does not present as a zombie mandate riding on inertia — it presents as an active, contested interpretive claim. This blocks the temptation to either fully valorize the expansion as pure coordination (ignoring the real transfer to settled_expectation_holders and legislative_majorities) or dismiss it as pure judicial overreach (ignoring the genuine coordination value of a single textual anchor for rights claims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_equality_clause,
    'Is the expansive_universalist reading of the equality clause the structurally correct account of the clause''s original commitment, or is it a retrospective moral gloss placed on a text whose ratifiers held a narrower understanding?',
    'This is not resolvable by this story alone — it is one of three declared readings of the equality_clause_scope kernel (expansive_universalist, restrictive_originalist, progressive_textualist), each authored as a separate constraint with its own ε, beneficiaries, and victims. Resolution would require independent historical-textual analysis of ratification-era intent and semantic scope, which the corpus does not adjudicate at the level of a single reading.',
    'If the restrictive_originalist reading is structurally correct, this constraint''s claim that historical exclusions are ''hypocrisy'' rather than ''binding original scope'' is itself an extractive reframing that manufactures moral high ground for a judicial power grab. If the progressive_textualist reading is correct, this reading''s low threshold for judicial expansion (versus amendment) is the extractive element, not the universalist premise itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_equality_clause, conceptual, 'Which of three sibling kernel readings correctly characterizes the equality clause''s original and legitimate scope.').

omega_variable(
    judicial_versus_amendment_legitimacy,
    'Does low-threshold judicial interpretation carry legitimate authority to expand constitutional rights scope, or does that authority properly belong exclusively to the amendment process?',
    'Comparative institutional analysis of how amendment-driven versus interpretation-driven rights expansions have performed on measures of durability, democratic legitimacy, and downstream backlash (e.g., contrasting judicially-driven expansions later entrenched by later amendment/statute against those that provoked sustained political conflict).',
    'If judicial interpretation is found to systematically produce less durable and more contested outcomes than amendment, that would support reclassifying this reading''s coordination claim as weaker than claimed, shifting the balance toward snare; if judicially-driven expansions prove as durable as amendment-driven ones, the tangled_rope classification''s coordination side is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_versus_amendment_legitimacy, preference, 'Whether judicial interpretation is a legitimate vehicle for constitutional scope expansion relative to the amendment process.').

omega_variable(
    reliance_cost_magnitude,
    'How large, in practice, are the reliance costs actually borne by settled_expectation_holders when courts expand equal-protection scope, versus the benefit gained by newly included groups?',
    'Empirical study of transition costs following major scope-expansion rulings (compliance costs, litigation costs, disrupted institutional arrangements) compared against measurable gains in inclusion (employment, access, recognition) for newly covered groups.',
    'If reliance costs are small relative to inclusion gains, the extractiveness score authored here (0.42) may be too high; if reliance costs are large and concentrated, the score may understate the transfer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reliance_cost_magnitude, empirical, 'The empirical magnitude of transition costs imposed on parties reliant on prior narrower scope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.15).
narrative_ontology:measurement(equa_tr_t10, equality_clause_scope__expansive_universalist, theater_ratio, 10, 0.16).
narrative_ontology:measurement(equa_tr_t20, equality_clause_scope__expansive_universalist, theater_ratio, 20, 0.18).
narrative_ontology:measurement(equa_tr_t30, equality_clause_scope__expansive_universalist, theater_ratio, 30, 0.19).
narrative_ontology:measurement(equa_tr_t40, equality_clause_scope__expansive_universalist, theater_ratio, 40, 0.2).
narrative_ontology:measurement(equa_tr_t50, equality_clause_scope__expansive_universalist, theater_ratio, 50, 0.21).
narrative_ontology:measurement(equa_tr_t60, equality_clause_scope__expansive_universalist, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(equa_be_t10, equality_clause_scope__expansive_universalist, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(equa_be_t20, equality_clause_scope__expansive_universalist, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(equa_be_t30, equality_clause_scope__expansive_universalist, base_extractiveness, 30, 0.37).
narrative_ontology:measurement(equa_be_t40, equality_clause_scope__expansive_universalist, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(equa_be_t50, equality_clause_scope__expansive_universalist, base_extractiveness, 50, 0.41).
narrative_ontology:measurement(equa_be_t60, equality_clause_scope__expansive_universalist, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(equa_su_t10, equality_clause_scope__expansive_universalist, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(equa_su_t20, equality_clause_scope__expansive_universalist, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(equa_su_t30, equality_clause_scope__expansive_universalist, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(equa_su_t40, equality_clause_scope__expansive_universalist, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(equa_su_t50, equality_clause_scope__expansive_universalist, suppression_requirement, 50, 0.39).
narrative_ontology:measurement(equa_su_t60, equality_clause_scope__expansive_universalist, suppression_requirement, 60, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__expansive_universalist, 0.12).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the equality_clause_scope kernel. restrictive_originalist ties the clause's scope to 18th-century propertied-white-male social contract terms (near-zero universal beneficiary set, high suppression of expansion claims). progressive_textualist accepts the universal principle textually but routes all scope expansion through amendment rather than judicial interpretation (moderate extraction, lower judicial agenda-setting power). This story (expansive_universalist) authors the highest judicial agenda-setting power and the widest beneficiary set of the three, with extraction concentrated in the transfer of interpretive authority from legislatures to courts and the override of settled reliance interests. Each reading carries a distinct ε assessed by its own lights, per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
