% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Expansive Universalist Reading of Constitutional Equality
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   The expansive universalist reading of the constitutional equality clause
 *   holds that 'all men are created equal' and the Fourteenth Amendment's
 *   'equal protection of the laws' are self-evident universal truths applying
 *   to every human person without exception. Historical exclusions (slavery,
 *   coverture, property qualifications, racial classifications, sexual
 *   orientation barriers) are understood as hypocrisy to be corrected, not as
 *   binding precedent that limits the principle's scope. The reading posits a
 *   low legitimacy threshold for rights expansion via judicial
 *   interpretation: once a group's exclusion is recognized as inconsistent
 *   with the self-evident truth, courts may and must extend protection
 *   without waiting for democratic majorities to authorize it. This
 *   constraint story models the reading as a standing arrangement — the
 *   constitutional order as it operates when this interpretive commitment is
 *   authoritative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.15).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.2).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.15).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, mountain).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading of Constitutional Equality").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).
domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, 'b90811ef-e490-4470-ac22-011d2ed66915').
narrative_ontology:cs_kernel_codification('b90811ef-e490-4470-ac22-011d2ed66915', fixed_text).
narrative_ontology:cs_authority_grounding('b90811ef-e490-4470-ac22-011d2ed66915', lineage).
narrative_ontology:cs_interpretation_layer_present('b90811ef-e490-4470-ac22-011d2ed66915').
narrative_ontology:cs_reading_relation('b90811ef-e490-4470-ac22-011d2ed66915', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('b90811ef-e490-4470-ac22-011d2ed66915', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('b90811ef-e490-4470-ac22-011d2ed66915', foundational, equality_universal_and_self_evident).
narrative_ontology:cs_axiom_status(equality_universal_and_self_evident, holdable).
narrative_ontology:cs_axiom_grounding('b90811ef-e490-4470-ac22-011d2ed66915', equality_universal_and_self_evident, deontological).
narrative_ontology:cs_axiom('b90811ef-e490-4470-ac22-011d2ed66915', foundational, judicial_interpretation_legitimate_expansion).
narrative_ontology:cs_axiom_status(judicial_interpretation_legitimate_expansion, holdable).
narrative_ontology:cs_axiom_grounding('b90811ef-e490-4470-ac22-011d2ed66915', judicial_interpretation_legitimate_expansion, conventional).
narrative_ontology:cs_reference_frame('b90811ef-e490-4470-ac22-011d2ed66915', founding_declaration_universal_promise).
narrative_ontology:cs_drift_state('b90811ef-e490-4470-ac22-011d2ed66915', post_reconstruction_civil_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b90811ef-e490-4470-ac22-011d2ed66915', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, all_humans).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, legislative_majorities).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, universal_human_equality).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, self_evident_truths_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, living_constitution_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Every human person is a rights-bearer under this reading. The constraint's operation extends equal protection and dignity to all without exception. No exit is meaningful because the constraint constitutes the baseline of moral and legal personhood itself.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, all_humans, beneficiary,
    moderate, generational, analytical, universal).

% Groups historically denied full equality (enslaved persons, women, racial minorities, LGBTQ+ persons, non-property-owners) receive the constraint's primary remedial force. Their inclusion is not a concession but a correction of founding hypocrisy. Exit options are constrained because the constraint defines their recognition; leaving the framework means losing the very ground of their claim.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    organized, generational, constrained, global).

% Future persons inherit the universal equality commitment as a settled baseline. They cannot exit the constraint because it structures the moral world they enter. The reading treats their inclusion as logically necessary, not contingent.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Courts are the primary institutional agents that operationalize the expansive reading. They interpret the equality clause to strike down exclusions and expand protected classes. Their authority derives from the claim that the text's self-evident meaning demands universal application. They can move between interpretive modalities (textualism, originalism, living constitutionalism) but remain bound by the institutional role.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, courts_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Democratic majorities see their legislative choices constrained by judicial enforcement of universal equality. From this reading's perspective, this is not extraction but correction: majorities have no legitimate power to restrict the self-evident rights of minorities. The constraint transfers authority to exclude from legislatures to no one. Exit is constrained because the constitutional structure binds them.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, legislative_majorities, payer,
    institutional, biographical, constrained, national).

% Advocates of the restrictive originalist reading (equality limited to 18th-century propertied white males) are structurally excluded from the conversation this reading constitutes. They would object that universal application usurps democratic authority and historical meaning. Their exclusion is not accidental — the reading defines itself against their premise. They can exit by persuading others or capturing courts, but within the expansive framework they have no seat.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, restrictive_originalists, excluded,
    organized, biographical, mobile, national).

% Sibling reading that agrees on universal scope but insists expansion must come through democratic amendment, not judicial reinterpretation. They observe the expansive universalist reading's operation with sympathy for its ends but skepticism about its means. They occupy an analytical seat, neither collecting nor paying under this constraint, but their existence shapes the legitimacy conditions the reading must meet.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, progressive_textualists, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates universal moral recognition and legal protection of equal dignity across all human persons, resolving the coordination problem of who counts as a rights-bearer by declaring the boundary coextensive with humanity itself.
% TRANSFER_FUNCTION: Moves interpretive authority from historical exclusions to universal inclusion; moves legal protection from privileged classes to all persons; moves the burden of justification from the excluded to the would-be excluder.
% ABSENT_VOICES: Restrictive originalists who would limit equality to 18th-century propertied white males; their exclusion from the universal beneficiary set is the point of the reading, not an oversight. Also absent: those who would ground equality in collective or communal membership rather than individual humanity.
% DISAPPEARANCE_RATIONALE: If the expansive universalist reading vanished overnight, constitutional equality would revert to its original restrictive scope. Protections for women, racial minorities, LGBTQ+ persons, non-citizens, and other historically excluded groups would lose their constitutional footing. The legal and moral world would rearrange around a narrower, historically bounded conception of who counts.
% FOUNDING_PROBLEM: The founding problem was the contradiction between the Declaration's 'all men are created equal' and the Constitution's protection of slavery and exclusion of women and non-property-owners from full citizenship. The expansive universalist reading treats this contradiction not as a fixed historical fact but as a mandate: the self-evident truth demands the correction of the hypocrisy.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionists (Frederick Douglass, William Lloyd Garrison), Reconstruction framers (Thaddeus Stevens, Charles Sumner), suffrage movement (Elizabeth Cady Stanton, Susan B. Anthony), civil rights movement (Martin Luther King Jr., Ella Baker), and contemporary human rights advocates all attest the founding problem remains live — the promise of universal equality is still being realized, not yet fulfilled. No corroboration comes from the restrictive originalist camp, which denies the problem's continued existence.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, ExtMetricName, E),
    domain_priors:suppression_score(equality_clause_scope__expansive_universalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the reading frames rights expansion as non-zero-sum recognition, not transfer from some to others. Suppression is low (0.20) because the constraint operates by expanding the circle of inclusion, not by coercing the included. Theater ratio is low (0.10) because the judicial enforcement function is genuine — courts actually strike down exclusions. Accessibility collapse is high (0.85) because accepting the premise (equality is self-evident and universal) logically collapses all exclusionary alternatives. Resistance is moderate (0.40) because restrictive originalist and progressive textualist readings contest the judicial mechanism, creating ongoing interpretive conflict. The claimed_type 'mountain' reflects the reading's own self-understanding: it presents itself as discovering a natural law, not constructing a policy.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (all_humans, historically_excluded_groups, future_generations), the constraint computes as Mountain: it is the natural law of their dignity, requiring no justification. From the legislative_majorities seat, it computes as Tangled Rope or Snare: a genuine coordination function (universal rights) married to asymmetric extraction (judicial removal of democratic authority). From the courts seat, it computes as Rope: a coordination mechanism they administer. The engine computes this divergence; the authored claim (mountain) reflects the reading's self-presentation, not a reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   All humans, historically excluded groups, and future generations are beneficiaries (d near 0.0) — the constraint subsidizes their recognition. Courts are agenda_setters with arbitrage-grade exit (d ~ 0.1) — they control interpretation but can shift modalities. Legislative majorities are payers (d ~ 0.7) — their power to exclude is structurally removed, which this reading frames as correction not extraction. Restrictive originalists are excluded (no d computed) — they are not coordinated by this constraint; their premise is foreclosed. Progressive textualists are observers (d ~ 0.5) — they share the beneficiary set but contest the mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading prevents mislabeling coordination as extraction by insisting the 'transfer' is not a transfer at all — recognition of equal dignity is not a resource taken from one group and given to another, but a truth acknowledged. The mandate (correct founding hypocrisy) remains live because exclusions persist in new forms. The reading would become a piton if universal inclusion were achieved and courts continued expanding 'equality' into domains where the self-evident truth has no clear application (theater without function). Currently, the mandate is live and the function genuine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_universality,
    'Is universal human equality a genuine natural law (Mountain) or a constructed normative achievement that requires continuous institutional maintenance (Rope/Tangled Rope)?',
    'Cross-cultural and historical analysis: if the universal equality claim emerges independently across disconnected moral traditions, evidence for natural law; if it appears only in specific historical lineages dependent on institutional enforcement, evidence for construction.',
    'If natural law, the reading''s Mountain claim holds and FSM does not trigger. If constructed, the declared beneficiaries (all_humans) reveal the constraint as a false summit — a coordination/extraction hybrid (Tangled Rope) presenting as natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_universality, conceptual, 'Whether the self-evident truth claim describes a mind-independent moral fact or a successful normative construction.').

omega_variable(
    judicial_legitimacy_threshold,
    'Is the low legitimacy threshold for judicial rights expansion structurally stable, or does it invite counter-majoritarian backlash that eventually collapses the reading''s authority?',
    'Longitudinal study of court legitimacy metrics before/after major rights expansions (Brown, Roe, Obergefell) correlated with subsequent democratic pushback (constitutional amendments, court-packing threats, jurisdiction stripping).',
    'If unstable, the reading''s enforcement mechanism (judicial review) degrades, pushing the constraint toward Piton (theatrical maintenance of lost authority) or Snare (coercive imposition without legitimacy). If stable, the Mountain/Rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_legitimacy_threshold, empirical, 'Whether the reading''s institutional vehicle (judicial interpretation) can sustain its low-threshold expansionism without losing the legitimacy that makes it effective.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the expansive universalist reading genuinely foreclose the restrictive_originalist reading within a single constitutional framework, or do they coexist as competing interpretive regimes?',
    'Analyze whether any coherent legal framework can simultaneously hold: (a) equality is self-evidently universal, and (b) the Constitution''s original meaning restricts equality to propertied white males. If no framework can hold both without contradiction, foreclosure is genuine; if practitioners routinely switch or blend, coexistence is real.',
    'If genuine foreclosure, the cs_structure relation ''forecloses'' is correct and the kernel has a structural fault line. If coexistence, the relation should be ''coexists_with'' and the kernel''s dispute is political not logical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the logical structure of the readings admits a single framework holding both, or forces a choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t0, equality_clause_scope__expansive_universalist, theater_ratio, 0, 0.05).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t50, equality_clause_scope__expansive_universalist, theater_ratio, 50, 0.08).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t90, equality_clause_scope__expansive_universalist, theater_ratio, 90, 0.1).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t130, equality_clause_scope__expansive_universalist, theater_ratio, 130, 0.1).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t170, equality_clause_scope__expansive_universalist, theater_ratio, 170, 0.1).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t210, equality_clause_scope__expansive_universalist, theater_ratio, 210, 0.1).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_tr_t250, equality_clause_scope__expansive_universalist, theater_ratio, 250, 0.1).

% Extraction over time
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t0, equality_clause_scope__expansive_universalist, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t50, equality_clause_scope__expansive_universalist, base_extractiveness, 50, 0.08).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t90, equality_clause_scope__expansive_universalist, base_extractiveness, 90, 0.12).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t130, equality_clause_scope__expansive_universalist, base_extractiveness, 130, 0.14).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t170, equality_clause_scope__expansive_universalist, base_extractiveness, 170, 0.15).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t210, equality_clause_scope__expansive_universalist, base_extractiveness, 210, 0.15).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_be_t250, equality_clause_scope__expansive_universalist, base_extractiveness, 250, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t0, equality_clause_scope__expansive_universalist, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t50, equality_clause_scope__expansive_universalist, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t90, equality_clause_scope__expansive_universalist, suppression_requirement, 90, 0.2).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t130, equality_clause_scope__expansive_universalist, suppression_requirement, 130, 0.2).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t170, equality_clause_scope__expansive_universalist, suppression_requirement, 170, 0.2).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t210, equality_clause_scope__expansive_universalist, suppression_requirement, 210, 0.2).
narrative_ontology:measurement(equality_clause_scope__expansive_universalist_su_t250, equality_clause_scope__expansive_universalist, suppression_requirement, 250, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:boltzmann_floor_override(equality_clause_scope__expansive_universalist, 0.08).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).

% DUAL FORMULATION NOTE:
% This story is the expansive_universalist member of the equality_clause_scope constraint family. The kernel decomposes into three readings with distinct ε values: expansive_universalist (low ε, claims Mountain), progressive_textualist (moderate ε, claims Rope/Scaffold), restrictive_originalist (moderate ε, claims Mountain but with declared beneficiaries triggering FSM). The ε-invariance principle requires separate stories because the standing arrangement's extractiveness differs radically depending on which reading's lights you assess it by.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
