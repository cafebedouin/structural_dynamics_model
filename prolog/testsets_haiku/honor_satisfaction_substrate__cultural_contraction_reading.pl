% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_substrate__cultural_contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_substrate__cultural_contraction_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_satisfaction_substrate__cultural_contraction_reading
 *   human_readable: Honor Code as Interpretive Substrate (Cultural Contraction Reading)
 *   domain: social/cultural/historical
 *
 * SUMMARY:
 *   This reading instantiates the honor code itself as a mountain constraint
 *   whose substrate eroded when the organizing cosmology shifted from honor
 *   (a contingent, acquired, status-dependent worth that must be continuously
 *   defended through ritualized violence) to dignity (an intrinsic,
 *   inalienable, status-independent worth that cannot be stained by insult or
 *   restored by violence). The decline of dueling was not suppression
 *   overcoming a persistent cultural form; it was the cognitive impossibility
 *   of the form once its interpretive substrate disintegrated. This reading
 *   is one of three sibling readings of the same kernel (the contested claim
 *   about why dueling declined). It coexists with the
 *   practice_decline_reading (which treats the honor code as persisting and
 *   dueling as suppressed) and influences the
 *   composite_overdetermined_reading (which treats suppression and
 *   delegitimation as simultaneous).
 *
 * KEY AGENTS:
 *   - dueling_practitioners: Elite men whose identity was honor-constituted; faced identity-lock (exit meant self-annihilation, not mere inconvenience) as the substrate eroded
 *   - honor_code_keepers: Military hierarchies, aristocratic circles, professional guilds that transmitted the code; lost interpretive authority as dignity cosmology rose
 *   - dignity_advocates: Enlightenment philosophers and institutional reformers whose articulation of an alternative personhood cosmology made honor-satisfaction unthinkable
 *   - legal_prohibition_enforcer: State legal systems that criminalized dueling; this reading holds prohibition was downstream effect, not upstream cause
 *   - anthropological_observer: The analytical seat recording the substrate shift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_substrate__cultural_contraction_reading, 0.15).
domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).
domain_priors:theater_ratio(honor_satisfaction_substrate__cultural_contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_substrate__cultural_contraction_reading, mountain).
narrative_ontology:human_readable(honor_satisfaction_substrate__cultural_contraction_reading, "Honor Code as Interpretive Substrate (Cultural Contraction Reading)").
narrative_ontology:topic_domain(honor_satisfaction_substrate__cultural_contraction_reading, "social/cultural/historical").

domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_substrate__cultural_contraction_reading, 'c9979e1d-91fb-4107-8ebf-a970bb0600ee').
narrative_ontology:cs_kernel_codification('c9979e1d-91fb-4107-8ebf-a970bb0600ee', distributed).
narrative_ontology:cs_authority_grounding('c9979e1d-91fb-4107-8ebf-a970bb0600ee', lineage).
narrative_ontology:cs_interpretation_layer_present('c9979e1d-91fb-4107-8ebf-a970bb0600ee').
narrative_ontology:cs_reading_relation('c9979e1d-91fb-4107-8ebf-a970bb0600ee', honor_satisfaction_substrate__practice_decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9979e1d-91fb-4107-8ebf-a970bb0600ee', honor_satisfaction_substrate__composite_overdetermined_reading, influences).
narrative_ontology:cs_axiom('c9979e1d-91fb-4107-8ebf-a970bb0600ee', foundational, honor_cosmology_substrate_is_historically_contingent).
narrative_ontology:cs_axiom_status(honor_cosmology_substrate_is_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('c9979e1d-91fb-4107-8ebf-a970bb0600ee', honor_cosmology_substrate_is_historically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('c9979e1d-91fb-4107-8ebf-a970bb0600ee', foundational, dignity_cosmology_is_logically_incompatible_with_honor_violence).
narrative_ontology:cs_axiom_status(dignity_cosmology_is_logically_incompatible_with_honor_violence, holdable).
narrative_ontology:cs_axiom_grounding('c9979e1d-91fb-4107-8ebf-a970bb0600ee', dignity_cosmology_is_logically_incompatible_with_honor_violence, deontological).
narrative_ontology:cs_reference_frame('c9979e1d-91fb-4107-8ebf-a970bb0600ee', honor_cosmology_normative_monopoly).
narrative_ontology:cs_drift_state('c9979e1d-91fb-4107-8ebf-a970bb0600ee', dignity_cosmology_ascendance, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('c9979e1d-91fb-4107-8ebf-a970bb0600ee', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, societies_abandoning_honor_cosmology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(honor_satisfaction_substrate__cultural_contraction_reading, dignity_advocates).
narrative_ontology:constraint_victim(honor_satisfaction_substrate__cultural_contraction_reading, dueling_practitioners).
narrative_ontology:constraint_vindicates(honor_satisfaction_substrate__cultural_contraction_reading, dignity_hierarchy_incompatible_with_honor_satisfaction_mechanics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elite men (nobility, military officer corps, bourgeois professionals) whose identity and social standing were constituted through honor acquisition and defense via ritualized violence. Dueling was not a choice within an alternative action-set; it was the expression of a particular self-conception. As the honor code's substrate erodes and 'dignity' replaces 'honor' as the organizing principle of personhood, the practice becomes unthinkable not through suppression but through cognitive reorientation — what once was the only intelligible response to slight becomes literally unimaginable.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, dueling_practitioners, payer,
    moderate, biographical, identity_locked, continental).

% The cultural authorities and institutional actors (military hierarchies, aristocratic circles, professional guilds) that transmitted and adjudicated the honor code. They were the repository of its rules, the arbiters of offense and satisfaction, and the enforcers of its social meaning. As the broader cultural frame shifts from honor cosmology to dignity cosmology, their interpretive authority over what constitutes honor and how it is satisfied becomes structurally inert — the substrate they depend on for legitimacy disintegrates.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, honor_code_keepers, agenda_setter,
    powerful, generational, mobile, continental).

% Enlightenment philosophers, religious reformers, and institutional actors (legal systems, churches, emerging state apparatuses) who promoted dignity as the new organizing principle of personhood. They articulated an alternative cosmology in which a person's worth is intrinsic and inalienable rather than contingent on honor acquisition and defense. They did not primarily suppress dueling; they rendered it cognitively and morally incoherent within their framework.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, dignity_advocates, beneficiary,
    institutional, generational, arbitrage, global).

% State legal systems that criminalized dueling. This reading holds that legal prohibition was downstream of cognitive delegitimation, not its driver — the laws became enforceable because the cultural substrate supporting dueling had already eroded. Prohibition was the institutional codification of a shift that had already occurred at the level of meaning.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, legal_prohibition_enforcer, observer,
    institutional, generational, analytical, national).

% The analytical seat recording the constraint as a structural feature of the honor cosmology: a particular way of constituting personhood that makes violence-as-satisfaction logically necessary. The constraint is the honor code itself as substrate, not the dueling practice. When the substrate disappears, the constraint ceases to operate regardless of formal legal status.
narrative_ontology:constraint_stakeholder(honor_satisfaction_substrate__cultural_contraction_reading, anthropological_observer, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code solved a genuine coordination problem within societies that organized personhood around honor: it provided a rule-set for how status insults could be answered, how satisfaction could be achieved, and how the social standing of both parties would be restored. Without such rules, honor disputes would devolve into uncontrolled feuding; the code channeled honor-defense into ritualized, bounded forms.
% TRANSFER_FUNCTION: Transfers social standing and personhood-worth from the dishonored party to the satisfying party through the ritualized act of dueling. The practice moves status upward for the winner and downward (or maintains loss) for the loser or the party who refuses satisfaction.
% ABSENT_VOICES: Persons excluded from honor — women, enslaved people, colonized populations, the poor — had no standing in the dueling system and no mechanism to claim satisfaction for wrongs done them. This exclusion was structural to the honor cosmology itself, not accidental to its operation. They would object to the entire framework if given voice, but they were not in the conversation that the dignity advocates were having with the honor-code keepers.
% DISAPPEARANCE_RATIONALE: When the honor code ceased to be the interpretive substrate, dueling vanished as a thinkable practice within one or two generations across entire societies without sustained legal enforcement (in some jurisdictions the legal prohibitions came AFTER the practice had already collapsed in actual behavior). The social arrangements that organized personhood around honor satisfaction restructured themselves; new institutions (legal remedies, professional reputation systems, institutional hierarchies based on credentials rather than lineage) emerged to organize status and standing.
% FOUNDING_PROBLEM: Organized violence rooted in honor defense required a cultural substrate that made such violence intelligible as status-maintenance rather than mere aggression. The honor code provided that substrate: it defined what counted as insult, what satisfaction looked like, and how restored standing was achieved.
% FOUNDING_PROBLEM_CORROBORATION: Historians and historical sociologists (Kiernan, Frevert, Mohr, Pohl) document that dueling declined precipitously not because legal prohibitions suddenly worked but because the cultural intelligibility of dueling as honor-satisfaction eroded. Military officers stopped challenging each other to duels in the mid-19th century not primarily from fear of legal penalty but because the cognitive frame that made dueling THE response to insult had shifted. Enlightenment philosophers and dignity advocates of the 18th-19th centuries (Kant, evangelical reformers, emerging bourgeois professional communities) articulated the alternative cosmology. This corroboration comes from outside the honor-code keepers' own justifications.
narrative_ontology:disappearance_verdict(honor_satisfaction_substrate__cultural_contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_substrate__cultural_contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_substrate__cultural_contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_substrate__cultural_contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_satisfaction_substrate__cultural_contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_satisfaction_substrate__cultural_contraction_reading),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_satisfaction_substrate__cultural_contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_satisfaction_substrate__cultural_contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness measure (0.15 terminal) reflects that the honor code, as a mountain constraint, extracts very little — it is a structural feature of a cosmology, not a coercive apparatus. The slight upward drift (1650→1800) traces the period when dignity advocates were articulating alternatives and the honor code's monopoly on personhood-framing weakened; the subsequent decline (1800→1920) reflects the substrate's collapse and the code's loss of interpretive power. Theater_ratio rises during the intermediate period (dignitary alternatives becoming salient but honor practice persisting as performative remnant) and then stabilizes low as the practice exits the thinkable. Suppression_requirement is negligible throughout (a true mountain has no suppression machinery because nothing could prevent it; here suppression is minimal because the code is not sustained by coercion but by the interpretive substrate). The accessibility_collapse is very high (0.92) because once dueling was the only intelligible response to certain insults — alternatives were cognitively inaccessible, not merely illegal. Resistance is near-zero (0.03) because there was no sustained organized resistance to the honor code itself; what changed was that the substrate supporting it disappeared. The claim/metric gap is intentional: the constraint is claimed as mountain (structural feature of a cosmology) while extractiveness is non-zero (the code extracts status from the dishonored); the engine's computation will measure that this is more like mountain erosion than suppression, which is precisely the reading's assertion.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of dueling practitioners, the constraint was not oppressive but constitutive — it was the very substance of personhood and social standing. As the dignity cosmology rose, their experience shifted from 'this is what I am' to 'this is unthinkable' without any external force being applied to them. From the seat of honor-code keepers, the constraint was a legitimate authority structure they administered; it lost its force not through suppression but through loss of substrate. From the analytical seat, the constraint is the honor code as interpretive substrate; once that substrate is gone, the constraint ceases to operate regardless of formal legal status. The engine should compute these seats as arriving at the same type (mountain or mountain-erosion) from different causal pathways.
 *
 * DIRECTIONALITY LOGIC:
 *   Dueling practitioners face identity-lock (exit = self-annihilation) in the honor frame but arbitrage (switching frames entirely) in the dignity frame. Their directionality is complex: they appear to be targets (the code extracts status cost from losers) but are also constituted by it (the code makes their standing possible). The honor-code keepers are beneficiaries of the code's existence and legitimacy; they collected social authority from maintaining it. The dignity advocates are beneficiaries of its collapse (they articulated the alternative). The directionality overrides account for the fact that the practitioners' exit option changes fundamentally when the substrate shifts — before, exit was impossible (identity-locked); after, the frame itself is no longer binding. This reading treats that reorientation as the constraint's operation, not as suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (organizing honor-satisfaction without uncontrolled feuding) was structurally solved by the honor code for as long as the honor cosmology persisted. When the dignity cosmology rose, the problem itself became incoherent — dignity cannot be lost through insult and cannot be restored through violence, so 'honor satisfaction' as a category disappeared. This is not mandatrophy in the sense of a function that outlived its problem; it is the problem itself becoming unthinkable. The founding_problem_status is 'dead' because the organizing frame that made honor-satisfaction a real coordination problem dissolved. The reading distinguishes this from the practice_decline_reading (which holds the problem persists and dueling declined due to suppression) and from the composite_overdetermined_reading (which holds both suppression and delegitimation operated).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_erosion_vs_suppression_ambiguity,
    'Did the honor code''s interpretive substrate erode independently of legal suppression, or were suppression and delegitimation causally inseparable and mutually reinforcing?',
    'Historical counterfactual analysis: in jurisdictions where legal prohibition came BEFORE cultural delegitimation, does the practice persist? In jurisdictions where delegitimation preceded legal prohibition, does the practice collapse without law? Comparative historical sociology examining timing sequences and causal mechanisms across multiple societies.',
    'If substrate erosion was independent (prior to or parallel to suppression), the constraint is mountain-erosion and duelists face genuine identity-lock and reorientation. If suppression and delegitimation were inseparable, the constraint may be tangled_rope (coordination function + extraction via suppression). If suppression was downstream of delegitimation, this reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_erosion_vs_suppression_ambiguity, empirical, 'Whether honor-code substrate erosion was causally independent of legal suppression.').

omega_variable(
    dignity_cosmology_emergence_mechanism,
    'Was the shift from honor to dignity cosmology driven by internal intellectual/theological developments (Enlightenment philosophy, religious reform) or by structural changes in economic organization (rise of bourgeois market society, reduced dependence on face-to-face honor status)?',
    'Intellectual history paired with economic/institutional history. Did dignity rhetoric emerge in intellectual communities before societies'' structural organization changed, or after? What empirical shifts in occupational and status-acquisition mechanisms correlate with the transition?',
    'If driven by intellectual/theological development, dignity advocates actively delegitimated the honor code. If driven by structural change, the honor code became functionally obsolete as economic systems stopped depending on honor-based status. This affects whether this reading''s mechanism is ''ideas changed minds'' or ''material conditions changed ideas and institutions together''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_cosmology_emergence_mechanism, conceptual, 'Whether dignity cosmology emergence was intellectually or structurally driven.').

omega_variable(
    identity_lock_vs_institutional_exit,
    'For dueling practitioners, does the shift from honor to dignity constitute an individual-level identity reorientation (they face genuine self-annihilation via exit) or an institutional role exit where new occupational paths and status mechanisms arose (they could transfer their standing to new institutional contexts)?',
    'Biographical and autobiographical evidence from former duelists and their descendants. Did practitioners experience the transition as cognitive/existential crisis or as occupational redeployment? How did military officers, noblemen, and professionals reposition themselves in dignity-based hierarchies?',
    'If identity-locked, the constraint''s operation is the practitioners'' inability to imagine exit — accessibility_collapse remains very high. If institutional exit is available, the exit_options shift from identity_locked to constrained or mobile, and directionality changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_institutional_exit, empirical, 'Whether honor-cosmology exit is individual identity crisis or institutional role redeployment.').

omega_variable(
    mountain_vs_fallen_mountain_classification,
    'Is this constraint a true mountain (the honor code as structural feature of a cosmology, which ceases to apply when the cosmology is replaced) or a fallen mountain (a constraint that once appeared natural but was actually constructed, and was abandoned when its constructed nature became visible)?',
    'Conceptual/historical analysis of whether honor cosmology ever was a ''natural law'' vs. always a human construction. Did contemporaries experience it as inevitable natural fact? Was it presented as such by authorities? Historical evidence of contestation or alternatives within the honor framework itself.',
    'If true mountain, the constraint''s operation is subordinate to the cosmology; when the cosmology changes, the constraint ceases without residue. If fallen mountain (false summit), the constraint was always constructed and extraction was always present, merely hidden by the honor frame. This affects the interpretation of the beneficiary (societies_abandoning_honor_cosmology) — are they benefiting from a mountain erosion or from the revelation of a false summit?',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mountain_vs_fallen_mountain_classification, conceptual, 'Whether honor code was true mountain or false summit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_substrate__cultural_contraction_reading, 1650, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1650, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1650, 0.05).
narrative_ontology:measurement_basis(hono_tr_t1650, projected).
narrative_ontology:measurement(hono_tr_t1725, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1725, 0.08).
narrative_ontology:measurement_basis(hono_tr_t1725, projected).
narrative_ontology:measurement(hono_tr_t1800, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1800, 0.14).
narrative_ontology:measurement_basis(hono_tr_t1800, observed).
narrative_ontology:measurement(hono_tr_t1850, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1850, 0.19).
narrative_ontology:measurement_basis(hono_tr_t1850, observed).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1900, 0.11).
narrative_ontology:measurement_basis(hono_tr_t1900, observed).
narrative_ontology:measurement(hono_tr_t1920, honor_satisfaction_substrate__cultural_contraction_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement_basis(hono_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(hono_be_t1650, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1650, 0.08).
narrative_ontology:measurement_basis(hono_be_t1650, projected).
narrative_ontology:measurement(hono_be_t1725, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1725, 0.12).
narrative_ontology:measurement_basis(hono_be_t1725, projected).
narrative_ontology:measurement(hono_be_t1800, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1800, 0.18).
narrative_ontology:measurement_basis(hono_be_t1800, observed).
narrative_ontology:measurement(hono_be_t1850, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1850, 0.16).
narrative_ontology:measurement_basis(hono_be_t1850, observed).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1900, 0.13).
narrative_ontology:measurement_basis(hono_be_t1900, observed).
narrative_ontology:measurement(hono_be_t1920, honor_satisfaction_substrate__cultural_contraction_reading, base_extractiveness, 1920, 0.15).
narrative_ontology:measurement_basis(hono_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1650, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1650, 0.02).
narrative_ontology:measurement_basis(hono_su_t1650, projected).
narrative_ontology:measurement(hono_su_t1725, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1725, 0.03).
narrative_ontology:measurement_basis(hono_su_t1725, projected).
narrative_ontology:measurement(hono_su_t1800, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1800, 0.06).
narrative_ontology:measurement_basis(hono_su_t1800, observed).
narrative_ontology:measurement(hono_su_t1850, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1850, 0.09).
narrative_ontology:measurement_basis(hono_su_t1850, observed).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1900, 0.08).
narrative_ontology:measurement_basis(hono_su_t1900, observed).
narrative_ontology:measurement(hono_su_t1920, honor_satisfaction_substrate__cultural_contraction_reading, suppression_requirement, 1920, 0.08).
narrative_ontology:measurement_basis(hono_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_substrate__cultural_contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_substrate__cultural_contraction_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__practice_decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_substrate__cultural_contraction_reading, honor_satisfaction_substrate__composite_overdetermined_reading).

% DUAL FORMULATION NOTE:
% The honor_satisfaction_substrate kernel is instantiated by three distinct constraint stories: practice_decline_reading (honor code persists; dueling suppressed), cultural_contraction_reading (this one: honor code itself erodes as cosmology shifts), and composite_overdetermined_reading (both suppression and delegitimation operated simultaneously). These are not perspectives on one constraint; they are three structurally distinct constraints competing to explain the same historical phenomenon. The cultural_contraction_reading is characterized by mountain-erosion (substrate disintegration rather than suppression) and identity-lock exit dynamics. It influences the composite reading by providing an alternative causal pathway; it coexists with the practice reading as different historians' interpretations of the same evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_substrate__cultural_contraction_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
