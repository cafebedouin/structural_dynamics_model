% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__drop_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__drop_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__drop_reading
 *   human_readable: Dueling as a Legitimate but Practically Abandoned Honor Mechanism (Drop Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the drop_reading of the honor_violence_legitimacy
 *   kernel: dueling as a practice among the gentry class between roughly 1700
 *   and 1900. Under this reading, the honor code that legitimates
 *   personal-combat resolution of insult was never conceptually revised or
 *   displaced — it remained fully thinkable and formally endorsed by the
 *   class that held it — but its practical exercise dropped sharply as
 *   external costs (legal criminalization, prosecution risk, church
 *   condemnation, rising bourgeois social disapproval, career and insurance
 *   consequences) made actually issuing or accepting a challenge increasingly
 *   ruinous. The theater_ratio rises across the interval precisely because
 *   the code's formal apparatus (seconds, codes duello, honor literature)
 *   persists and even elaborates while the underlying practice hollows out —
 *   an increasingly performative maintenance of a mechanism rarely exercised.
 *   This is deliberately NOT the contraction_reading (which holds honor
 *   itself was redefined to exclude violence, making dueling unthinkable
 *   rather than merely costly) and NOT the composite_reading (which holds
 *   both mechanisms operated together). Under the drop reading, the concept
 *   of honor-as-defensible-by-combat never atrophies; only the frequency of
 *   its exercise does, driven entirely by rising external cost.
 *
 * KEY AGENTS:
 *   - gentry_class_status_holders: primary beneficiary of continued code legitimacy
 *   - codified_honor_code_authors: agenda-setters maintaining the formal apparatus
 *   - dueling_participants_and_families: bear the escalating external costs of exercising a still-legitimate right
 *   - lower_status_men_excluded_from_the_code: permanently outside the mechanism, bearing the class-boundary cost
 *   - state_legal_authorities: raise external costs without engaging the honor logic directly
 *   - social_historians: analytical observers distinguishing drop from contraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__drop_reading, 0.42).
domain_priors:suppression_score(honor_violence_legitimacy__drop_reading, 0.35).
domain_priors:theater_ratio(honor_violence_legitimacy__drop_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(honor_violence_legitimacy__drop_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__drop_reading, piton).
narrative_ontology:human_readable(honor_violence_legitimacy__drop_reading, "Dueling as a Legitimate but Practically Abandoned Honor Mechanism (Drop Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__drop_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__drop_reading, '77425c0e-fc62-4394-8d9a-aa30b63488d8').
narrative_ontology:cs_kernel_codification('77425c0e-fc62-4394-8d9a-aa30b63488d8', formalized).
narrative_ontology:cs_authority_grounding('77425c0e-fc62-4394-8d9a-aa30b63488d8', practice).
narrative_ontology:cs_interpretation_layer_present('77425c0e-fc62-4394-8d9a-aa30b63488d8').
narrative_ontology:cs_reading_relation('77425c0e-fc62-4394-8d9a-aa30b63488d8', honor_violence_legitimacy__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('77425c0e-fc62-4394-8d9a-aa30b63488d8', honor_violence_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('77425c0e-fc62-4394-8d9a-aa30b63488d8', foundational, honor_code_legitimacy_survives_practice_decline).
narrative_ontology:cs_axiom_status(honor_code_legitimacy_survives_practice_decline, holdable).
narrative_ontology:cs_axiom_grounding('77425c0e-fc62-4394-8d9a-aa30b63488d8', honor_code_legitimacy_survives_practice_decline, conventional).
narrative_ontology:cs_axiom('77425c0e-fc62-4394-8d9a-aa30b63488d8', foundational, external_cost_alone_explains_frequency_drop).
narrative_ontology:cs_axiom_status(external_cost_alone_explains_frequency_drop, holdable).
narrative_ontology:cs_axiom_grounding('77425c0e-fc62-4394-8d9a-aa30b63488d8', external_cost_alone_explains_frequency_drop, empirically_contingent).
narrative_ontology:cs_reference_frame('77425c0e-fc62-4394-8d9a-aa30b63488d8', codified_gentry_honor_practice).
narrative_ontology:cs_drift_state('77425c0e-fc62-4394-8d9a-aa30b63488d8', late_nineteenth_century_legal_suppression, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('77425c0e-fc62-4394-8d9a-aa30b63488d8', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, gentry_class_status_holders).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__drop_reading, codified_honor_code_authors).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, dueling_participants_and_families).
narrative_ontology:constraint_victim(honor_violence_legitimacy__drop_reading, lower_status_men_excluded_from_the_code).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__drop_reading, honor_is_defensible_by_personal_combat).
narrative_ontology:constraint_vindicates(honor_violence_legitimacy__drop_reading, the_gentleman_class_polices_its_own_conduct).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold their position partly through the reputational architecture that dueling legitimacy underwrites — being a class of men entitled to answer insult with the sword or pistol marks who counts as a gentleman at all. As external costs (legal liability, social scandal, rising bourgeois disapproval, insurance and career risk) rose, this class increasingly relies on the mere availability of the code rather than its exercise: the right to duel remains a status marker that need not be cashed in.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, gentry_class_status_holders, beneficiary,
    powerful, generational, arbitrage, national).

% Authors and custodians of formal codes duello and honor manuals who maintain the doctrinal apparatus — rules of challenge, seconds, satisfaction — even as actual encounters become rare. They administer the framework that keeps dueling structurally legitimate: it is never repealed or redefined out of existence, merely priced out of practice.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, codified_honor_code_authors, agenda_setter,
    organized, generational, mobile, national).

% Individual men who, when insulted, remain formally entitled and in some circles obligated to issue or accept a challenge — bearing death, injury, prosecution, and family ruin as the cost of exercising a right that still counts as honorable. External costs (dueling statutes, church condemnation, social ostracism from emerging bourgeois norms) make exercising the right increasingly ruinous without removing the obligation to be willing to exercise it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, dueling_participants_and_families, payer,
    moderate, biographical, constrained, local).

% Men outside the gentry class have no standing to duel and no honor-defense mechanism recognized by the code at all — insults against them are not answerable by the same ritual, which reinforces the class boundary the code exists partly to police. They pay through permanent exclusion from a recognized redress mechanism, not through participation in it.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, lower_status_men_excluded_from_the_code, payer,
    powerless, biographical, trapped, local).

% Increasingly criminalize dueling and impose real legal costs (prosecution, loss of office, civil liability) that drive down practice frequency, but cannot fully displace the honor code's social legitimacy among the gentry — their statutes raise the external cost without ever formally engaging or refuting the honor logic itself, so the code persists structurally alongside the law that suppresses its exercise.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, state_legal_authorities, excluded,
    institutional, generational, constrained, national).

% Study the divergence between dueling's continued conceptual legitimacy and its collapsing practice frequency, using dueling-rate data, legal records, and honor-code literature to determine whether the decline reflects cost-driven suppression, conceptual redefinition, or both.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__drop_reading, social_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(honor_violence_legitimacy__drop_reading, diffuse).
narrative_ontology:fixing_cost_class(honor_violence_legitimacy__drop_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The honor code coordinates status competition among a gentry class by providing an agreed, rule-bound mechanism for answering insult that avoids unregulated violence — everyone in the class knows the procedure, the escalation ladder, and the terms of resolution.
% TRANSFER_FUNCTION: Moves risk of injury, death, and legal jeopardy onto individual challengers and their families in exchange for reputational capital that accrues to the class as a whole and to the code's institutional custodians who administer its rules of legitimacy.
% ABSENT_VOICES: Lower-status men have no standing under the code and are never consulted on its terms; women, entirely excluded as principals, bear collateral costs (widowhood, family ruin) without any voice in whether the mechanism operates. Rising bourgeois and legal reformist voices object to the code's continued legitimacy but operate largely outside gentry social circles where the code's authority is actually adjudicated.
% DISAPPEARANCE_RATIONALE: Under the drop reading, if dueling's residual legitimacy vanished overnight, gentry status competition would not immediately rearrange — practice was already rare, so day-to-day life changes little. But the class's self-conception as bound by a personal-combat code of honor, and the background threat that structures certain insults as answerable, would lose their reference point; whether that constitutes 'the world rearranging' or 'the world staying the same' is exactly what separates this reading from the contraction reading, which holds the underlying honor concept itself has already moved on.
% FOUNDING_PROBLEM: In the absence of reliable state monopoly on adjudicating insult among social equals, dueling provided a rule-bound alternative to unregulated blood feud or unanswered dishonor, preserving hierarchy and preventing wider retaliatory violence.
% FOUNDING_PROBLEM_CORROBORATION: State legal authorities and social historians, standing outside the gentry class that benefits from continued code legitimacy, attest that functioning courts, police, and civil defamation remedies have long since supplied reliable non-violent adjudication of insult — the founding problem is solved by other institutions. The gentry class and honor-code authors themselves largely do not concede this; their continued adherence to the code's legitimacy despite the problem's resolution is precisely the drop-reading's claim: the mechanism persists structurally after its coordination rationale has been superseded, kept alive by external-cost suppression of practice rather than removal of the underlying claim.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__drop_reading, contested).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__drop_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__drop_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_violence_legitimacy__drop_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__drop_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__drop_reading_tests).
:- end_tests(honor_violence_legitimacy__drop_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rises only slightly across the interval — under this reading the mechanism's coordination function (status-competition resolution among equals) is real and largely unchanged; what drops is exercise, not legitimacy, so the underlying extractive structure is fairly stable. Theater_ratio rises sharply (0.15 to 0.58) precisely because this is the drop reading's signature: formal apparatus persists and elaborates (published codes, ritualized seconds, honor literature) while actual practice frequency collapses, producing an increasingly performative gap between doctrine and behavior. Suppression rises modestly (0.20 to 0.35) reflecting increasing legal criminalization, but stays well below what a contraction reading would need to show (where suppression of the underlying CONCEPT, not just the practice, would be the driver). Accessibility_collapse is low (0.3) because under this reading alternatives to dueling for resolving gentry-class insult (dueling itself always remained an option, formally) never structurally close off — the code just becomes costlier to invoke, not conceptually unavailable.
 *
 * PERSPECTIVAL GAP:
 *   From the gentry beneficiary and honor-code-author seats, this looks like a stable, dignified tradition experiencing a natural decline in vulgar frequency while retaining its essential and honorable character — a rope, even a mountain of social necessity, that people simply exercise less. From the seat of participants who still face live obligations under the code, or from state legal authorities absorbing the externalized costs of enforcement and prosecution, the same structure looks like an increasingly hollow, performative piton: a mechanism whose founding coordination problem is dead but whose formal legitimacy is kept alive by class interest in the status marker it still confers, without anyone bearing enough concentrated cost or benefit to either fully abolish or fully revive it.
 *
 * DIRECTIONALITY LOGIC:
 *   Gentry status-holders and honor-code authors sit near the beneficiary end: they collect reputational capital and class-boundary maintenance from the code's mere availability, with arbitrage-grade exit (they need never actually duel to receive the benefit). Individual participants who must actually exercise the code sit near the target end: constrained exit, real bodily and legal risk, biographical time horizon. Lower-status excluded men sit as trapped, powerless payers — they bear the caste-boundary cost of a mechanism they cannot invoke at all. State authorities are excluded rather than coordinated: their statutes raise cost but never negotiate with the honor logic on its own terms, which is exactly why the code's legitimacy survives their pressure under this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This is close to a textbook mandatrophy case under the piton signature: the founding coordination problem (adjudicating insult among social equals absent reliable state redress) is dead — courts and civil remedies now perform that function — yet the code's formal legitimacy persists, sustained not by continued function but by the diffuse status interest of a class that benefits from the code's mere availability without needing anyone to bear the concentrated cost of formally repealing or defending it. No single agent profits enough from the code's exercise to actively maintain it (which would make it a snare); rather, everyone who could revise its legitimacy (courts, church, the gentry themselves) finds it cheaper to let it atrophy in practice than to formally contest it in doctrine — hence rising theater_ratio and moderate, non-accelerating extraction. The drop reading's classification hinges entirely on this: legitimacy is inert, not renegotiated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    drop_vs_contraction_discriminating_evidence,
    'Is the observed decline in dueling frequency better explained by rising external costs alone (drop), by conceptual redefinition of honor to exclude violence (contraction), or is it genuinely overdetermined (composite)?',
    'Examine honor-code literature and gentry correspondence for explicit redefinition of what constitutes honorable conduct versus mere lamentation about legal/social risk; a drop-reading-confirming pattern would show continued endorsement of dueling''s legitimacy in principle even as authors counsel against practicing it for prudential reasons; a contraction-confirming pattern would show honor itself being redefined (e.g. self-restraint or legal recourse becoming the honorable response to insult).',
    'If the historical record shows honor being actively redefined rather than merely priced out, this drop_reading story should be understood as capturing only part of the mechanism, with the contraction_reading or composite_reading carrying more of the true classification weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drop_vs_contraction_discriminating_evidence, empirical, 'Whether decline is driven by cost alone versus conceptual redefinition of honor.').

omega_variable(
    code_legitimacy_measurement_ambiguity,
    'Can ''structural legitimacy'' of dueling be measured independently of practice frequency, or does declining frequency itself erode legitimacy over time (making the drop reading unstable at long time horizons)?',
    'Track formal legal and social sanction status of dueling (is it still described as honorable in etiquette manuals, military codes of conduct, journalistic commentary) separately from prosecution/injury statistics across the 1700-1900 interval.',
    'If legitimacy itself measurably erodes over the interval (not just frequency), the drop reading collapses toward the contraction or composite reading at the tail end of the period, meaning this story''s flat/near-stable extractiveness trajectory may understate a real endpoint shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(code_legitimacy_measurement_ambiguity, conceptual, 'Whether legitimacy and practice frequency can be cleanly decoupled across a century-plus interval.').

omega_variable(
    beneficiary_class_natural_vs_constructed_ambiguity,
    'Is gentry status competition via honor-code legitimacy a natural feature of stratified social organization (any elite needs some status-defense mechanism) or a specific constructed artifact of this historical class formation?',
    'Comparative historical analysis of stratified societies without dueling-equivalent mechanisms to determine whether some formalized violence-based status defense is a structural universal or a contingent European gentry invention.',
    'If natural/universal, the underlying coordination function may deserve more mountain-like treatment even as this specific instantiation (dueling) is classified as piton; if contingent, the entire apparatus including its beneficiary structure is more clearly a constructed extraction mechanism from the start.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_class_natural_vs_constructed_ambiguity, conceptual, 'Whether honor-defense-by-combat is a structural universal of stratified societies or a contingent historical construct.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__drop_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__drop_reading, theater_ratio, 1700, 0.15).
narrative_ontology:measurement(hono_tr_t1740, honor_violence_legitimacy__drop_reading, theater_ratio, 1740, 0.22).
narrative_ontology:measurement(hono_tr_t1780, honor_violence_legitimacy__drop_reading, theater_ratio, 1780, 0.31).
narrative_ontology:measurement(hono_tr_t1820, honor_violence_legitimacy__drop_reading, theater_ratio, 1820, 0.42).
narrative_ontology:measurement(hono_tr_t1860, honor_violence_legitimacy__drop_reading, theater_ratio, 1860, 0.51).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__drop_reading, theater_ratio, 1900, 0.58).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__drop_reading, base_extractiveness, 1700, 0.3).
narrative_ontology:measurement(hono_be_t1740, honor_violence_legitimacy__drop_reading, base_extractiveness, 1740, 0.33).
narrative_ontology:measurement(hono_be_t1780, honor_violence_legitimacy__drop_reading, base_extractiveness, 1780, 0.36).
narrative_ontology:measurement(hono_be_t1820, honor_violence_legitimacy__drop_reading, base_extractiveness, 1820, 0.39).
narrative_ontology:measurement(hono_be_t1860, honor_violence_legitimacy__drop_reading, base_extractiveness, 1860, 0.41).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__drop_reading, base_extractiveness, 1900, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__drop_reading, suppression_requirement, 1700, 0.2).
narrative_ontology:measurement(hono_su_t1740, honor_violence_legitimacy__drop_reading, suppression_requirement, 1740, 0.24).
narrative_ontology:measurement(hono_su_t1780, honor_violence_legitimacy__drop_reading, suppression_requirement, 1780, 0.28).
narrative_ontology:measurement(hono_su_t1820, honor_violence_legitimacy__drop_reading, suppression_requirement, 1820, 0.31).
narrative_ontology:measurement(hono_su_t1860, honor_violence_legitimacy__drop_reading, suppression_requirement, 1860, 0.33).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__drop_reading, suppression_requirement, 1900, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__drop_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_violence_legitimacy__drop_reading, 0.1).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__contraction_reading).
narrative_ontology:affects_constraint(honor_violence_legitimacy__drop_reading, honor_violence_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the honor_violence_legitimacy kernel: drop_reading (this story — legitimacy persists, practice frequency falls due to external cost), contraction_reading (honor is conceptually redefined to exclude violence, making dueling unthinkable), and composite_reading (both mechanisms operate simultaneously, overdetermining the decline). Each reading authors its own extractiveness, theater_ratio, and beneficiary/victim structure per the reading's own lights; they are not to be averaged or reconciled into a single ε. The drop_reading is distinguished by low accessibility_collapse (alternatives to dueling never conceptually foreclose the practice) and rising theater_ratio (formal apparatus persists while exercise hollows out) — a signature the contraction_reading would not share, since under contraction the concept itself narrows rather than the formal doctrine becoming performative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
