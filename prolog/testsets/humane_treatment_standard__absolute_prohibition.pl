% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Common Article 3 Absolute Prohibition on Torture and Degrading Treatment
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes that persons not
 *   actively participating in armed conflict—detainees, wounded, the
 *   sick—shall be treated humanely in all circumstances. The
 *   absolute-prohibition reading claims this standard is non-derogable: no
 *   military necessity, no security emergency, no contextual justification
 *   permits torture or degrading treatment. Detainees are rights-holders by
 *   status, not by negotiation. The constraint is claimed as a Mountain
 *   (irreducible feature of human dignity and international law); the low
 *   extraction and suppression scores (0.18, 0.12) reflect its presentation
 *   as a natural-law anchor. However, the beneficiary declaration
 *   (vindication of IHL doctrine) triggers FSM evaluation: does the
 *   international humanitarian law community profit from treating a
 *   constructed commitment as immutable natural law? The measurement series
 *   shows slight oscillation in extraction (small rise, then stabilization)
 *   and minimal theater—the constraint is minimally performative. The reading
 *   contest is the central structure: absolute prohibition forecloses
 *   contextual-necessity reading by design (either torture is never
 *   permitted, or it sometimes is—one negates the other), while it influences
 *   the proportionality-balancing reading (if you accept absolute
 *   prohibition, proportionality becomes incoherent).
 *
 * KEY AGENTS:
 *   - detainees_and_protected_persons: structurally trapped; rights-holder status conferred absolutely
 *   - state_security_apparatus: institutional payer; constrained exit because international law binds states collectively
 *   - international_humanitarian_law_community: institutional beneficiary and agenda-setter; maintains and interprets the standard
 *   - international_court_system: institutional agenda-setter; adjudicates boundaries through case law
 *   - security_doctrine_proponents: structurally excluded; their security-exception argument is foreclosed by the reading itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.18).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.12).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.18).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, mountain).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Common Article 3 Absolute Prohibition on Torture and Degrading Treatment").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights").

domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '5ff98756-44d5-43e1-8d30-6d0e5f83d6ca').
narrative_ontology:cs_kernel_codification('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', fixed_text).
narrative_ontology:cs_authority_grounding('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', lineage).
narrative_ontology:cs_interpretation_layer_present('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca').
narrative_ontology:cs_reading_relation('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', foundational, torture_categorically_impermissible).
narrative_ontology:cs_axiom_status(torture_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', torture_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', foundational, human_dignity_non_negotiable).
narrative_ontology:cs_axiom_status(human_dignity_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', human_dignity_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', post_wwii_non_derogable_rights_framework).
narrative_ontology:cs_drift_state('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', contemporary_terrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ff98756-44d5-43e1-8d30-6d0e5f83d6ca', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainee_protection_regime).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, international_humanitarian_law_vindication).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees_and_protected_persons).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, international_humanitarian_law_community).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, state_security_apparatus).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, inalienable_human_dignity).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, non_derogable_rights_doctrine).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, absolute_prohibition_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons detained in armed conflict or custody. The constraint declares them rights-holders whose bodily integrity and dignity are inalienable — torture and degrading treatment are absolutely prohibited regardless of security circumstances. They cannot negotiate exit from the constraint; it protects them by preventing the state apparatus from crossing an absolute threshold.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees_and_protected_persons, beneficiary,
    powerless, biographical, trapped, universal).

% Military, intelligence, and law enforcement entities tasked with national security. The constraint prevents them from using torture or degrading treatment as interrogation methods, even when they believe security imperatives override detainee protection. They bear the cost of adhering to the absolute standard: interrogation must proceed through lawful methods that yield less rapid intelligence and constrain tactical flexibility.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_security_apparatus, payer,
    institutional, generational, constrained, universal).

% Treaty-signatory states, international courts, human rights bodies, and legal scholars committed to the principle that certain acts are unconditionally impermissible. The constraint vindicates the non-derogability doctrine itself — the claim that some protections cannot be suspended even in emergency. Their benefit is doctrinal coherence and the precedent that absolute prohibitions can persist as binding law.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_humanitarian_law_community, beneficiary,
    institutional, generational, arbitrage, universal).

% State actors and strategic theorists who argue that national security emergencies justify enhanced interrogation methods. They are excluded from the constraint's framing because the absolute-prohibition reading forecloses their security-exception argument by design. They would contest that torture bans reduce effectiveness against terrorism; their voices are structurally shut out by the constraint's own logic.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, security_doctrine_proponents, excluded,
    institutional, generational, trapped, universal).

% International Criminal Court, International Court of Justice, human rights treaty bodies, and courts of signatory states that adjudicate Common Article 3 compliance. They interpret and enforce the absolute standard, determining what acts constitute torture or degrading treatment and whether states have breached the non-derogable obligation. They decide the constraint's boundaries through case law.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_court_system, agenda_setter,
    institutional, generational, analytical, universal).

% Persons who have survived torture or degrading treatment. They testify to the physical and psychological harm, provide evidence for prosecutions, and represent the constraint's vindication through accountability. They carry the constraint's impact in their bodies and advocate for its enforcement.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, victims_of_torture, observer,
    organized, biographical, constrained, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a binding legal standard that all states—regardless of their security posture, geopolitical position, or threat environment—treat detainees with minimum respect for human dignity. Solves the collective-action problem of race-to-the-bottom in interrogation practices by making the prohibition universal and non-waivable.
% TRANSFER_FUNCTION: Transfers interrogation constraints (loss of certain tactics, slower intelligence gathering) from detainees to state security apparatus. Detainees gain absolute protection; states pay through operational friction and limits on tactical options.
% ABSENT_VOICES: State security personnel and doctrine theorists who believe certain detainees pose extreme risks that justify enhanced methods; strategic actors who would employ context-dependent interrogation regimes if permitted. These voices are structurally foreclosed by the reading itself—the absolute prohibition exists to override their security calculus.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, detainee protections would collapse to national legal standards (highly variable); torture would become conditionally legal in many jurisdictions; interrogation practices would immediately shift toward enhanced methods justified by security necessity; international humanitarian law would lose its strongest non-derogable anchor; accountability regimes would fragment. The entire post-WWII human rights architecture depends on absolute prohibitions persisting.
% FOUNDING_PROBLEM: After WWII and the discovery of Nazi and Japanese systematized torture, the international community identified torture and degrading treatment as existential threats to human dignity that must never be permitted, regardless of circumstance or justification.
% FOUNDING_PROBLEM_CORROBORATION: The International Court of Justice (Prosecutor v. Furundžija, 1998) and International Criminal Court repeatedly affirm the absolute character of the prohibition. Human rights bodies document ongoing torture globally, confirming the founding problem persists. Academic consensus in international law treats non-derogability as a core feature of IHL. States themselves have never repealed Common Article 3, though some contest its application in specific cases—they do not contest the standard's existence.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, ExtMetricName, E),
    domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The authored extractiveness (0.18) is low because the absolute prohibition is presented as emerging from human dignity, not as a mechanism by which one party extracts from another. Suppression (0.12) is minimal because the constraint does not depend on coercive enforcement—it depends on normative commitment and legal jurisdiction. Theater (0.08) is very low: the constraint is minimally performative. Accessibility collapse (0.92) is very high: once you accept that torture is constitutive harm, alternatives (contextualized interrogation methods, security exceptions) become literally unthinkable within the framework. Resistance (0.64) is substantial because state security actors regularly attempt to reframe torture as 'enhanced interrogation' and test the boundaries through practice, and because powerful states sometimes violate with impunity. The time-series measurements show slight metric drift: extractiveness ticks up very slightly (0.15→0.18) as the international legal apparatus becomes more active in enforcement and interpretation, theater increases marginally as compliance performance becomes more prominent, and suppression rises slightly as states invest more enforcement effort to prevent violations by their own apparatus. None of the drift is severe—the core standard remains stable.
 *
 * PERSPECTIVAL GAP:
 *   From the detainee's perspective, the constraint offers absolute protection—exit is impossible but the constraint protects them by making torture illegal. From the state security apparatus's perspective, the constraint imposes a cost: intelligence-gathering is constrained and interrogation must proceed through slower, less coercive methods. From the IHL community's perspective, the constraint vindicates the doctrine of absolute rights, but only if the community can maintain the standard against state violations—enforcement failures weaken the whole framework. From a security-doctrine perspective (excluded), the constraint appears as an externally imposed limitation that prevents effective response to genuine threats. The engine should compute these as different directionalities: detainees benefit (d near 0.0), states pay (d near 1.0), IHL community benefits (d near 0.0), excluded security actors see extraction from their perspective (d near 1.0 from a security-first frame). The author claims Mountain; the engine measures low extraction and will likely confirm or flag false-summit depending on how it weights IHL community benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees are beneficiaries with no exit options (trapped): the constraint protects them absolutely, so d is very low (full beneficiary, 0.0). State security apparatus are payers with constrained exit (states are bound by treaty collectively and cannot unilaterally opt out): d is high but not maximal because states can attempt to redefine the boundary through interpretation, so d ≈ 0.75. IHL community are beneficiaries (the doctrine vindicates their framework), exit is arbitrage (they can move to a more permissive legal regime if they prefer, but choose not to), so d ≈ 0.15. Excluded security actors have trapped exit (they cannot participate in the framework to change it from within), so from their perspective d would be high (1.0), but they are excluded so their directionality is not computed. The engine derives these from beneficiary/victim data and exit modulation; no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing torture after WWII) remains live—states continue to torture despite the prohibition, and the international community continues to oppose it. The constraint has not atrophied functionally; it remains the anchor of international humanitarian law. However, selective enforcement raises the mandatrophy question at the edges: when powerful states violate and avoid prosecution, does the constraint's mandate (absolute prohibition) outlive its enforcement capacity? The false-summit omega captures this: if the IHL community benefits from treating the constraint as natural law while enforcing it selectively, the constraint becomes a Tangled Rope (coordination of the IHL regime + asymmetric extraction by powerful states). The absolute-prohibition framing prevents this reclassification from being acknowledged publicly, but the omega documents the structural tension. The constraint is NOT yet a piton (its function is not atrophied, it is actively maintained), but it shows strain from the gap between the stated standard and differential enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_commitment,
    'Is the absolute prohibition on torture a natural law that emerges from irreducible human dignity, or a constructed legal and political commitment that could theoretically be undone by states?',
    'Philosophical and historical analysis: does the prohibition persist because it reflects an unchangeable feature of moral reality, or because the international community has chosen to bind itself collectively? Test: can the commitment be genuinely rescinded, or do attempts to rescind it reveal that the prohibition is treated as non-negotiable by the broader community regardless of formal withdrawal?',
    'If natural law: the constraint''s classification as a mountain is vindicated. If constructed: the constraint might be a Tangled Rope with extremely high consensus, and future rifts could unravel it. FSM would flag whether beneficiaries profit from treating a constructed commitment as natural law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_commitment, conceptual, 'Whether human dignity grounds an irreducible absolute prohibition or a chosen legal standard.').

omega_variable(
    absolute_vs_contextual_boundary,
    'Is the boundary between torture and lawful interrogation truly absolute and observer-independent, or does it depend on context (detainee status, interrogator intent, security stakes, cultural norms)?',
    'Comparative jurisprudence: does case law produce a stable, context-invariant definition of torture? Or do courts apply context-dependent standards while labeling them absolute? ICC prosecutions show how the boundary is operationalized.',
    'If truly absolute: accessibility collapse is complete (0.92) and alternatives vanish structurally. If context-dependent: accessibility collapse is lower, suppression is higher (interpretively enforced, not structurally immutable), and the reading may be a Tangled Rope pretending to be Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absolute_vs_contextual_boundary, empirical, 'Whether absolute-prohibition standards remain stable across cases or require contextual interpretation.').

omega_variable(
    sibling_reading_pressure__contextual_necessity,
    'Does this absolute-prohibition reading foreclose the contextual-necessity reading logically, or do they coexist as competing frameworks held simultaneously by different states?',
    'Examine state ratification patterns, reservations, and subsequent practice: do states switch from absolute-prohibition understanding to contextual-necessity practice under security pressure? Do they explicitly disavow the absolute reading, or claim to remain within the absolute framework while redefining the boundary?',
    'If logically opposed: one reading must prevail and the kernel contest is genuine binary choice. If coexisting: both readings are live and the constraint family is genuinely contested. This affects whether the engine tracks them as opposed constraints or as competing frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_pressure__contextual_necessity, empirical, 'Whether absolute prohibition and contextual necessity are logically opposed or coexisting frameworks.').

omega_variable(
    beneficiary_selective_enforcement,
    'Does the IHL community enforce absolute prohibition equally against all states, or is enforcement selective when major powers face security threats?',
    'Track ICC and ICJ prosecution patterns: are powerful states prosecuted for violations with the same intensity as weaker states? Does the community redefine boundaries contextually for powerful actors while maintaining strict standards for others?',
    'If equal enforcement: genuine coordination—the community solves a collective-action problem. If selective: the community profits from treating the standard as natural law while maintaining exit optionality for itself—false-summit candidate. FSM would reclassify as Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_selective_enforcement, empirical, 'Whether IHL institutions enforce absolute prohibition uniformly or practice selective application.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__absolute_prohibition, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(huma_tr_t0, observed).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__absolute_prohibition, theater_ratio, 10, 0.06).
narrative_ontology:measurement_basis(huma_tr_t10, observed).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__absolute_prohibition, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(huma_tr_t20, observed).
narrative_ontology:measurement(huma_tr_t30, humane_treatment_standard__absolute_prohibition, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(huma_tr_t30, observed).
narrative_ontology:measurement(huma_tr_t40, humane_treatment_standard__absolute_prohibition, theater_ratio, 40, 0.08).
narrative_ontology:measurement_basis(huma_tr_t40, observed).
narrative_ontology:measurement(huma_tr_t50, humane_treatment_standard__absolute_prohibition, theater_ratio, 50, 0.08).
narrative_ontology:measurement_basis(huma_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__absolute_prohibition, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(huma_be_t0, observed).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__absolute_prohibition, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(huma_be_t10, observed).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__absolute_prohibition, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(huma_be_t20, observed).
narrative_ontology:measurement(huma_be_t30, humane_treatment_standard__absolute_prohibition, base_extractiveness, 30, 0.19).
narrative_ontology:measurement_basis(huma_be_t30, observed).
narrative_ontology:measurement(huma_be_t40, humane_treatment_standard__absolute_prohibition, base_extractiveness, 40, 0.18).
narrative_ontology:measurement_basis(huma_be_t40, observed).
narrative_ontology:measurement(huma_be_t50, humane_treatment_standard__absolute_prohibition, base_extractiveness, 50, 0.18).
narrative_ontology:measurement_basis(huma_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__absolute_prohibition, suppression_requirement, 0, 0.1).
narrative_ontology:measurement_basis(huma_su_t0, observed).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__absolute_prohibition, suppression_requirement, 10, 0.11).
narrative_ontology:measurement_basis(huma_su_t10, observed).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__absolute_prohibition, suppression_requirement, 20, 0.12).
narrative_ontology:measurement_basis(huma_su_t20, observed).
narrative_ontology:measurement(huma_su_t30, humane_treatment_standard__absolute_prohibition, suppression_requirement, 30, 0.13).
narrative_ontology:measurement_basis(huma_su_t30, observed).
narrative_ontology:measurement(huma_su_t40, humane_treatment_standard__absolute_prohibition, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(huma_su_t40, observed).
narrative_ontology:measurement(huma_su_t50, humane_treatment_standard__absolute_prohibition, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(huma_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__absolute_prohibition, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).

% DUAL FORMULATION NOTE:
% The humane_treatment_standard kernel decomposes into three constraints: (1) absolute_prohibition—this reading, treating torture as unconditionally impermissible; (2) contextual_necessity—holding that security imperatives can justify enhanced interrogation; (3) proportionality_balancing—requiring weighing detainee dignity against security without accepting either pole. These are NOT measurements of the same constraint from different angles; they are different structural claims about what Common Article 3 permits. Each has distinct ε (extraction increases under contextual_necessity and proportionality readings), distinct beneficiary/victim structures, and distinct stakeholder positions. The absolute-prohibition reading logically forecloses contextual-necessity within any single legal framework; it influences proportionality-balancing by making pure balancing seem incoherent to those who accept the absolute standard. Network links track these influences. Each reading is ε-invariant under its own interpretation; the contest between them is carried through omega variables (natural-law vs. constructed, beneficiary selective enforcement) rather than collapsed into one constraint's metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
