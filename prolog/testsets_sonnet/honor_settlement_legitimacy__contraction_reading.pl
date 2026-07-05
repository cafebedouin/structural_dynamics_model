% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Honor-Settlement Legitimacy Kernel — Contraction Reading (Dueling as Cognitively Unthinkable)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This story instantiates the CONTRACTION reading of the
 *   honor_settlement_legitimacy kernel: the claim that dueling did not merely
 *   become illegal or socially disfavored but became cognitively unavailable
 *   as a category of legitimate action for resolving disputes among
 *   status-equals. Under this reading, honor culture as a normative framework
 *   exits the space of live options entirely between roughly 1780 and 1930
 *   across Western Europe and North America — a bourgeois professional class
 *   oriented around courts, contracts, and institutional reputation displaces
 *   the aristocratic honor code that made dueling intelligible. This is
 *   distinct from the sibling drop_reading (which holds that dueling
 *   persisted as a fringe practice among residual honor-culture adherents,
 *   implying the framework never fully vanished, just narrowed) and the
 *   sibling composite_reading (which holds the decline was overdetermined by
 *   multiple reinforcing mechanisms — legal prohibition, insurance actuarial
 *   pressure, military discipline reform, print-culture ridicule — with only
 *   a partial contraction edge, not full framework exit). Each reading is a
 *   separate constraint with its own epsilon; this file claims full
 *   contraction and authors low extraction, near-total accessibility
 *   collapse, and near-zero resistance, consistent with a genuine, though
 *   contestably natural, cognitive phase transition.
 *
 * KEY AGENTS:
 *   - bourgeois_professional_class: Primary beneficiary (institutional/arbitrage) — captures legitimacy and status previously allocated through honor combat, now allocated through professional credentialing and courts
 *   - centralizing_nation_states: Primary beneficiary (institutional/arbitrage) — captures sole legitimate authority over lethal violence and adjudication of insult/injury
 *   - life_insurance_and_actuarial_industries: Secondary beneficiary (institutional/arbitrage) — dueling deaths were uninsurable risk; the framework's exit removes an actuarial anomaly and opens a market
 *   - former_honor_culture_aristocracy: Primary target (powerful, declining/constrained) — loses the framework that once let it settle status disputes and assert rank without appeal to state courts
 *   - dueling_participants_and_seconds_historical: Historical payer class (powerless-in-retrospect, trapped within their own era's norms) — bore the mortal risk the framework required as the price of maintaining status
 *   - legal_and_cultural_historians: Analytical observer — reconstructs whether the shift was cognitive, coercive, or overdetermined
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.18).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.35).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Honor-Settlement Legitimacy Kernel — Contraction Reading (Dueling as Cognitively Unthinkable)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419').
narrative_ontology:cs_kernel_codification('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', implicit).
narrative_ontology:cs_authority_grounding('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', practice).
narrative_ontology:cs_interpretation_layer_present('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419').
narrative_ontology:cs_reading_relation('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', foundational, honor_framework_cognitive_exit_is_total).
narrative_ontology:cs_axiom_status(honor_framework_cognitive_exit_is_total, holdable).
narrative_ontology:cs_axiom_grounding('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', honor_framework_cognitive_exit_is_total, empirically_contingent).
narrative_ontology:cs_axiom('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', secondary, no_present_tense_adherent_class_persists).
narrative_ontology:cs_axiom_status(no_present_tense_adherent_class_persists, holdable).
narrative_ontology:cs_axiom_grounding('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', no_present_tense_adherent_class_persists, empirically_contingent).
narrative_ontology:cs_reference_frame('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', aristocratic_code_duello_authority).
narrative_ontology:cs_drift_state('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', post_bourgeois_consolidation_1900, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('3d8a1ec8-2896-4bd2-ac7a-e91e32ebe419', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, centralizing_nation_states).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, life_insurance_and_actuarial_industries).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, former_honor_culture_aristocracy).
narrative_ontology:constraint_victim(honor_settlement_legitimacy__contraction_reading, dueling_participants_and_seconds_historical).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, state_monopoly_on_legitimate_violence_doctrine).
narrative_ontology:constraint_vindicates(honor_settlement_legitimacy__contraction_reading, rational_dispute_resolution_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gains status and dispute-resolution authority through credentialed profession, courts, and print reputation rather than through willingness to risk mortal combat. As the honor framework exits cognitive availability, this class's model of legitimate status contest becomes the only intelligible one, converting what was once a competing framework into the default without needing to argue for it.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, bourgeois_professional_class, beneficiary,
    institutional, generational, arbitrage, continental).

% Consolidates sole legitimate authority over lethal violence and formal adjudication of insult and injury. Passed statutes and built court infrastructure that both reflected and helped complete the cognitive shift; by the interval's end the state does not need to actively re-suppress dueling case by case because the framework that made dueling intelligible has itself dissolved.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, centralizing_nation_states, beneficiary,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(honor_settlement_legitimacy__contraction_reading, centralizing_nation_states, agenda_setter).

% Dueling deaths were an uninsurable, framework-dependent mortality risk; once honor-based combat exits the space of live options, actuarial tables simplify and a market in life insurance for the professional and mercantile classes opens without needing to underwrite honor-violence risk.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, life_insurance_and_actuarial_industries, beneficiary,
    organized, generational, mobile, continental).

% Historically held status and settled grievances through the honor-duel framework. As the framework exits cognitive availability across the interval, this class loses its distinctive mechanism for asserting rank and answering insult, and finds itself increasingly unable even to frame a duel-challenge as a serious option rather than an anachronism or crime. By the interval's end this class no longer exists as a present-tense claimant on the old framework — its historical loss is real but not an ongoing extraction any living agent currently bears.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, former_honor_culture_aristocracy, payer,
    powerful, biographical, constrained, national).

% Bore the direct mortal risk the honor framework required as the price of maintaining or defending social standing, within the norms of their own historical moment. Their situation is authored as historically bounded — they are not an ongoing victim class under the contraction reading, since the reading's core claim is that no comparable class persists once the framework exits cognition.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, dueling_participants_and_seconds_historical, payer,
    powerless, immediate, trapped, local).

% Reconstructs, from correspondence, legal argumentation, and literature, whether the disappearance of dueling reflects genuine cognitive framework exit, mere suppression of a persisting fringe, or an overdetermined multi-causal decline. Adjudicates between the three sibling readings without holding a stake in any of their outcomes.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, legal_and_cultural_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The historical honor-duel framework solved a real coordination problem for status-equals lacking a trusted third-party adjudicator: how to settle mortal insults without descending into open-ended feud. Its exit under this reading did not leave that problem unsolved — it redefines which grievances count as requiring settlement at all, folding the function into court-adjudicated defamation and professional-reputation systems.
% TRANSFER_FUNCTION: Under contraction, there is no ongoing transfer at the interval's end because the framework being transferred FROM no longer computes as intelligible; the one-time transfer is of legitimacy-granting authority, moving historically from private honor networks and their code-duello arbiters to state courts, professional bodies, and actuarial institutions.
% ABSENT_VOICES: Former honor-culture aristocrats and their descendants might object that something valuable — an autonomous, non-state mechanism for defending personal dignity — was lost rather than superseded, but by the interval's end this constituency has no coherent voice within the framework itself to raise the objection in terms the framework would recognize as intelligible, which is precisely the contraction reading's central claim.
% DISAPPEARANCE_RATIONALE: If this specific constraint (the cognitive unavailability of dueling as legitimate) were to reverse overnight, the modern world would not simply 'rearrange' around a returned option — restoring dueling's intelligibility would require rebuilding an entire cognitive and normative infrastructure (codes of honor, second-based procedure, status-equal recognition rules) that no longer has supporting institutions. Absent that infrastructure, an isolated cognitive reversal changes little in practice; the constraint's disappearance is closer to a null event within the modern institutional world than a rearranging one, which is itself evidence for the mountain-like character claimed here.
% FOUNDING_PROBLEM: How do status-equals settle mortal insults to personal or family honor without the dispute escalating into open feud, in the absence of a trusted, mutually recognized third-party adjudicator with jurisdiction over questions of honor?
% FOUNDING_PROBLEM_CORROBORATION: Legal historians studying the parallel rise of defamation and libel jurisprudence (outside both the aristocratic honor tradition and the professional-class beneficiary set) document that the underlying grievance category of 'insult requiring mortal settlement' itself narrowed and was substantially redefined as a matter for civil and criminal courts, rather than the honor-duel mechanism simply being replaced function-for-function by an equivalent alternative.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) and rising only slightly, because under the contraction reading there is no ongoing extractive transfer once the framework has exited the possibility space — there is no one left paying a toll to a defunct cognitive category. What extraction exists is diffuse and indirect: professional and state actors captured legitimacy-allocation functions previously held by private honor networks, a one-time institutional transfer rather than a continuing rent. Accessibility collapse is authored very high (0.88) because the defining claim of THIS reading is that the alternative (dueling as legitimate) is no longer merely disfavored but unavailable as a coherent option to reason with — this is the structural core of contraction as opposed to drop. Resistance is authored very low (0.08) because if the framework has genuinely exited cognition, there is by definition no live constituency defending dueling as legitimate; residual romanticism or historical reenactment is not resistance to the constraint, it is nostalgia for an extinct one. Theater ratio is kept low and only slowly rising (0.05 to 0.12) since there is minimal ongoing performative enforcement — a genuine mountain does not need to keep re-suppressing something that has become unthinkable; what little theater exists is antiquarian, not enforcement-functional.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (bourgeois professionals, centralizing states, insurers) are declared because the exit of honor culture from the possibility space reallocated legitimacy-granting and risk-pricing functions to them — the FSM gate on this Mountain fires precisely because these beneficiaries exist, and the omega on natural-law-vs-constructed status is the required companion. No victims are declared under this reading because the aristocratic honor-holders and historical duelists are POSITIONED IN THE PAST relative to the interval's endpoint: by 1930 there is no live agent who experiences the unthinkability of dueling as an ongoing extraction, only a historical class that experienced the TRANSITION as loss. This is the key structural feature that differentiates contraction from drop: under drop, a persisting fringe population would be an active, present-tense victim class bearing continued suppression; under contraction, by definition, no coherent present-tense victim class exists because the framework that would define them as aggrieved no longer computes as intelligible even to them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how status-equals settle mortal insults to honor without escalation into feud or war — is authored as DEAD under this reading, not merely resolved by substitution. This blocks the mandatrophy trap of treating a still-live coordination need as if it vanished: courts and professional reputation systems did not merely out-compete dueling as an alternative mechanism for the SAME persisting problem, they redefined what counts as an insult requiring settlement at all, changing the founding problem's shape rather than just its solution. This is corroborated from outside the beneficiary set by legal historians tracing insult-law and defamation jurisprudence, which shows the underlying grievance category itself narrowing, not merely its remedy channel changing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_cognitive_shift,
    'Is the unthinkability of dueling a genuine mountain — an irreversible cognitive/normative phase transition analogous to a phase change in a physical system — or is it a constructed constraint that happens to benefit specific class and state actors who profit from monopolizing legitimate violence and formalizing dispute resolution through courts and insurance?',
    'Comparative historical analysis of societies where honor-dueling frameworks were suppressed by force versus those where the framework genuinely lost cognitive purchase (measured via private correspondence, literature, and legal argumentation no longer treating dueling as an intelligible option even hypothetically). If suppression required continuous active enforcement decades after framework change, the mountain framing is suspect.',
    'If constructed, the beneficiary declarations indicate a false-summit pattern: a professional/state class presenting a contingent institutional victory as an irreversible fact of moral cognition. If genuine, the beneficiaries are incidental downstream winners of a real phase transition, not architects of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_cognitive_shift, conceptual, 'Whether the cognitive-unthinkability claim is a true mountain or a false summit serving bourgeois/state beneficiaries.').

omega_variable(
    kernel_reading_indeterminacy,
    'Does the historical record actually support full exit of honor culture from the normative possibility space (contraction), or does it better fit persistence-as-fringe (drop) or overdetermined multi-causal decline with a contraction edge (composite)?',
    'Systematic review of post-1900 dueling incidents, private codes of honor in military and aristocratic subcultures, and whether any elite actor ever again treats a duel-challenge as a live option requiring refusal rather than as a category error. Absence of even refusal-discourse (nobody bothers explaining why they won''t duel) would support contraction; persistent apologetics or residual practice would support drop.',
    'If evidence supports drop or composite instead of contraction, this specific reading is empirically the weaker of the three siblings and should carry lower confidence weight in any downstream synthesis, though it remains a coherent internally-consistent reading of the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, empirical, 'Whether the historical evidence favors the contraction reading over its sibling readings.').

omega_variable(
    cs_framing_under_determination,
    'Is the correct kernel-object ''honor as a legitimate framework for dispute resolution'' (the obvious framing) or ''the state''s claim to sole legitimate authority over violence, of which anti-dueling norms are one instrument'' (the less obvious framing, which nests this constraint inside a much larger constraint about sovereign monopoly on force)?',
    'Trace whether anti-dueling legal reform preceded or followed broader consolidations of state violence-monopoly (standing armies, police forces, centralized courts) in the same jurisdictions. Sequencing would indicate which framing is causally prior.',
    'Under the narrower framing (honor culture itself), this constraint reads as mountain/natural-cognitive-shift. Under the broader framing (instrument of state monopoly), it reads as tangled_rope — a coordination device (uniform dispute resolution) with asymmetric extraction (state and professional classes capture legitimacy and revenue that dueling parties and their kin networks previously held privately).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Whether the kernel is honor-culture-as-framework or state-violence-monopoly-as-framework, which would change the classification from mountain to tangled_rope.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1780, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1780, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1780, 0.05).
narrative_ontology:measurement(hono_tr_t1810, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1810, 0.06).
narrative_ontology:measurement(hono_tr_t1840, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1840, 0.08).
narrative_ontology:measurement(hono_tr_t1870, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1870, 0.09).
narrative_ontology:measurement(hono_tr_t1900, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1900, 0.11).
narrative_ontology:measurement(hono_tr_t1930, honor_settlement_legitimacy__contraction_reading, theater_ratio, 1930, 0.12).

% Extraction over time
narrative_ontology:measurement(hono_be_t1780, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1780, 0.06).
narrative_ontology:measurement(hono_be_t1810, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1810, 0.09).
narrative_ontology:measurement(hono_be_t1840, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1840, 0.12).
narrative_ontology:measurement(hono_be_t1870, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1870, 0.14).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.16).
narrative_ontology:measurement(hono_be_t1930, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1930, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(honor_settlement_legitimacy__contraction_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_settlement_legitimacy__contraction_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_settlement_legitimacy__contraction_reading, 0.05).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__drop_reading).
narrative_ontology:affects_constraint(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy__composite_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language claim 'dueling declined and honor culture faded' per the ε-invariance principle. drop_reading holds dueling persisted as a fringe practice among residual honor-culture adherents (implying continued, if narrowed, extraction from a live adherent population). composite_reading holds the decline was overdetermined by multiple independently reinforcing mechanisms with only a partial contraction edge. contraction_reading (this file) claims the strongest version: full cognitive exit of the honor framework from the space of legitimate options, authored with correspondingly low, non-rising extraction and very high accessibility collapse. The three should never be merged or averaged; they are linked here so contamination/coupling analysis can trace how evidence bearing on one reading's plausibility propagates to the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
