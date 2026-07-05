% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__composite_reading, []).

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
 *   constraint_id: honor_satisfaction_mechanism__composite_reading
 *   human_readable: Honor-Satisfaction Mechanism (Composite Erosion Reading): Dueling as Overdetermined by State Monopoly, Bourgeois Norms, Insurance, and Category-Shift
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   This story is the composite reading of the honor-satisfaction-mechanism
 *   kernel: it treats the historical decline of dueling not as a single-cause
 *   process but as the joint, overdetermined product of four independently
 *   operating pressures — state consolidation of legitimate violence via
 *   criminal prosecution, the rise of bourgeois respectability norms that
 *   substituted a new status hierarchy for the aristocratic one, the
 *   actuarial logic of life-insurance exclusion clauses that made dueling
 *   financially punishing for dependents, and a category-shift in which
 *   'honor defense' itself was reclassified from combat into etiquette and
 *   litigation. No single mechanism is claimed sufficient; the reading's
 *   structural claim is that these four channels, none individually decisive,
 *   jointly dismantled the coordination function dueling once served. This
 *   differs sharply from the decline_reading (gradual frequency reduction to
 *   fringe status under an implicit single frequency-metric) and the
 *   contraction_reading (a hard cognitive-category collapse making dueling
 *   literally unthinkable) — this reading instead holds the practice remained
 *   thinkable but became structurally unsupportable across several fronts at
 *   once, which is why its extraction trajectory rises steadily rather than
 *   showing a discrete inflection.
 *
 * KEY AGENTS:
 *   - consolidating_nation_states: agenda_setter (institutional/analytical) — builds monopoly on legitimate violence via criminalization
 *   - life_insurance_industry: beneficiary (organized/arbitrage) — prices dueling out via exclusion clauses
 *   - rising_bourgeois_professional_class: beneficiary/agenda_setter (organized/mobile) — displaces aristocratic status marker with a competing one
 *   - dueling_code_arbiters: beneficiary/agenda_setter (moderate/constrained) — professionalizes as category shifts from combat to etiquette management
 *   - aristocratic_officer_class: payer (powerful/constrained) — loses distinctive honor mechanism across multiple fronts simultaneously
 *   - dueling_participants: payer (moderate/trapped) — bears compounding legal, financial, and mortal risk
 *   - widows_and_dependents_of_duelists: payer (powerless/trapped) — absorbs uninsured financial catastrophe with no voice
 *   - legal_historians: observer (analytical/analytical) — traces the overdetermined, multi-channel erosion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, 0.58).
domain_priors:suppression_score(honor_satisfaction_mechanism__composite_reading, 0.71).
domain_priors:theater_ratio(honor_satisfaction_mechanism__composite_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__composite_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__composite_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__composite_reading, "Honor-Satisfaction Mechanism (Composite Erosion Reading): Dueling as Overdetermined by State Monopoly, Bourgeois Norms, Insurance, and Category-Shift").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__composite_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__composite_reading, 'fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9').
narrative_ontology:cs_kernel_codification('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', distributed).
narrative_ontology:cs_authority_grounding('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', practice).
narrative_ontology:cs_interpretation_layer_present('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9').
narrative_ontology:cs_reading_relation('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', honor_satisfaction_mechanism__decline_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', honor_satisfaction_mechanism__contraction_reading, influences).
narrative_ontology:cs_axiom('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', foundational, erosion_is_multiply_overdetermined).
narrative_ontology:cs_axiom_status(erosion_is_multiply_overdetermined, holdable).
narrative_ontology:cs_axiom_grounding('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', erosion_is_multiply_overdetermined, empirically_contingent).
narrative_ontology:cs_axiom('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', secondary, practice_remained_cognitively_available_throughout).
narrative_ontology:cs_axiom_status(practice_remained_cognitively_available_throughout, holdable).
narrative_ontology:cs_axiom_grounding('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', practice_remained_cognitively_available_throughout, empirically_contingent).
narrative_ontology:cs_reference_frame('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', elite_extrajudicial_honor_adjudication).
narrative_ontology:cs_drift_state('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', state_consolidated_professional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fe26ba8f-e82d-4a62-bb9d-5ad3972bd0d9', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, consolidating_nation_states).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, life_insurance_industry).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, rising_bourgeois_professional_class).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__composite_reading, dueling_code_arbiters).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, aristocratic_officer_class).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, dueling_participants).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__composite_reading, widows_and_dependents_of_duelists).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, state_monopoly_on_legitimate_violence).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__composite_reading, bourgeois_reputational_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Criminalizes dueling, prosecutes participants selectively, and builds courts and police as the exclusive legitimate channel for settling honor disputes. Gains a monopoly on organized violence and removes a rival adjudication system that had operated outside state control. Enforcement intensity varies by jurisdiction and class of defendant.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, consolidating_nation_states, agenda_setter,
    institutional, generational, analytical, national).

% Writes dueling-exclusion clauses into life policies, making death-by-duel financially catastrophic for a duelist's dependents. Collects premiums under an actuarial logic that treats dueling as a rateable, excludable risk, converting an honor practice into an insurance-underwriting problem and quietly pricing it out of viability for the economically exposed.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, life_insurance_industry, beneficiary,
    organized, generational, arbitrage, national).

% Displaces aristocratic honor codes with respectability norms centered on reputation-via-commerce, credentialing, and print scandal rather than blood satisfaction. Benefits by delegitimizing a status marker (readiness to duel) that the old aristocracy monopolized, replacing it with a status marker (respectability, litigation, public apology) that the professional class can win at.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, rising_bourgeois_professional_class, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, rising_bourgeois_professional_class, agenda_setter).

% Codes of honor (codes duello) formalize seconds, procedures, and grounds for satisfaction, professionalizing a role for arbiters and seconds who administer disputes. As the practice erodes, this role shifts into policing 'acceptable apology' and adjudicating whether an affront has been sufficiently withdrawn without combat, retaining social function while the underlying violence recategorizes into etiquette management.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, dueling_code_arbiters, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__composite_reading, dueling_code_arbiters, agenda_setter).

% Loses its distinctive mechanism for defending honor and rank as the state criminalizes it, insurers price against it, and bourgeois norms make it look archaic rather than noble. Cannot simply decline to participate without status loss inside a still-operating subculture, yet participation now carries legal, financial, and increasingly reputational costs across multiple independent channels at once.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, aristocratic_officer_class, payer,
    powerful, biographical, constrained, national).

% Individuals who accept a challenge under continuing social pressure absorb the actual physical risk of the encounter, now compounded by exposure to criminal prosecution and loss of insurance payout for dependents. The mechanism that once offered unambiguous satisfaction now carries stacked, hard-to-price extractive costs on top of the mortal risk it always carried.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, dueling_participants, payer,
    moderate, immediate, trapped, local).

% Bear the financial consequence when a duel proves fatal and the insurance exclusion clause voids the policy, receiving neither the honor-restoration the duel supposedly secured nor the financial protection an ordinary death would have carried. Have no voice in whether the duel occurs and no recourse once it has.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, widows_and_dependents_of_duelists, payer,
    powerless, biographical, trapped, local).

% Trace how state prosecution records, insurance actuarial tables, etiquette manuals, and newspaper scandal coverage jointly account for the decline of dueling without any single mechanism being sufficient on its own — the analytical seat from which the composite reading is legible as overdetermination rather than a single cause.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__composite_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Historically, the duel-and-satisfaction system coordinated honor disputes among status-equals outside formal courts, providing a procedure (challenge, seconds, terms, field) that let elites resolve reputational injury without endless private feud or reliance on a monarch's court. The composite erosion reading concerns what replaced and dismantled that coordination function via several independent channels simultaneously.
% TRANSFER_FUNCTION: Under the composite reading, the erosion mechanism transfers legitimacy and adjudicative authority away from private honor codes and toward the state (monopoly on violence), toward insurers (who price and thereby govern acceptable risk), and toward bourgeois status markers (who redefine what counts as respectable) — extracting standing from the aristocratic officer class and imposing residual financial risk onto duelists' dependents, while consolidating authority in states, insurers, and the professional class.
% ABSENT_VOICES: Widows and dependents of duelists have no procedural voice in the code duello itself, and no seat in state prosecution decisions or insurance underwriting tables; their exclusion is structural rather than incidental — the entire apparatus (state, code, insurer) treats them as downstream absorbers of risk, never as parties whose consent the mechanism requires.
% DISAPPEARANCE_RATIONALE: If the composite erosion mechanism (state prosecution, insurance exclusion, bourgeois respectability norms, and the category-shift of honor into etiquette) had not operated, dueling as an elite adjudication practice would plausibly have persisted far longer among status-equals with continuing incentive to defend rank through combat; removing any one erosion channel alone (say, insurance exclusion) would not have been sufficient, which is precisely the composite reading's structural claim — multiple independent pressures, not one dominant cause, dismantled the practice.
% FOUNDING_PROBLEM: Elite honor disputes needed a legitimate, rule-governed procedure for restoring reputational standing among status-equals without either endless private feud or subordination to a monarch's courts that many elites did not fully trust or control.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (an analytical seat outside the aristocratic officer class, the insurance industry, and the bourgeois professional class) attest via prosecution records, insurance actuarial archives, and etiquette-manual textual analysis that the founding problem — lack of a trusted non-state elite adjudication procedure — was resolved not by any beneficiary group's self-interested account but by the convergent, independently documentable operation of state judicial consolidation, actuarial risk-pricing, and status-marker substitution; no party currently benefiting from the practice's disappearance is the sole source of this genealogy.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__composite_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__composite_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__composite_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(honor_satisfaction_mechanism__composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(honor_satisfaction_mechanism__composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.22 to 0.58 over the interval because the mechanism increasingly functions to strip standing and resources from the aristocratic class and duelists' dependents across four simultaneously operating channels, rather than any one channel alone driving the change. Suppression rises even faster (0.30 to 0.71) because state prosecution, insurance underwriting refusal, and social delegitimization are each independently coercive, and their combination compounds rather than substitutes. Theater ratio rises meaningfully (0.10 to 0.42) because as fatal duels become rarer under the compounding pressure, the surviving apparatus — codes duello, formal apology procedures, ritualized 'satisfaction' via retraction rather than combat — increasingly performs honor-restoration without underwriting real physical risk, exactly the category-shift component of this reading.
 *
 * PERSPECTIVAL GAP:
 *   From the state's seat, this composite mechanism looks like successful monopolization of legitimate violence — an unambiguous institutional win. From the aristocratic officer class's seat, the same period looks like simultaneous assault from law, finance, and social norms, none of which they individually consented to or could resist as a unified front. The engine should compute these as structurally different experiences of the same underlying erosion process, which is the core evidentiary content of choosing the composite reading over the single-cause decline or contraction readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Consolidating states, insurers, and the bourgeois professional class are beneficiaries with low derived directionality: each gains authority, revenue, or status-hierarchy dominance from the mechanism's operation, and each has meaningful exit or arbitrage options (states set the rules, insurers price the risk, the bourgeois class simply adopts the new respectability norms). The aristocratic officer class and dueling participants sit at high directionality: they bear compounding legal, financial, and mortal costs with constrained or trapped exit, since declining to duel under continuing social pressure itself carried status costs during the transition period. Widows and dependents are the purest target seat: fully powerless, trapped, and structurally voiceless, receiving none of the coordination benefit the original duel procedure was meant to provide.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a trusted non-state elite adjudication procedure for honor disputes) is dead by 1900 — states now fully monopolize legitimate dispute resolution, and the professional class no longer needs combat-based status validation. But the residual apparatus (codes of etiquette, formal-apology procedures, arbiters who once seconded duels now negotiating public retractions) persists as theater, which is why theater_ratio climbs steadily. This is the composite reading's mandatrophy signature: not one clean function-obsolescence event but a gradual sedimentation of theatrical residue across several formerly distinct mechanisms as each is separately defunded of its original coercive force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    single_cause_vs_composite_sufficiency,
    'Would any one of the four erosion channels (state prosecution, insurance exclusion, bourgeois norms, category-shift) have been sufficient alone to dismantle the dueling-satisfaction mechanism, or was the joint operation of multiple channels structurally necessary?',
    'Comparative historical analysis across jurisdictions where one channel operated without the others (e.g., regions with strong state prosecution but weak insurance markets, or vice versa) to test whether dueling persisted longer where fewer channels were active.',
    'If any single channel proves sufficient on its own in comparative cases, the composite reading collapses into (or is subsumed by) the decline_reading''s single-metric account; if no single channel is sufficient anywhere, the composite reading''s overdetermination claim is strongly corroborated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(single_cause_vs_composite_sufficiency, empirical, 'Whether the composite reading''s core overdetermination claim holds against comparative jurisdictional evidence.').

omega_variable(
    cognitive_category_collapse_timing,
    'At what point, if any, did dueling shift from ''structurally disadvantaged option still within the space of live options'' to ''cognitively unthinkable category'' — and does that shift, if it occurred, belong to this reading or displace it into the contraction_reading?',
    'Close reading of late-period etiquette manuals, newspaper editorials, and legal commentary for language indicating dueling had become inconceivable (category-level exclusion) versus merely disreputable-but-imaginable (structural disadvantage, consistent with composite reading).',
    'If evidence shows a genuine cognitive-category collapse at some point within the interval, this composite reading may only accurately describe the earlier portion of the timeline, with the contraction_reading taking over for the later portion — suggesting the two readings are sequential rather than purely alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_category_collapse_timing, conceptual, 'Whether the composite and contraction readings are mutually exclusive framings or sequential phases requiring temporal boundary-drawing.').

omega_variable(
    insurance_causation_or_symptom,
    'Did life-insurance dueling-exclusion clauses independently cause erosion of the practice, or were they merely a downstream symptom of bourgeois norms already devaluing dueling (i.e., insurers priced against a practice already losing status, rather than driving its decline)?',
    'Archival dating of exclusion-clause introduction relative to independent measures of dueling frequency and status decline in the same jurisdictions; if clauses precede frequency decline, causal weight is stronger.',
    'If insurance exclusion is merely symptomatic rather than independently causal, the composite reading''s claim of four INDEPENDENT mechanisms weakens to three independent plus one derivative, altering the extractiveness attribution across beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_causation_or_symptom, empirical, 'Whether the insurance-industry beneficiary channel is causally independent or a downstream artifact of the bourgeois-norms channel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__composite_reading, 1750, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1750, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1750, 0.1).
narrative_ontology:measurement(hono_tr_t1780, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1780, 0.15).
narrative_ontology:measurement(hono_tr_t1810, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1810, 0.24).
narrative_ontology:measurement(hono_tr_t1840, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1840, 0.32).
narrative_ontology:measurement(hono_tr_t1870, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1870, 0.38).
narrative_ontology:measurement(hono_tr_t1900, honor_satisfaction_mechanism__composite_reading, theater_ratio, 1900, 0.42).

% Extraction over time
narrative_ontology:measurement(hono_be_t1750, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1750, 0.22).
narrative_ontology:measurement(hono_be_t1780, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1780, 0.3).
narrative_ontology:measurement(hono_be_t1810, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1810, 0.38).
narrative_ontology:measurement(hono_be_t1840, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1840, 0.47).
narrative_ontology:measurement(hono_be_t1870, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1870, 0.54).
narrative_ontology:measurement(hono_be_t1900, honor_satisfaction_mechanism__composite_reading, base_extractiveness, 1900, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1750, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1750, 0.3).
narrative_ontology:measurement(hono_su_t1780, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1780, 0.38).
narrative_ontology:measurement(hono_su_t1810, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1810, 0.5).
narrative_ontology:measurement(hono_su_t1840, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1840, 0.6).
narrative_ontology:measurement(hono_su_t1870, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1870, 0.67).
narrative_ontology:measurement(hono_su_t1900, honor_satisfaction_mechanism__composite_reading, suppression_requirement, 1900, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__composite_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__composite_reading, 0.1).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__decline_reading).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__composite_reading, honor_satisfaction_mechanism__contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the honor_satisfaction_mechanism kernel (composite_reading, decline_reading, contraction_reading), each authored as a structurally distinct constraint per the ε-invariance principle rather than as one story with a measurement parameter. The composite reading's ε (0.58 by 1900) reflects overdetermined multi-channel erosion; the decline_reading's ε reflects a single-metric frequency-decay account; the contraction_reading's ε reflects a hard category-collapse account. All three are linked bidirectionally in this network so contamination-propagation analysis can trace how evidence bearing on one reading structurally pressures the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__composite_reading, powerful, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
