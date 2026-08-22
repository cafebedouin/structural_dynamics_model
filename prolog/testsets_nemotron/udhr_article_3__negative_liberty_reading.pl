% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_article_3__negative_liberty_reading, []).

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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3 — Negative Liberty Reading
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the negative liberty reading of UDHR
 *   Article 3: 'Everyone has the right to life, liberty and security of
 *   person.' The reading interprets 'security of person' as freedom from
 *   state violence — a procedural shield against arbitrary deprivation. It
 *   generates high extractiveness (0.78) by demanding expansive due process,
 *   restricting self-defense doctrines, and categorically abolishing capital
 *   punishment. These requirements extract operational capacity from the
 *   state security apparatus (the primary payer) and transfer it as
 *   procedural protection to individuals facing state power (primary
 *   beneficiaries). The constraint is a tangled_rope: it performs a genuine
 *   coordination function (defining the legitimate procedural boundary of
 *   state violence) while simultaneously extracting asymmetric costs from
 *   collective security claimants. Active enforcement is required — courts
 *   must continuously police the procedural boundary against legislative and
 *   executive encroachment. The reading coexists with two sibling readings of
 *   the same kernel: the positive entitlement reading
 *   (welfare/housing/healthcare as positive obligations) and the procedural
 *   hybrid reading (due process without substantive resolution).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.78).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.72).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3 — Negative Liberty Reading").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, '04d57b27-d0bb-4d89-8590-180e2f4cb83e').
narrative_ontology:cs_kernel_codification('04d57b27-d0bb-4d89-8590-180e2f4cb83e', fixed_text).
narrative_ontology:cs_authority_grounding('04d57b27-d0bb-4d89-8590-180e2f4cb83e', lineage).
narrative_ontology:cs_interpretation_layer_present('04d57b27-d0bb-4d89-8590-180e2f4cb83e').
narrative_ontology:cs_reading_relation('04d57b27-d0bb-4d89-8590-180e2f4cb83e', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('04d57b27-d0bb-4d89-8590-180e2f4cb83e', udhr_article_3__procedural_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('04d57b27-d0bb-4d89-8590-180e2f4cb83e', foundational, state_shall_not_kill).
narrative_ontology:cs_axiom_status(state_shall_not_kill, holdable).
narrative_ontology:cs_axiom_grounding('04d57b27-d0bb-4d89-8590-180e2f4cb83e', state_shall_not_kill, deontological).
narrative_ontology:cs_axiom('04d57b27-d0bb-4d89-8590-180e2f4cb83e', foundational, procedural_justice_is_constitutive_of_legitimate_deprivation).
narrative_ontology:cs_axiom_status(procedural_justice_is_constitutive_of_legitimate_deprivation, holdable).
narrative_ontology:cs_axiom_grounding('04d57b27-d0bb-4d89-8590-180e2f4cb83e', procedural_justice_is_constitutive_of_legitimate_deprivation, deontological).
narrative_ontology:cs_axiom('04d57b27-d0bb-4d89-8590-180e2f4cb83e', secondary, security_of_person_means_freedom_from_state_violence).
narrative_ontology:cs_axiom_status(security_of_person_means_freedom_from_state_violence, holdable).
narrative_ontology:cs_axiom_grounding('04d57b27-d0bb-4d89-8590-180e2f4cb83e', security_of_person_means_freedom_from_state_violence, conventional).
narrative_ontology:cs_reference_frame('04d57b27-d0bb-4d89-8590-180e2f4cb83e', post_war_human_rights_settlement).
narrative_ontology:cs_drift_state('04d57b27-d0bb-4d89-8590-180e2f4cb83e', contemporary_counterterrorism_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('04d57b27-d0bb-4d89-8590-180e2f4cb83e', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individuals_facing_state_power).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, death_row_prisoners).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, detainees_awaiting_trial).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, political_dissidents).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, state_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, collective_security_claimants).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, victims_of_preventable_crime_due_to_procedural_constraints).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, death_row_prisoners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons subject to arrest, detention, prosecution, or capital sentence by state authorities. The constraint shields them from arbitrary deprivation; their exit from state power is structurally blocked — they cannot opt out of the state's jurisdiction. The negative liberty reading maximizes their protection by demanding narrow procedural pathways before any deprivation.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, individuals_facing_state_power, beneficiary,
    powerless, biographical, trapped, universal).

% The most extreme beneficiaries of the negative liberty reading — their lives are directly at stake in capital punishment abolition. They pay the cost of prolonged incarceration under death sentence while appeals proceed; the reading extracts procedural delays from the state's execution machinery.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, death_row_prisoners, beneficiary,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, death_row_prisoners, payer).

% Pre-trial detainees benefit from expansive due process requirements (habeas corpus, speedy trial, bail presumptions) that the negative liberty reading demands. They bear the cost of detention conditions while the state's procedural obligations are litigated.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, detainees_awaiting_trial, beneficiary,
    powerless, immediate, trapped, universal).

% Activists, journalists, and opposition figures who challenge state power. The negative liberty reading provides their primary structural protection against arbitrary detention, disappearance, or extrajudicial killing. Their exit options are constrained — exile is possible but costly; the reading's force is their main shield within the jurisdiction.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, political_dissidents, beneficiary,
    moderate, biographical, constrained, universal).

% Police, intelligence services, prosecutors, and correctional systems. They bear the operational costs of the negative liberty reading: exhaustive procedural compliance, evidentiary exclusions, habeas corpus litigation, capital punishment abolition. They set the enforcement agenda (can narrow procedural gates through legislation and doctrine) but pay the institutional cost of compliance. Their exit is arbitrage-grade — they can shift tactics, jurisdictions, or legal frameworks.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, state_security_apparatus, payer,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, state_security_apparatus, agenda_setter).

% Communities and individuals who experience crime, terrorism, or disorder that expanded procedural protections may hinder the state from preventing or punishing. They bear diffuse costs when procedural constraints delay or block security operations. Their exit is constrained — they cannot individually opt out of the security environment, but collective political action can shift the balance.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, collective_security_claimants, payer,
    organized, biographical, constrained, national).

% Concrete victims of specific crimes that might have been prevented or solved absent the procedural barriers the negative liberty reading imposes (exclusionary rules, Miranda warnings, warrant requirements). They are trapped in the aftermath; their voice enters only retrospectively through political mobilization.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, victims_of_preventable_crime_due_to_procedural_constraints, payer,
    powerless, immediate, trapped, local).

% Supreme and constitutional courts that interpret and enforce Article 3's negative liberty reading. They administer the constraint's procedural architecture — defining 'arbitrary,' 'due process,' 'narrow exceptions.' They neither collect nor pay the extraction directly; they structure the game. Their analytical exit is the interpretive space itself.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% UN Human Rights Committee, regional courts (ECtHR, IACtHR, AfCHPR), treaty bodies. They monitor state compliance with the negative liberty reading, issue general comments, and adjudicate individual communications. They observe the structural dynamics across jurisdictions; their analytical exit is the comparative framework.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundary between state power and individual existence by establishing a procedural floor: the state may not kill or imprison without a narrow, predefined, and reviewable process. Solves the coordination problem of mutual vulnerability — individuals need protection from arbitrary state violence; the state needs legitimate authority to act.
% TRANSFER_FUNCTION: Transfers operational latitude and coercive efficiency from the state security apparatus to individuals facing state power. The state loses speed, flexibility, and substantive discretion in deprivation decisions; individuals gain procedural shields (notice, hearing, counsel, appeal, proportionality review). Capital punishment abolition transfers the power of life/death from state to a categorical prohibition.
% ABSENT_VOICES: Future generations who will inherit the security architecture shaped by today's procedural balances; victims of state violence in non-signatory or non-compliant states who fall outside the UDHR's enforcement reach; the 'silent majority' who may prefer stronger state security powers but lack organized representation in rights adjudication.
% DISAPPEARANCE_RATIONALE: If the negative liberty reading vanished overnight, states would immediately expand preventive detention, restore capital punishment where abolished, narrow habeas corpus, and weaken exclusionary rules. The security apparatus would regain operational latitude; individuals would lose procedural shields. The world would rearrange toward state-centric security logic within months.
% FOUNDING_PROBLEM: The founding problem was the Nazi and totalitarian precedent of state power unlimited by law — arbitrary arrest, secret detention, extrajudicial killing, and genocide carried out under color of law. Article 3's negative liberty reading was built to answer: how to constitutionally forbid the state from treating human life as disposable?
% FOUNDING_PROBLEM_CORROBORATION: The negative liberty reading's beneficiaries (human rights NGOs, constitutional courts in abolitionist states) attest the founding problem remains live — authoritarian backsliding, extrajudicial killings, and preventive detention regimes persist globally. State security establishments and positive entitlement proponents attest the founding problem is substantially addressed in democratic states and the reading now functions as an obstacle to effective governance and social rights; legislative records and counter-terrorism policy documents from outside the beneficiary set support the shifted-function reading.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_article_3__negative_liberty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_article_3__negative_liberty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the reading categorically forbids capital punishment (removing a state power exercised for centuries), imposes costly procedural superstructures (habeas, Miranda, exclusionary rules, proportionality review), and restricts preventive detention — all extracting substantial resources and operational latitude from the state. Suppression is high (0.72) because the constraint's persistence depends on active judicial enforcement against democratic majorities that often favor broader state security powers; exit for collective security claimants is constrained. Theater ratio is low (0.18) — the procedural machinery is genuinely functional, not performative; courts actually exclude evidence, overturn convictions, and stay executions. Accessibility collapse is moderate (0.42) — alternative security frameworks (restorative justice, community policing, administrative detention regimes) remain conceptually available but are legally foreclosed by the reading's dominance in constitutional doctrine. Resistance is high (0.68) — states consistently resist through legislative overrides, emergency derogations, non-compliance, and doctrinal narrowing.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (state security apparatus) experiences the constraint as extraction of operational capacity — every procedural requirement is a friction on their core function. The beneficiary seats (individuals, death row prisoners, dissidents) experience it as the only structural barrier between them and arbitrary state violence. The victim seats (collective security claimants, crime victims) experience it as a shield for perpetrators at their expense. The constitutional court seat experiences it as the interpretive architecture they must maintain — neither pure extraction nor pure coordination, but the institutional burden of drawing the line. The engine computes these divergences from the authored power/exit/scope data.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the structural positions: individuals facing state power are trapped (exit_options: trapped) and powerless — they are full beneficiaries (d ~ 0.0) because the constraint subsidizes their existence against state violence. Death row prisoners are the extreme case — the constraint literally prevents their execution. The state security apparatus is institutional with arbitrage exit — they administer the constraint but pay its costs (d ~ 0.7-0.8). Collective security claimants are organized but constrained — they bear diffuse costs without controlling the constraint (d ~ 0.6). Constitutional courts are institutional/analytical — they structure the game (d ~ 0.5 symmetric). The engine will compute per-seat effective extraction from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The negative liberty reading's founding problem (totalitarian arbitrary state violence) remains contested — live in authoritarian contexts, arguably addressed in stable democracies. In the latter, the reading risks mandatrophy: its procedural architecture persists and expands (more exclusions, broader habeas, categorical abolition) while the original tyranny it answered has receded. The extraction accumulates (measurements show rising epsilon 1948→2025) as courts elaborate new procedural rights without sunset. Yet the reading is not a piton — it retains genuine coordination function (democratic states still need procedural legitimacy) and active enforcement (courts actively apply it). The mandatrophy tension is real but unresolved: is the expanding procedural superstructure still solving the founding problem, or has it become a self-justifying extraction machine? The founding_problem_status = contested captures this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the negative liberty reading of Article 3 a distinct constraint from the positive entitlement and procedural hybrid readings, or are they measurement perspectives on a single constraint?',
    'Apply the ε-invariance test: if measuring Article 3 compliance via capital punishment abolition yields ε ≈ 0.78, but measuring via healthcare provision yields ε ≈ 0.35, they are different constraints with different extraction profiles. The test is whether the beneficiary/victim structures differ structurally.',
    'If they are one constraint, the classification must reconcile the conflicting extraction profiles. If they are three constraints (per ε-invariance), each gets its own story, its own ε, its own classification — linked by network.affects_constraints. This story assumes the latter per DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings are structurally distinct constraints or observer perspectives on one constraint.').

omega_variable(
    procedural_coordination_vs_extraction_boundary,
    'Where does the genuine coordination function of procedural due process end and asymmetric extraction from collective security begin?',
    'Comparative analysis of jurisdictions that have relaxed specific procedural requirements (e.g., narrowed exclusionary rules, shortened habeas timelines) without collapsing rule of law — measuring crime prevention gains vs. rights erosion.',
    'If the coordination function saturates at a lower procedural threshold, the marginal extraction above that threshold is pure snare-component. If coordination requires the full current superstructure, the extraction is the price of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_coordination_vs_extraction_boundary, empirical, 'The structural boundary between coordination and extraction within the negative liberty reading''s procedural architecture.').

omega_variable(
    capital_punishment_abolition_as_extraction_or_coordination,
    'Is categorical capital punishment abolition a coordination achievement (establishing a civilizational floor) or an extraction from state penal sovereignty that serves no coordination function?',
    'Historical analysis: did abolition correlate with reduced state killing overall, or merely shift it to extrajudicial/war contexts? Does the abolition norm coordinate international human rights behavior, or merely express a moral preference of powerful states?',
    'If coordination, abolition''s extraction from state penal power is the price of a genuine civilizational standard. If extraction, it is a normative preference imposed on states that retain death penalty for deterrence/retribution — a tangled_rope component that could be a snare if enforced without consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_punishment_abolition_as_extraction_or_coordination, preference, 'Whether capital punishment abolition is a coordination floor or an extractive imposition.').

omega_variable(
    collective_security_victim_structure,
    'Are collective security claimants a coherent victim class with shared interests, or a constructed category that masks state power''s self-interest?',
    'Disaggregate ''collective security'': measure crime victimization rates across demographics, compare with procedural constraint impacts, test whether security rhetoric correlates with actual protection of vulnerable populations or with state capacity expansion.',
    'If collective security claimants are a genuine victim class, the tangled_rope''s asymmetric extraction is real and the constraint must account for their costs. If they are a state-constructed cover, the victim declaration overstates the snare component and the constraint is more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_security_victim_structure, empirical, 'Whether the victim side of the tangled rope is structurally real or politically constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_art3_neg__tr_t1948, udhr_article_3__negative_liberty_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement(udhr_art3_neg__tr_t1966, udhr_article_3__negative_liberty_reading, theater_ratio, 1966, 0.14).
narrative_ontology:measurement(udhr_art3_neg__tr_t1976, udhr_article_3__negative_liberty_reading, theater_ratio, 1976, 0.16).
narrative_ontology:measurement(udhr_art3_neg__tr_t1989, udhr_article_3__negative_liberty_reading, theater_ratio, 1989, 0.15).
narrative_ontology:measurement(udhr_art3_neg__tr_t2000, udhr_article_3__negative_liberty_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement(udhr_art3_neg__tr_t2010, udhr_article_3__negative_liberty_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(udhr_art3_neg__tr_t2025, udhr_article_3__negative_liberty_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(udhr_art3_neg__be_t1948, udhr_article_3__negative_liberty_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(udhr_art3_neg__be_t1966, udhr_article_3__negative_liberty_reading, base_extractiveness, 1966, 0.48).
narrative_ontology:measurement(udhr_art3_neg__be_t1976, udhr_article_3__negative_liberty_reading, base_extractiveness, 1976, 0.58).
narrative_ontology:measurement(udhr_art3_neg__be_t1989, udhr_article_3__negative_liberty_reading, base_extractiveness, 1989, 0.65).
narrative_ontology:measurement(udhr_art3_neg__be_t2000, udhr_article_3__negative_liberty_reading, base_extractiveness, 2000, 0.72).
narrative_ontology:measurement(udhr_art3_neg__be_t2010, udhr_article_3__negative_liberty_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(udhr_art3_neg__be_t2025, udhr_article_3__negative_liberty_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(udhr_art3_neg__su_t1948, udhr_article_3__negative_liberty_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(udhr_art3_neg__su_t1966, udhr_article_3__negative_liberty_reading, suppression_requirement, 1966, 0.62).
narrative_ontology:measurement(udhr_art3_neg__su_t1976, udhr_article_3__negative_liberty_reading, suppression_requirement, 1976, 0.68).
narrative_ontology:measurement(udhr_art3_neg__su_t1989, udhr_article_3__negative_liberty_reading, suppression_requirement, 1989, 0.65).
narrative_ontology:measurement(udhr_art3_neg__su_t2000, udhr_article_3__negative_liberty_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(udhr_art3_neg__su_t2010, udhr_article_3__negative_liberty_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(udhr_art3_neg__su_t2025, udhr_article_3__negative_liberty_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__negative_liberty_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, udhr_article_3__procedural_hybrid_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, iccpr_article_6_capital_punishment).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, iccpr_article_9_arbitrary_detention).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, echr_article_2_right_to_life).

% DUAL FORMULATION NOTE:
% This story decomposes the colloquial 'Article 3 right to life/liberty/security' into three structurally distinct constraints linked by the kernel udhr_article_3. The negative liberty reading (this story) has high epsilon (0.78) and is tangled_rope. The positive entitlement reading would have different beneficiaries (welfare claimants) and victims (taxpayers/fiscal authorities) with its own epsilon. The procedural hybrid reading would have lower epsilon (procedural coordination without substantive extraction) and likely classify as rope. The three readings compete in the same doctrinal space but extract from different parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_article_3__negative_liberty_reading, institutional, 0.75).
constraint_indexing:directionality_override(udhr_article_3__negative_liberty_reading, powerless, 0.05).
constraint_indexing:directionality_override(udhr_article_3__negative_liberty_reading, organized, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
