% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity of Life Prohibition on Assisted Dying
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity reading of end-of-life authority asserts that human life
 *   possesses intrinsic value that categorically prohibits intentional
 *   life-ending, regardless of individual preference or suffering. This
 *   reading instantiates a constraint that operates through criminal law,
 *   medical licensing, and professional ethics to prevent assisted dying and
 *   euthanasia. The constraint claims mountain status — a natural law of
 *   morality — but its operation requires active enforcement (suppression
 *   0.82), extracts autonomy from terminal patients (extractiveness 0.68),
 *   and benefits identifiable institutional actors (religious institutions,
 *   pro-life organizations, palliative care establishment). The
 *   pressured-vulnerable populations (elderly, disabled, economically
 *   disadvantaged) are authored as victims (payers) under this reading's own
 *   structural logic: they bear the cost of prolonged suffering the
 *   constraint mandates, and their 'protection' is the constraint's cover
 *   story. The engine will compute per-seat classifications from this
 *   structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.82).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, mountain).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity of Life Prohibition on Assisted Dying").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).
domain_priors:emerges_naturally(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, '9b4d7e84-23fe-4135-9094-96fceff8c62c').
narrative_ontology:cs_kernel_codification('9b4d7e84-23fe-4135-9094-96fceff8c62c', fixed_text).
narrative_ontology:cs_authority_grounding('9b4d7e84-23fe-4135-9094-96fceff8c62c', lineage).
narrative_ontology:cs_interpretation_layer_present('9b4d7e84-23fe-4135-9094-96fceff8c62c').
narrative_ontology:cs_reading_relation('9b4d7e84-23fe-4135-9094-96fceff8c62c', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('9b4d7e84-23fe-4135-9094-96fceff8c62c', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('9b4d7e84-23fe-4135-9094-96fceff8c62c', foundational, human_life_has_intrinsic_value).
narrative_ontology:cs_axiom_status(human_life_has_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('9b4d7e84-23fe-4135-9094-96fceff8c62c', human_life_has_intrinsic_value, deontological).
narrative_ontology:cs_axiom('9b4d7e84-23fe-4135-9094-96fceff8c62c', foundational, intentional_life_ending_is_categorically_prohibited).
narrative_ontology:cs_axiom_status(intentional_life_ending_is_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('9b4d7e84-23fe-4135-9094-96fceff8c62c', intentional_life_ending_is_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('9b4d7e84-23fe-4135-9094-96fceff8c62c', secondary, physician_role_excludes_killing).
narrative_ontology:cs_axiom_status(physician_role_excludes_killing, holdable).
narrative_ontology:cs_axiom_grounding('9b4d7e84-23fe-4135-9094-96fceff8c62c', physician_role_excludes_killing, conventional).
narrative_ontology:cs_reference_frame('9b4d7e84-23fe-4135-9094-96fceff8c62c', sanctity_of_life_framework).
narrative_ontology:cs_drift_state('9b4d7e84-23fe-4135-9094-96fceff8c62c', contemporary_bioethics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9b4d7e84-23fe-4135-9094-96fceff8c62c', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_institutions_upholding_sanctity).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pro_life_advocacy_organizations).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, palliative_care_establishment).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, pressured_vulnerable_populations).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminal_patients_seeking_assisted_dying).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, physicians_constrained_by_prohibition).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, intrinsic_value_of_human_life).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, categorical_prohibition_on_intentional_killing).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, physician_role_as_life_preserver).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elderly, disabled, and economically disadvantaged individuals who the sanctity reading claims to protect from coercion into assisted dying. Under this constraint, they are denied the option of assisted dying even when they autonomously request it, bearing the cost of prolonged suffering that the prohibition mandates. Their identity as 'vulnerable' is fused with the constraint's protective framing, making exit from the protected status conceptually unavailable.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pressured_vulnerable_populations, payer,
    powerless, biographical, identity_locked, national).

% Competent terminal patients experiencing unbearable suffering who would choose assisted dying if legally permitted. The categorical prohibition denies them this option entirely. They cannot exit the constraint's reach because the prohibition is enforced through criminal law and medical licensing; traveling to jurisdictions where assisted dying is legal requires resources and capacity many lack.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminal_patients_seeking_assisted_dying, payer,
    powerless, immediate, trapped, national).

% Physicians whose professional role is defined by the constraint as exclusively life-preserving. They administer the constraint by refusing assisted dying requests and providing palliative alternatives. Some experience moral injury from being unable to honor patient requests; others embrace the role as consonant with their professional identity. Exit means leaving clinical practice or relocating to permissive jurisdictions.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physicians_constrained_by_prohibition, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, physicians_constrained_by_prohibition, payer).

% Religious bodies (particularly Catholic, Orthodox, and conservative Protestant institutions) that ground their authority in the sanctity-of-life doctrine. The constraint vindicates their theological anthropology and sustains their institutional relevance in bioethical debates. They actively lobby to maintain prohibition and shape palliative care infrastructure.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_institutions_upholding_sanctity, beneficiary,
    institutional, civilizational, arbitrage, global).

% Secular and religious advocacy groups that organize political opposition to assisted dying legislation. They benefit from the constraint's existence as a mobilizing cause and a source of donor support. Their exit options include shifting focus to adjacent issues (abortion, embryo research) if the constraint were relaxed.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pro_life_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Medical specialty and institutional infrastructure built around managing dying without hastening death. The constraint secures their professional jurisdiction and funding model. They develop clinical guidelines that operationalize the prohibition. Exit would require restructuring the entire specialty around a different paradigm of end-of-life care.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, palliative_care_establishment, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, palliative_care_establishment, agenda_setter).

% Legislative bodies that enact and maintain criminal prohibitions on assisted dying. They bear the enforcement cost (policing, prosecution, medical board oversight) but gain legitimacy with religious and traditionalist constituencies. They can repeal the prohibition (as several jurisdictions have), but doing so triggers intense political conflict.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, state_legislature, agenda_setter,
    institutional, generational, mobile, national).

% Disability rights organizations (split between those fearing coercion and those demanding autonomy), civil liberties groups, and right-to-die societies. They are excluded from the constraint's internal logic because the sanctity reading treats autonomy claims as irrelevant to the intrinsic value prohibition. They contest the constraint through litigation, legislative campaigns, and public advocacy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_advocates, excluded,
    organized, biographical, mobile, national).

% Scholars who analyze the constraint's structure, history, and effects without being subject to its enforcement or benefiting from its rents. They observe the seat divergence: the constraint computes as mountain for religious institutions, tangled_rope for physicians, snare for terminal patients, and piton for legislatures in jurisdictions where public opinion has shifted but laws remain.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, analytical_bioethicist, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared moral framework that treats human life as inviolable, preventing a 'race to the bottom' where vulnerable lives are deemed less worth living. Solves the coordination problem of mutual assurance: no one need fear that their dependence or disability makes them a candidate for elimination.
% TRANSFER_FUNCTION: Transfers the burden of suffering from the collective (which would bear the moral weight of permitting killing) to individual patients who must endure terminal suffering without the option of assisted death. Transfers professional authority from physicians (who lose discretion over end-of-life decisions) to the state and religious doctrine.
% ABSENT_VOICES: Competent terminal patients who would choose assisted dying but cannot speak because they are dead or silenced by suffering. Future cohorts who will face the same prohibition. Disabled individuals who support assisted dying as an autonomy right but are excluded from the 'vulnerable' category the constraint claims to protect.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, jurisdictions would rapidly enact regulated assisted dying frameworks (as seen in Canada, Netherlands, Oregon). Physicians would gain discretion, palliative care would restructure around patient choice, religious institutions would lose a central bioethical battleground, and the moral framework of 'inviolable life' would fracture into contested policy debates.
% FOUNDING_PROBLEM: The horror of state-sanctioned killing revealed by Nazi euthanasia programs (Aktion T4) and eugenics movements, which demonstrated how quickly 'mercy killing' expands to eliminate 'lives unworthy of life.' The sanctity constraint was rebuilt post-WWII as a categorical firewall against any state or medical authority deciding which lives are worth living.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on Nazi medicine (e.g., Burleigh, Proctor) corroborates the founding horror. However, disability rights scholars (e.g., Shakespeare, Singer controversy) and autonomy advocates attest that the founding problem is contested: they argue modern regulated assisted dying frameworks contain safeguards that prevent the slippery slope, and that the categorical prohibition now serves a different function — protecting institutional authority rather than vulnerable lives.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, ExtMetricName, E),
    domain_priors:suppression_score(end_of_life_authority__sanctity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(end_of_life_authority__sanctity_reading),
    narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: The constraint transfers the full burden of terminal suffering to patients who would choose assisted dying, while the coordination benefit (mutual assurance against elimination) is diffuse and hypothetical. Suppression 0.82: The prohibition is maintained by criminal statutes, medical board sanctions, and institutional policies that actively prevent assisted dying — not by spontaneous compliance. Theater 0.25: The palliative care infrastructure is genuine coordination, but a growing share of enforcement activity defends the categorical prohibition against autonomy-based challenges rather than serving patient care. Accessibility_collapse 0.78: Once the sanctity premise is accepted, alternatives (autonomy-based frameworks) appear morally incoherent. Resistance 0.45: Organized opposition exists (autonomy advocates, some disability rights groups) but has achieved legal change in only a minority of jurisdictions over 55 years.
 *
 * PERSPECTIVAL GAP:
 *   The religious institutional seat computes mountain: the constraint is divine law, extraction is zero, suppression is zero (voluntary compliance). The terminal patient seat computes snare: categorical prohibition with no exit, identifiable victim, active enforcement. The physician seat computes tangled_rope: genuine coordination (palliative care) fused with asymmetric extraction (denied discretion). The legislature seat in post-shift jurisdictions computes piton: the constraint persists by inertia despite majority support for assisted dying, maintained by organized minority pressure. The engine computes this divergence; the authored claim (mountain) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and pro-life advocates are structural beneficiaries (d near 0.0) — they collect legitimacy, mobilization, and institutional relevance from the constraint. Pressured-vulnerable populations and terminal patients are structural targets (d near 1.0) — they bear the extraction (denied autonomy, prolonged suffering) with trapped/identity_locked exit. Physicians are agenda_setters who also pay (secondary_role: payer) — they administer the constraint but suffer moral injury and professional constraint (d ~0.6). The state legislature is an agenda_setter with mobile exit (can repeal) but faces high political cost (d ~0.4). Autonomy advocates are excluded (not in the constraint's internal logic). The analytical observer sits at d=0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state-sanctioned elimination of 'unworthy lives') was live in 1947-1970. By 2025, regulated assisted dying frameworks in multiple jurisdictions have operated for decades without the predicted slide into eugenics. The constraint persists despite the founding problem's empirical mitigation — a classic mandatrophy signal. However, the sanctity reading's proponents contest the founding problem's status (claiming slippery slope evidence in Netherlands/Belgium), so the engine's mandatrophy detection must weigh corroboration from outside the beneficiary set (historical scholarship vs. advocacy claims).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the end_of_life_authority kernel, or a strategic deployment of the sanctity concept to block autonomy-based reforms?',
    'Compare the constraint''s operational structure across jurisdictions: if the prohibition''s enforcement intensity tracks religious institutional power rather than vulnerable-population outcomes, it is strategic deployment. If enforcement correlates with measurable reductions in coercion deaths, it is a genuine reading.',
    'If strategic deployment, the constraint is a snare using sanctity as cover; if genuine reading, it is a mountain (by its own lights) or tangled_rope (by analytical lights) with sincere coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the sanctity reading authentically instantiates the kernel or instrumentally deploys it.').

omega_variable(
    pressured_vulnerable_victim_status,
    'Are pressured-vulnerable populations genuinely victims of this constraint (denied autonomy they would exercise), or are they beneficiaries (protected from coercion they would face under autonomy frameworks)?',
    'Empirical study of disabled/elderly preferences in jurisdictions with and without assisted dying: do majorities of these populations support or oppose legalization? Longitudinal data on coercion deaths in permissive jurisdictions.',
    'If they are beneficiaries, the constraint''s victim set shrinks to terminal_patients_seeking_assisted_dying only, altering the extraction profile. If they are victims, the constraint extracts from the very population it claims to protect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pressured_vulnerable_victim_status, empirical, 'Direction of the pressured-vulnerable population''s structural relationship to the constraint.').

omega_variable(
    slippery_slope_empirical_status,
    'Does the slippery_slope_mechanism reading''s empirical claim (autonomy frameworks expand to incompetent/non-terminal) hold in jurisdictions with 20+ years of regulated assisted dying?',
    'Systematic review of jurisdictional data (Netherlands, Belgium, Oregon, Canada) on expansion of eligibility criteria, non-voluntary euthanasia rates, and regulatory capture.',
    'If the slope is empirically real, the sanctity reading''s coordination function is validated (genuine firewall). If not, the slope claim is a cover story for maintaining extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_empirical_status, empirical, 'Empirical status of the slippery slope claim that the sanctity reading invokes.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the constraint''s high suppression structural (criminal law, licensing) or internalized (moral internalization of sanctity doctrine by patients and physicians)?',
    'Post-legalization suppression trajectory: in jurisdictions that legalized assisted dying, does demand remain suppressed by internalized norms, or does it emerge rapidly? Compare physician willingness pre/post legalization.',
    'If internalized, effective suppression exceeds structural measure — the constraint persists in agent psychology after legal removal. If structural, legal change rapidly alters behavior.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/institutional constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1970, end_of_life_authority__sanctity_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(end__tr_t1985, end_of_life_authority__sanctity_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(end__tr_t1995, end_of_life_authority__sanctity_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(end__tr_t2005, end_of_life_authority__sanctity_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(end__tr_t2015, end_of_life_authority__sanctity_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(end__tr_t2025, end_of_life_authority__sanctity_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(end__be_t1970, end_of_life_authority__sanctity_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(end__be_t1985, end_of_life_authority__sanctity_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(end__be_t1995, end_of_life_authority__sanctity_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(end__be_t2005, end_of_life_authority__sanctity_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(end__be_t2015, end_of_life_authority__sanctity_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(end__be_t2025, end_of_life_authority__sanctity_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1970, end_of_life_authority__sanctity_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(end__su_t1985, end_of_life_authority__sanctity_reading, suppression_requirement, 1985, 0.78).
narrative_ontology:measurement(end__su_t1995, end_of_life_authority__sanctity_reading, suppression_requirement, 1995, 0.8).
narrative_ontology:measurement(end__su_t2005, end_of_life_authority__sanctity_reading, suppression_requirement, 2005, 0.81).
narrative_ontology:measurement(end__su_t2015, end_of_life_authority__sanctity_reading, suppression_requirement, 2015, 0.81).
narrative_ontology:measurement(end__su_t2025, end_of_life_authority__sanctity_reading, suppression_requirement, 2025, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__sanctity_reading, 0.08).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This constraint (sanctity_reading) and autonomy_reading are dual formulations of the end_of_life_authority kernel. The sanctity reading treats intrinsic value as the fixed kernel with categorical prohibition; the autonomy reading treats individual sovereignty as the fixed kernel with regulated permission. They foreclose each other within any single legal framework. The slippery_slope_mechanism is a downstream empirical claim that influences both readings' legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__sanctity_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
