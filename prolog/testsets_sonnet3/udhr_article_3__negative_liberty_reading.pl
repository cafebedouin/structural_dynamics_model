% ============================================================================
% CONSTRAINT STORY: udhr_article_3__negative_liberty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: udhr_article_3__negative_liberty_reading
 *   human_readable: UDHR Article 3 — Negative Liberty Reading (Freedom From State Violence)
 *   domain: constitutional_law/human_rights/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the negative-liberty reading of UDHR Article 3,
 *   under which the right to life and security of person is read as a shield
 *   against state action: the state may deprive a person of life or liberty
 *   only through narrow, tightly bounded procedural justice, and this reading
 *   treats capital punishment as per se incompatible with the guarantee and
 *   reads self-defense/use-of-force doctrine restrictively. The reading has
 *   grown more extractive over time (ε rising from ~0.35 in 1948 to ~0.68 in
 *   2024) as international jurisprudence — especially in regional human
 *   rights courts and UN treaty-body commentary — has hardened the
 *   abolitionist and due-process-maximalist implications of the text well
 *   beyond what the 1948 drafters explicitly settled. This is NOT a story
 *   about positive welfare entitlements (that is a sibling reading,
 *   positive_entitlement_reading) nor about the due-process floor without a
 *   substantive liberty/welfare resolution (procedural_hybrid_reading) — this
 *   story's ε, beneficiaries, and victims describe only the negative-liberty
 *   claim, holding it fixed as one specific, structurally distinct constraint
 *   per the ε-invariance principle.
 *
 * KEY AGENTS:
 *   - criminal_defendants: Primary beneficiary (powerless/trapped) — receives due-process shield
 *   - death_row_inmates: Primary beneficiary (powerless/trapped) — sentence voided under abolitionist extension
 *   - civil_liberties_advocates: Agenda-setter (organized/mobile) — advances the reading through litigation and advocacy
 *   - crime_victims_seeking_deterrence: Primary payer (powerless/trapped) — loses claim to harsh state sanction
 *   - collective_security_apparatus: Institutional payer (institutional/constrained) — operational discretion narrowed
 *   - retentionist_states: Excluded (institutional/constrained) — treated as per se violators, not a legitimate interpretive party
 *   - international_human_rights_bodies: Analytical observer (institutional/analytical) — adjudicates and channels doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_article_3__negative_liberty_reading, 0.68).
domain_priors:suppression_score(udhr_article_3__negative_liberty_reading, 0.52).
domain_priors:theater_ratio(udhr_article_3__negative_liberty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(udhr_article_3__negative_liberty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_article_3__negative_liberty_reading, tangled_rope).
narrative_ontology:human_readable(udhr_article_3__negative_liberty_reading, "UDHR Article 3 — Negative Liberty Reading (Freedom From State Violence)").
narrative_ontology:topic_domain(udhr_article_3__negative_liberty_reading, "constitutional_law/human_rights/political_philosophy").

domain_priors:requires_active_enforcement(udhr_article_3__negative_liberty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_article_3__negative_liberty_reading, 'c29fd888-26bd-4924-be5a-bd27979d792e').
narrative_ontology:cs_kernel_codification('c29fd888-26bd-4924-be5a-bd27979d792e', fixed_text).
narrative_ontology:cs_authority_grounding('c29fd888-26bd-4924-be5a-bd27979d792e', practice).
narrative_ontology:cs_interpretation_layer_present('c29fd888-26bd-4924-be5a-bd27979d792e').
narrative_ontology:cs_reading_relation('c29fd888-26bd-4924-be5a-bd27979d792e', udhr_article_3__positive_entitlement_reading, coexists_with).
narrative_ontology:cs_reading_relation('c29fd888-26bd-4924-be5a-bd27979d792e', udhr_article_3__procedural_hybrid_reading, influences).
narrative_ontology:cs_axiom('c29fd888-26bd-4924-be5a-bd27979d792e', foundational, state_lethal_power_is_the_primary_threat_to_security).
narrative_ontology:cs_axiom_status(state_lethal_power_is_the_primary_threat_to_security, holdable).
narrative_ontology:cs_axiom_grounding('c29fd888-26bd-4924-be5a-bd27979d792e', state_lethal_power_is_the_primary_threat_to_security, deontological).
narrative_ontology:cs_axiom('c29fd888-26bd-4924-be5a-bd27979d792e', foundational, capital_punishment_is_categorically_incompatible_with_article_3).
narrative_ontology:cs_axiom_status(capital_punishment_is_categorically_incompatible_with_article_3, holdable).
narrative_ontology:cs_axiom_grounding('c29fd888-26bd-4924-be5a-bd27979d792e', capital_punishment_is_categorically_incompatible_with_article_3, deontological).
narrative_ontology:cs_reference_frame('c29fd888-26bd-4924-be5a-bd27979d792e', post_atrocity_state_restraint_settlement).
narrative_ontology:cs_drift_state('c29fd888-26bd-4924-be5a-bd27979d792e', post_cold_war_jurisprudential_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c29fd888-26bd-4924-be5a-bd27979d792e', '').
narrative_ontology:cs_kernel_id(udhr_article_3__negative_liberty_reading, udhr_article_3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, criminal_defendants).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, death_row_inmates).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, civil_liberties_advocates).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, individuals_facing_state_action).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, crime_victims_seeking_deterrence).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, collective_security_apparatus).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, communities_facing_organized_violence).
narrative_ontology:constraint_victim(udhr_article_3__negative_liberty_reading, law_enforcement_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_article_3__negative_liberty_reading, abolitionist_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face state prosecution and potential deprivation of life or liberty. Under this reading, they receive expansive due process protections, abolition of capital punishment, and narrow procedural gates the state must clear before acting against them. They have no exit from the proceeding itself but the reading structurally shifts the burden onto the state.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, criminal_defendants, beneficiary,
    powerless, biographical, trapped, national).

% Under a capital sentence in jurisdictions that retain the death penalty. This reading treats their execution as a per se violation of Article 3 regardless of the procedural safeguards observed, converting Article 3 into an abolitionist mandate that would void their sentences outright.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, death_row_inmates, beneficiary,
    powerless, biographical, trapped, national).

% Litigate, lobby, and author doctrine advancing the negative-liberty reading. They set the interpretive agenda by filing test cases, drafting model statutes, and pressing international bodies to read Article 3 as an anti-state-violence guarantee. They can shift jurisdictions and forums; the constraint's persistence depends on their continued advocacy.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, civil_liberties_advocates, agenda_setter,
    organized, generational, mobile, global).

% Have suffered violent crime and seek the deterrent and incapacitative effect of severe sanctions, including capital punishment where it exists. Under the negative-liberty reading, their claim to state protection through harsh sanction is subordinated to the defendant's Article 3 shield; they bear the cost of a system that structurally favors restraint on state power over their demand for retribution or deterrence.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, crime_victims_seeking_deterrence, payer,
    powerless, biographical, trapped, national).

% The state's police, military, and security services whose authority to use lethal force, detain preventively, or act on emergency footing is narrowed by this reading's restrictive self-defense and due-process doctrines. They must redesign use-of-force protocols and detention procedures around narrow procedural gates, at operational and political cost.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, collective_security_apparatus, payer,
    institutional, generational, constrained, national).

% Live under threat from gangs, insurgencies, or cartels in contexts where state capacity to respond with lethal force is constrained by narrow procedural justice requirements. They argue the negative-liberty reading, by disabling emergency and preventive measures, leaves them exposed to non-state violence the state could otherwise suppress.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, communities_facing_organized_violence, payer,
    powerless, biographical, trapped, regional).

% Must operate under use-of-force doctrines and detention limits shaped by this reading's expansive due-process demands. They administer the procedures that implement Article 3 constraints day to day, giving them some agenda-setting latitude in practice, but bear the operational cost and political exposure when constrained tactics are blamed for security failures.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, law_enforcement_agencies, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_article_3__negative_liberty_reading, law_enforcement_agencies, agenda_setter).

% States that have already abolished capital punishment and adopted restrictive self-defense doctrine benefit from this reading's international vindication of their domestic settlement, using it to pressure retentionist states diplomatically and in multilateral fora.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, abolitionist_states, beneficiary,
    institutional, civilizational, arbitrage, global).

% States retaining capital punishment or broader self-defense/security doctrines are treated by this reading as per se in violation of Article 3's core guarantee. Their own security rationale — democratic mandate, victim advocacy, deterrence evidence — is not admitted as a legitimate counter-consideration within the reading's framework; they are cast as the violation rather than a party to the interpretive contest.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, retentionist_states, excluded,
    institutional, generational, constrained, national).

% Treaty bodies, special rapporteurs, and regional human rights courts adjudicate and comment on state compliance with Article 3. They take submissions from advocates, retentionist states, and civil society, and their jurisprudence increasingly channels toward the negative-liberty reading in some regional systems while other systems resist it.
narrative_ontology:constraint_stakeholder(udhr_article_3__negative_liberty_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_article_3__negative_liberty_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_article_3__negative_liberty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared limit on state lethal and coercive power across jurisdictions: no state may take life or liberty except through narrowly bounded procedure, giving individuals a stable, portable baseline of protection against arbitrary state violence regardless of which state they are under.
% TRANSFER_FUNCTION: Moves discretion away from states (police, military, courts, executive clemency and sentencing power) and toward individuals subject to state action; correspondingly moves the cost of restraint — foregone deterrence, foregone capital sanction, foregone preventive detention — onto crime victims, communities under organized-violence threat, and the security apparatus itself.
% ABSENT_VOICES: Crime victims' associations and communities living under gang or insurgent violence are rarely direct parties to the international jurisprudence that develops this reading; their preference for stronger state response is filtered through prosecutors and legislatures rather than voiced directly in the human-rights forums that generate the doctrine. Retentionist-state publics who support capital punishment via democratic process are treated as presumptively wrong rather than as a competing legitimate voice.
% DISAPPEARANCE_RATIONALE: If the negative-liberty reading disappeared overnight, retentionist states would face far less international and doctrinal pressure to abolish capital punishment, self-defense and preventive-detention doctrines would expand toward the state, and death-row inmates in retentionist jurisdictions would lose their strongest available international legal argument. Abolitionist advocacy networks would lose a central doctrinal anchor; security agencies would regain the discretion the reading currently narrows.
% FOUNDING_PROBLEM: Post-WWII drafters sought to prevent recurrence of state-perpetrated mass killing, extrajudicial execution, and arbitrary detention — the immediate memory was totalitarian states using 'security' and 'order' as pretexts for eliminating populations without any check on state lethal power.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the UDHR drafting record and international law scholars outside the abolitionist advocacy movement (e.g., accounts of the 1948 Third Committee debates) corroborate that unchecked state violence against civilian populations was the drafters' central concern. However, those same historical accounts show the drafters left open whether capital punishment via due process and legitimate self-defense doctrine were meant to be foreclosed — the abolitionist extension is a later interpretive development, not something the founding-era corroboration record unambiguously establishes.
narrative_ontology:disappearance_verdict(udhr_article_3__negative_liberty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_article_3__negative_liberty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_article_3__negative_liberty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_article_3__negative_liberty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_article_3__negative_liberty_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.68) is authored high because the negative-liberty reading's expansive due-process and abolitionist implications extract real discretion and real policy outcomes (executions barred, preventive detention narrowed, use-of-force doctrine tightened) from states and from crime victims/communities who would prefer stronger security measures — this is a genuine transfer, not merely rhetorical. Suppression (0.52) is moderate: the reading is enforced through litigation, treaty-body pressure, and diplomatic consequence rather than direct coercive apparatus, but retentionist states face real reputational and legal cost for departing from it. Theater ratio (0.28) is modest — the doctrinal machinery (courts, rapporteurs, treaty bodies) performs real adjudicative work, though a rising share of activity is advocacy-driven doctrinal elaboration rather than dispute resolution. Accessibility collapse (0.45) is mid-range: retentionist states retain a real alternative (continued capital punishment, broader self-defense doctrine) that has not collapsed globally, unlike a Mountain-type constraint. Resistance (0.71) is high: retentionist states, victims' rights movements, and law-and-order constituencies actively contest this reading in nearly every jurisdiction where it is pressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Criminal defendants, death-row inmates, and civil liberties advocates sit near the beneficiary end: the reading transfers discretion and protection toward them and away from the state. Crime victims, communities facing organized violence, the security apparatus, and law enforcement sit near the target end: they bear the cost of narrowed state capacity to respond to threats with lethal or preventive force. Retentionist states are treated structurally as violators rather than as parties with a competing legitimate security interest — this asymmetry is itself part of what the reading does, and is why they are marked 'excluded' rather than 'payer': their security rationale is not admitted into the interpretive framework at all, which is a stronger exclusion than merely bearing a cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state mass-killing and extrajudicial execution) remains partially live — state violence against civilians has not disappeared — but the negative-liberty reading's specific mechanisms (capital-punishment abolition doctrine, restrictive self-defense) extend well past that founding concern into contested policy terrain (deterrence efficacy, victims' rights, democratic sentencing authority) where the founding-era consensus does not clearly reach. Classifying this as tangled_rope rather than snare or rope captures that: there IS a genuine coordination function (a shared floor against arbitrary state violence that individuals in every jurisdiction can invoke) bundled with real asymmetric extraction (the doctrine's abolitionist and restrictive-self-defense extensions transfer real cost onto crime victims, threatened communities, and security agencies) sustained by active enforcement (treaty-body pressure, regional court jurisprudence, diplomatic consequence). Neither 'pure coordination' (rope) nor 'pure extraction with victims and no coordination function' (snare) captures the full structure honestly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_negative_liberty,
    'Is the negative-liberty reading of Article 3 the drafters'' intended meaning, a legitimate later doctrinal extension consistent with the founding purpose, or an interpretive overreach that the procedural_hybrid_reading and positive_entitlement_reading siblings more accurately capture?',
    'Comparative analysis of Third Committee drafting records, subsequent state practice (opinio juris) across abolitionist and retentionist states, and whether regional human rights court jurisprudence has converged or diverged on the abolitionist/restrictive-self-defense extension over the 1948-2024 interval.',
    'If the negative-liberty reading is found to be a legitimate extension consistent with founding purpose, its high ε reflects genuine and defensible protection strengthening over time. If found to be overreach beyond drafter intent and beyond durable international consensus, the same high ε instead reflects doctrinal capture by advocacy networks (civil_liberties_advocates as agenda_setter) imposing a contested policy preference under color of settled human-rights law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_negative_liberty, conceptual, 'Whether the negative-liberty reading is faithful extension or interpretive overreach relative to the kernel''s founding purpose.').

omega_variable(
    security_measure_tradeoff_empirics,
    'Does restrictive self-defense doctrine and abolition of capital punishment measurably increase harm to communities facing organized violence and to crime victims seeking deterrence, or does the empirical deterrence/incapacitation literature not support the security apparatus''s claimed cost?',
    'Cross-jurisdictional empirical comparison of violent crime rates, extrajudicial killing rates, and community security outcomes between jurisdictions that have and have not adopted the negative-liberty reading''s restrictive doctrines, controlling for confounds.',
    'If the empirical harm to victims/communities is negligible, the ''victim'' framing for collective_security_apparatus and crime_victims_seeking_deterrence is overstated and the reading''s ε should be read as lower-extraction coordination; if the harm is substantial and well-evidenced, it corroborates the tangled_rope classification''s asymmetric-extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_measure_tradeoff_empirics, empirical, 'Whether the reading''s restriction of state security capacity produces measurable harm to the parties treated as victims.').

omega_variable(
    retentionist_state_democratic_legitimacy,
    'When a retentionist state''s capital punishment or broad self-defense doctrine reflects a sustained democratic mandate, does the negative-liberty reading''s treatment of that state as a per se violator override a legitimate competing source of political legitimacy, or is human-rights law properly understood as a constraint that democratic majorities cannot vote around?',
    'This is a values question not resolvable by data alone — it depends on whether one holds that certain rights are properly counter-majoritarian (unresolvable by democratic process) or that human-rights instruments should defer to sustained, procedurally legitimate domestic democratic outcomes.',
    'Determines whether excluding retentionist_states'' security rationale from the interpretive framework (as this reading does) is itself a legitimate feature of human-rights constitutionalism or an extractive move that silences a legitimate competing voice.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retentionist_state_democratic_legitimacy, preference, 'Whether counter-majoritarian human-rights constraint on retentionist democratic mandates is legitimate or extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_article_3__negative_liberty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_article_3__negative_liberty_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(udhr_tr_t1970, udhr_article_3__negative_liberty_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(udhr_tr_t1990, udhr_article_3__negative_liberty_reading, theater_ratio, 1990, 0.19).
narrative_ontology:measurement(udhr_tr_t2005, udhr_article_3__negative_liberty_reading, theater_ratio, 2005, 0.23).
narrative_ontology:measurement(udhr_tr_t2015, udhr_article_3__negative_liberty_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement(udhr_tr_t2024, udhr_article_3__negative_liberty_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_article_3__negative_liberty_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement(udhr_be_t1970, udhr_article_3__negative_liberty_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement(udhr_be_t1990, udhr_article_3__negative_liberty_reading, base_extractiveness, 1990, 0.53).
narrative_ontology:measurement(udhr_be_t2005, udhr_article_3__negative_liberty_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement(udhr_be_t2015, udhr_article_3__negative_liberty_reading, base_extractiveness, 2015, 0.66).
narrative_ontology:measurement(udhr_be_t2024, udhr_article_3__negative_liberty_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_article_3__negative_liberty_reading, suppression_requirement, 1948, 0.25).
narrative_ontology:measurement(udhr_su_t1970, udhr_article_3__negative_liberty_reading, suppression_requirement, 1970, 0.31).
narrative_ontology:measurement(udhr_su_t1990, udhr_article_3__negative_liberty_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(udhr_su_t2005, udhr_article_3__negative_liberty_reading, suppression_requirement, 2005, 0.44).
narrative_ontology:measurement(udhr_su_t2015, udhr_article_3__negative_liberty_reading, suppression_requirement, 2015, 0.49).
narrative_ontology:measurement(udhr_su_t2024, udhr_article_3__negative_liberty_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_article_3__negative_liberty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_article_3__negative_liberty_reading, 0.12).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, positive_entitlement_reading).
narrative_ontology:affects_constraint(udhr_article_3__negative_liberty_reading, procedural_hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the natural-language concept 'UDHR Article 3.' negative_liberty_reading (this story) authors high ε via capital-punishment abolition and restrictive self-defense doctrine, with individuals as beneficiaries and collective security measures as victims. positive_entitlement_reading authors a structurally distinct ε for the claim that Article 3 obligates material provision (welfare/healthcare/housing). procedural_hybrid_reading authors a lower, narrower ε for the due-process floor (habeas corpus, torture prohibition) that does not resolve the negative/positive liberty contest. Per the ε-invariance principle, these are three separate constraints sharing a kernel, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
