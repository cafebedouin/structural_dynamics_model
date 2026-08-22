% ============================================================================
% CONSTRAINT STORY: article_17_complementarity__international_oversight_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_17_complementarity__international_oversight_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: article_17_complementarity__international_oversight_reading
 *   human_readable: Article 17 Complementarity: International Oversight Reading
 *   domain: international_law/criminal_justice/state_sovereignty
 *
 * SUMMARY:
 *   Article 17 of the Rome Statute establishes complementarity: the ICC may
 *   only exercise jurisdiction when national courts are 'unwilling or unable'
 *   to investigate and prosecute crimes within its mandate. This constraint
 *   is the broad international-oversight reading: 'unwilling or unable' is
 *   interpreted expansively to include any compromise of independence,
 *   genuine intent, or impartiality—not just factual inability or
 *   transparently farcical proceedings. Under this reading, the ICC operates
 *   as a guardian against impunity when states fail, setting the evidentiary
 *   and procedural standard for what counts as adequate domestic
 *   accountability. The sibling national-primacy reading views
 *   complementarity as a sovereignty-protection mechanism: states retain a
 *   strong presumption of adequacy, and the ICC bears a heavy burden to prove
 *   domestic courts are sham or factually incapable. This story instantiates
 *   only the international-oversight reading: a tangled rope of coordination
 *   (victims need accountability) and extraction (states' judicial autonomy
 *   is constrained by external oversight).
 *
 * KEY AGENTS:
 *   - ICC Prosecutor Office — sets admissibility threshold via interpretation of 'unwilling or unable'; controls timing and scope of intervention
 *   - Victims in complicit/failed states — primary beneficiaries; access accountability forum only through ICC intervention
 *   - Sovereign states under scrutiny — payers; judicial autonomy constrained by threat of ICC override
 *   - National prosecution systems — payers; institutional independence undermined by admissibility scrutiny
 *   - International justice community — beneficiary; norm vindicates international criminal law expansion
 *   - Powerful non-ratifying states — excluded; protected by non-cooperation and non-ratification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, 0.68).
domain_priors:suppression_score(article_17_complementarity__international_oversight_reading, 0.71).
domain_priors:theater_ratio(article_17_complementarity__international_oversight_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_17_complementarity__international_oversight_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_17_complementarity__international_oversight_reading, tangled_rope).
narrative_ontology:human_readable(article_17_complementarity__international_oversight_reading, "Article 17 Complementarity: International Oversight Reading").
narrative_ontology:topic_domain(article_17_complementarity__international_oversight_reading, "international_law/criminal_justice/state_sovereignty").

domain_priors:requires_active_enforcement(article_17_complementarity__international_oversight_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_17_complementarity__international_oversight_reading, '428c809b-af42-4af1-9b6d-bfddb202dec3').
narrative_ontology:cs_kernel_codification('428c809b-af42-4af1-9b6d-bfddb202dec3', fixed_text).
narrative_ontology:cs_authority_grounding('428c809b-af42-4af1-9b6d-bfddb202dec3', lineage).
narrative_ontology:cs_interpretation_layer_present('428c809b-af42-4af1-9b6d-bfddb202dec3').
narrative_ontology:cs_reading_relation('428c809b-af42-4af1-9b6d-bfddb202dec3', article_17_complementarity__national_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('428c809b-af42-4af1-9b6d-bfddb202dec3', foundational, unwilling_or_unable_interpreted_expansively).
narrative_ontology:cs_axiom_status(unwilling_or_unable_interpreted_expansively, holdable).
narrative_ontology:cs_axiom_grounding('428c809b-af42-4af1-9b6d-bfddb202dec3', unwilling_or_unable_interpreted_expansively, deontological).
narrative_ontology:cs_axiom('428c809b-af42-4af1-9b6d-bfddb202dec3', foundational, icc_as_accountability_guardian_of_last_resort).
narrative_ontology:cs_axiom_status(icc_as_accountability_guardian_of_last_resort, holdable).
narrative_ontology:cs_axiom_grounding('428c809b-af42-4af1-9b6d-bfddb202dec3', icc_as_accountability_guardian_of_last_resort, deontological).
narrative_ontology:cs_reference_frame('428c809b-af42-4af1-9b6d-bfddb202dec3', complementarity_as_accountability_safety_valve).
narrative_ontology:cs_drift_state('428c809b-af42-4af1-9b6d-bfddb202dec3', contemporary_prosecutor_expansionism, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('428c809b-af42-4af1-9b6d-bfddb202dec3', '').
narrative_ontology:cs_kernel_id(article_17_complementarity__international_oversight_reading, article_17_complementarity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_in_complicit_states).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, victims_of_elite_capture).
narrative_ontology:constraint_beneficiary(article_17_complementarity__international_oversight_reading, international_justice_community).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, sovereign_states_under_scrutiny).
narrative_ontology:constraint_victim(article_17_complementarity__international_oversight_reading, national_prosecution_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets 'unwilling or unable' broadly to justify intervention when domestic prosecution systems show signs of elite capture, political motivation, or sham procedure. Controls the evidentiary standard and timing of admissibility determinations. Pursues parallel investigations in complicit states while pressing states for cooperation. Sets the threshold for what counts as genuine domestic accountability.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, icc_prosecutor_office, agenda_setter,
    institutional, generational, analytical, universal).

% Citizens of states where the governing elite committed crimes (genocide, crimes against humanity, war crimes) and no domestic court will prosecute. The ICC intervention opens a forum they would otherwise lack. Their access to accountability depends entirely on the ICC's willingness to intervene and the state's capacity to arrest and extradite suspects. They are identity-locked as victims of the state whose perpetrators control the justice system.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_in_complicit_states, beneficiary,
    powerless, biographical, identity_locked, global).

% Harmed by crimes committed by the same elites who control the national justice system. A purely national court would be captured—judges appointed by the perpetrators, prosecutors pressured to drop cases, witnesses intimidated by state power. ICC intervention is their only realistic path to prosecution. They are identity-locked by their status as victims within a captured state apparatus.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victims_of_elite_capture, beneficiary,
    powerless, biographical, identity_locked, global).

% Interprets the broad complementarity threshold as vindicating the principle that atrocities should not go unpunished simply because a perpetrator-state controls the domestic system. Expands the normative consensus that international criminal law fills the protection gap where national courts fail. Benefits from expanded jurisdiction and institutional relevance.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, international_justice_community, beneficiary,
    institutional, generational, analytical, universal).

% Subject to ICC jurisdiction and admissibility scrutiny. The broad 'unwilling or unable' standard means the ICC can declare a national prosecution inadequate even when the state in good faith pursues cases. Sovereignty is constrained: they cannot insulate their security forces and political leaders from international oversight through domestic legal procedures alone. Exit (treaty withdrawal) is possible but costly.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, sovereign_states_under_scrutiny, payer,
    institutional, generational, constrained, national).

% Courts and prosecutors face ICC oversight of their independence, impartiality, and genuine intent. A low admissibility threshold means even good-faith prosecutions can be second-guessed by ICC judges who have access to different evidence or hold different views of what constitutes genuine intent. Institutional autonomy is undermined by the prospect of ICC intervention. They cannot exit without state-level withdrawal.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, national_prosecution_systems, payer,
    organized, biographical, constrained, national).

% The broad complementarity reading creates vulnerability to selective ICC prosecution if rival powers dominate the prosecutor's office or UN Security Council referrals. Major powers maintain non-ratification or threat of withdrawal to preserve freedom of action. Their elite and security forces are structurally protected from ICC jurisdiction by state-level refusal to cooperate. They would object if included in the broad oversight reading but are not in the conversation.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, powerful_states_with_geopolitical_interests, excluded,
    powerful, generational, arbitrage, global).

% May face ICC investigation and prosecution of adversaries they defeated, while their own crimes are overlooked or prosecuted unevenly. The broad reading leaves space for political motivation—the ICC prosecutor can be pressured to pursue cases against defeated powers while ignoring winners. Their exclusion is not from the mechanism itself but from equal accountability under it. They would argue for the national-primacy reading but have little formal voice in admissibility determinations.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, victor_states_in_recent_conflicts, excluded,
    powerful, biographical, mobile, global).

% Academic and NGO analysts track whether the broad complementarity standard serves accountability or becomes a tool for victor's justice and geopolitical prosecution. They document which states face scrutiny and which remain untouched, seeking to measure whether the constraint functions as intended or drifts into selective enforcement.
narrative_ontology:constraint_stakeholder(article_17_complementarity__international_oversight_reading, complementarity_observers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_17_complementarity__international_oversight_reading, icc_prosecutor_office).
narrative_ontology:fixing_cost_class(article_17_complementarity__international_oversight_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a safety valve for accountability when states fail: coordinates international justice system to step in when domestic courts cannot or will not prosecute crimes against humanity, genocide, and war crimes. Solves the coordination problem of who has standing and authority to prosecute when the perpetrator controls the state.
% TRANSFER_FUNCTION: Transfers judicial authority and prosecutorial discretion from national courts to an international forum. Moves the power to determine adequacy of domestic prosecution from the accused state (which has every incentive to claim its system is impartial) to an external arbiter. Transfers the burden of proof downward: states must now affirmatively demonstrate independence and genuine intent rather than claiming it by default.
% ABSENT_VOICES: Perpetrators in powerful states that have not ratified the ICC statute or maintain credible non-cooperation threats; national judges in weak states pressured to accept ICC scrutiny; rival geopolitical powers whose crimes are overlooked while others are prosecuted; scholars and states that argue the broad reading empowers selective prosecution by dominant powers and violates the principle that complementarity was designed to protect state sovereignty.
% DISAPPEARANCE_RATIONALE: If the broad complementarity reading were abandoned (e.g., by a reversion to the 'national primacy' reading where states get a strong presumption of adequacy), perpetrators in complicit or failed states would return to practical impunity. Victims would lose their only accountability forum. States with elite capture of their justice systems would face no external pressure. The international justice architecture would shrink to cases prosecuted only when the accused state permits it. The normative consensus that atrocities must not go unpunished would weaken.
% FOUNDING_PROBLEM: Genocides, crimes against humanity, and systematic war crimes are committed by state elites who control the domestic courts meant to try them. A purely national approach leaves victims without recourse when the perpetrators are also the judges. Historical precedent: Rwanda (tribunal created because domestic courts could not prosecute), Yugoslavia, Cambodia, Sierra Leone.
% FOUNDING_PROBLEM_CORROBORATION: Documented by human rights organizations (Amnesty International, Human Rights Watch, International Crisis Group) and international legal scholars studying post-conflict societies (Uganda, Democratic Republic of Congo, Libya, Syria) where state officials and security forces commit atrocities while the state judiciary is structurally incapable of impartial prosecution. The ICTR, ICTY, and Special Courts for Sierra Leone and Cambodia all emerged because national courts could not or would not act. These precedents corroborate the founding problem from parties outside the ICC beneficiary set (the courts themselves and external observers, not the prosecutor who has institutional interest in maintaining broad jurisdiction).
narrative_ontology:disappearance_verdict(article_17_complementarity__international_oversight_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_17_complementarity__international_oversight_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_17_complementarity__international_oversight_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_17_complementarity__international_oversight_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_17_complementarity__international_oversight_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_17_complementarity__international_oversight_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_17_complementarity__international_oversight_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_17_complementarity__international_oversight_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.48 → 0.68 over the interval) because the broad interpretation expands what the ICC can declare inadequate, giving it wide leverage over state behavior. Suppression is also high (0.71) because states cannot easily exit: withdrawal from the treaty carries diplomatic costs, non-cooperation invites targeted sanctions, and the threat of ICC jurisdiction constrains elite behavior regardless of actual prosecution likelihood. Theater ratio is moderate-to-high (0.48) because a growing share of ICC activity consists of admissibility determinations and state-cooperation demands rather than actual convictions. The accessibility_collapse score (0.62) reflects that once a state understands the broad standard, alternative paths to impunity (purely domestic handling of crimes) appear closed; but resistance remains significant (0.58) because powerful states refuse ratification and refuse cooperation. The measurement series shows extractiveness and suppression both accumulating over the 24-year interval, consistent with the prosecutor's office interpreting 'unwilling or unable' more expansively and states facing increasingly intensive cooperation demands.
 *
 * PERSPECTIVAL GAP:
 *   The international-oversight reading frames complementarity as enabling accountability; the national-primacy reading frames it as threatening sovereignty. From the oversight perspective, states' judicial autonomy must yield when elites capture courts. From the primacy perspective, the ICC's expansive threshold violates the presumption that states can manage their own justice systems. These are not reconcilable disagreements about facts but disagreements about how to balance accountability against sovereignty—what axiom should ground the kernel's interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality overrides are needed; the structural derivation (beneficiary low-d for the ICC and international justice community; victim high-d for states and national courts; trapped beneficiary balanced d for victims in complicit states who benefit from the mechanism despite bearing the identity-lock cost) follows directly from the beneficiary/victim declarations and exit-options data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (elite-captured courts leaving atrocities unpunished) is live, but there is substantial debate about whether the broad complementarity reading is the right tool. The founding-problem-status x disappearance-verdict mismatch test: founding_problem_status=live, disappearance_verdict=world_rearranges. If the constraint disappeared (reverting to the national-primacy reading), victims in complicit states would lose their accountability forum and perpetrators would return to practical impunity. This mismatch does not by itself indicate mandatrophy—the founding problem still exists and the constraint still addresses it. However, the rising theater_ratio (0.32 → 0.48) and the expansion of suppression_requirement suggest the constraint may be drifting from accountability mechanism toward sovereignty extraction: an increasing share of ICC activity is admissibility determinations and cooperation demands rather than actual convictions of perpetrators. If the trend continues and the actual prosecution rate stalls while the cooperation demands intensify, the constraint could accumulate toward piton status (performed accountability covering elite immunity by other means). The measurement data does not yet show mandatrophy, but the trajectory warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victor_justice_vs_genuine_accountability,
    'Does the broad complementarity reading function as a genuine accountability mechanism, or does it create an opening for victor''s justice—where defeated powers and rivals face prosecution while allied or victorious states'' crimes are overlooked?',
    'Comparative analysis of which state actors face ICC scrutiny relative to the severity and scope of their alleged crimes, cross-referenced against geopolitical alignment and power asymmetries. Longitudinal tracking of case selection over decades to reveal patterns of selective prosecution.',
    'If victor''s justice predominates, the constraint is a snare—extraction disguised as accountability. If genuine accountability is achieved despite power asymmetries, the tangled_rope framing (coordination + asymmetric extraction) holds. If the answer oscillates by case, the reading itself is unstable and may require decomposition into separate constraints for different prosecution scenarios.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victor_justice_vs_genuine_accountability, empirical, 'Whether broad complementarity enables genuine accountability or geopolitical prosecution.').

omega_variable(
    broad_vs_narrow_unwilling_or_unable_interpretation,
    'Does ''unwilling or unable'' mean any deviation from the ICC''s own standards for independence and impartiality (broad reading, this constraint), or does it mean only factual inability to prosecute or transparently non-independent proceedings (narrow reading)?',
    'Textual analysis of Rome Statute Article 17; historical intent from preparatory documents; comparison of how different prosecutor administrations have applied the standard; observance of whether states with genuine prosecutorial independence but different legal philosophies or definitions of impartiality are nonetheless subject to ICC override.',
    'The broad reading supports this constraint''s high extractiveness (states must meet ICC standards, not merely domestic ones). The narrow reading would lower extractiveness and shift the type toward rope. This is a fundamental interpretive disagreement between this reading and the national_primacy_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(broad_vs_narrow_unwilling_or_unable_interpretation, conceptual, 'The definitional boundary between broad and narrow complementarity interpretations.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of alternative accountability mechanisms (national courts retaining presumptive primacy) structural (enforcement machinery of ICC override) or partly internalized (states internalize the standard as legitimate and voluntarily defer)?',
    'Post-ICC-refusal behavior: states that withdraw from the treaty or resist cooperation; examination of whether national courts cite ICC standards as persuasive (internalization) or resist them as external imposition (structural suppression only). Tracking of non-cooperating states'' behavior when the ICC lacks enforcement capacity.',
    'If internalized, suppression is lower than measured and the constraint is more durable. If structural alone, it depends on continued ICC enforcement capacity; degradation of enforcement would reduce suppression sharply. High internalization would point to genuine norm acceptance; low internalization would indicate coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of national primacy is internalized or structural.').

omega_variable(
    kernel_reading_contest_structure,
    'This constraint instantiates the broad ''international oversight'' reading of Article 17 complementarity. The sibling ''national primacy'' reading holds that complementarity protects state sovereignty and presumes national courts adequate unless proven sham. Which reading reflects the true legal architecture of the Rome Statute—or do both coexist as live interpretive options held by different institutional actors?',
    'Close reading of Rome Statute text; examination of preparatory documents; analysis of how the ICC Pre-Trial Chamber has applied Article 17 admissibility determinations across cases; tracking of whether the prosecutor and judges consistently favor one reading or oscillate between them; observation of state practice (resistance/acceptance patterns across different state cohorts).',
    'If the international oversight reading is the settled law, the national primacy reading is foreclosed or severely constrained. If both remain live, they coexist and the kernel is under-specified, creating ongoing interpretive contestation. If the readings influence each other (e.g., the broad reading''s aggressive application drives states to tighten domestic procedures in reaction), they influence rather than foreclose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'The kernel contest between international oversight and national primacy readings of complementarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_17_complementarity__international_oversight_reading, 2002, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t2002, article_17_complementarity__international_oversight_reading, theater_ratio, 2002, 0.32).
narrative_ontology:measurement_basis(arti_tr_t2002, observed).
narrative_ontology:measurement(arti_tr_t2008, article_17_complementarity__international_oversight_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement_basis(arti_tr_t2008, observed).
narrative_ontology:measurement(arti_tr_t2014, article_17_complementarity__international_oversight_reading, theater_ratio, 2014, 0.43).
narrative_ontology:measurement_basis(arti_tr_t2014, observed).
narrative_ontology:measurement(arti_tr_t2020, article_17_complementarity__international_oversight_reading, theater_ratio, 2020, 0.46).
narrative_ontology:measurement_basis(arti_tr_t2020, observed).
narrative_ontology:measurement(arti_tr_t2026, article_17_complementarity__international_oversight_reading, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(arti_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(arti_be_t2002, article_17_complementarity__international_oversight_reading, base_extractiveness, 2002, 0.48).
narrative_ontology:measurement_basis(arti_be_t2002, observed).
narrative_ontology:measurement(arti_be_t2008, article_17_complementarity__international_oversight_reading, base_extractiveness, 2008, 0.54).
narrative_ontology:measurement_basis(arti_be_t2008, observed).
narrative_ontology:measurement(arti_be_t2014, article_17_complementarity__international_oversight_reading, base_extractiveness, 2014, 0.61).
narrative_ontology:measurement_basis(arti_be_t2014, observed).
narrative_ontology:measurement(arti_be_t2020, article_17_complementarity__international_oversight_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement_basis(arti_be_t2020, observed).
narrative_ontology:measurement(arti_be_t2026, article_17_complementarity__international_oversight_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(arti_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t2002, article_17_complementarity__international_oversight_reading, suppression_requirement, 2002, 0.55).
narrative_ontology:measurement_basis(arti_su_t2002, observed).
narrative_ontology:measurement(arti_su_t2008, article_17_complementarity__international_oversight_reading, suppression_requirement, 2008, 0.62).
narrative_ontology:measurement_basis(arti_su_t2008, observed).
narrative_ontology:measurement(arti_su_t2014, article_17_complementarity__international_oversight_reading, suppression_requirement, 2014, 0.67).
narrative_ontology:measurement_basis(arti_su_t2014, observed).
narrative_ontology:measurement(arti_su_t2020, article_17_complementarity__international_oversight_reading, suppression_requirement, 2020, 0.69).
narrative_ontology:measurement_basis(arti_su_t2020, observed).
narrative_ontology:measurement(arti_su_t2026, article_17_complementarity__international_oversight_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(arti_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_17_complementarity__international_oversight_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_17_complementarity__international_oversight_reading, 0.12).
narrative_ontology:affects_constraint(article_17_complementarity__international_oversight_reading, article_17_complementarity__national_primacy_reading).

% DUAL FORMULATION NOTE:
% Article 17 complementarity decomposes into two structurally distinct constraints corresponding to the broad (international-oversight, this story) and narrow (national-primacy, sibling story) interpretations of 'unwilling or unable'. The readings coexist as live institutional positions within the ICC and state practice. The international-oversight reading exhibits higher extractiveness and stronger suppression; the national-primacy reading exhibits lower extractiveness and weaker suppression. Each reading has its own ε-invariant victim and beneficiary structure. The readings influence each other: aggressive application of the broad standard drives states to contest it and strengthen domestic procedures; states' resistance to the broad standard creates pressure on the ICC to clarify its standard, which can shift back toward the national-primacy reading. Network link direction: the international-oversight reading (this story) upstream-influences the national-primacy reading because the former's expansive application is what triggers the latter's defensive response.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
