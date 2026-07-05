% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter — Secular Democratic Reading (Civilian Supremacy, Islamist Exclusion)
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   This story instantiates the secular-democratic reading of the July
 *   Charter — one of three structurally distinct constructions of the same
 *   founding text (the others being a guided-nationalist reading grounding
 *   sovereignty in religious-national identity, and a military-custodian
 *   reading ratifying the armed forces as permanent guardian). Under this
 *   reading, the Charter's core commitment is civilian democratic supremacy:
 *   elected institutions govern, the military is subordinate, and political
 *   organizations whose programs are read as incompatible with secular
 *   constitutionalism (chiefly Jamaat-e-Islami and its student wings) are
 *   constrained or excluded from full participation. The coordination
 *   function is real — the settlement genuinely prevents relapse into open
 *   military rule and forecloses a religious-monopoly outcome that many
 *   transition participants feared. But the same enforcement machinery that
 *   holds off those outcomes also transfers standing away from an organized
 *   political constituency and an institutional actor (the military's
 *   autonomous command layer), which is why this reading computes as tangled
 *   rope rather than pure rope: real coordination, riding alongside
 *   identifiable victims who bear the cost through the same courts and
 *   commissions that administer the settlement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.58).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.62).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter — Secular Democratic Reading (Civilian Supremacy, Islamist Exclusion)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, '50933a76-586d-4ea5-adf5-f8875a733edc').
narrative_ontology:cs_kernel_codification('50933a76-586d-4ea5-adf5-f8875a733edc', fixed_text).
narrative_ontology:cs_authority_grounding('50933a76-586d-4ea5-adf5-f8875a733edc', lineage).
narrative_ontology:cs_interpretation_layer_present('50933a76-586d-4ea5-adf5-f8875a733edc').
narrative_ontology:cs_reading_relation('50933a76-586d-4ea5-adf5-f8875a733edc', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('50933a76-586d-4ea5-adf5-f8875a733edc', july_charter_sovereign_legitimacy__military_custodian_reading, coexists_with).
narrative_ontology:cs_axiom('50933a76-586d-4ea5-adf5-f8875a733edc', foundational, sovereignty_grounds_in_popular_civilian_mandate_not_religious_identity).
narrative_ontology:cs_axiom_status(sovereignty_grounds_in_popular_civilian_mandate_not_religious_identity, holdable).
narrative_ontology:cs_axiom_grounding('50933a76-586d-4ea5-adf5-f8875a733edc', sovereignty_grounds_in_popular_civilian_mandate_not_religious_identity, deontological).
narrative_ontology:cs_axiom('50933a76-586d-4ea5-adf5-f8875a733edc', foundational, armed_forces_subordinate_to_elected_civilian_command).
narrative_ontology:cs_axiom_status(armed_forces_subordinate_to_elected_civilian_command, holdable).
narrative_ontology:cs_axiom_grounding('50933a76-586d-4ea5-adf5-f8875a733edc', armed_forces_subordinate_to_elected_civilian_command, conventional).
narrative_ontology:cs_reference_frame('50933a76-586d-4ea5-adf5-f8875a733edc', post_uprising_secular_constitutional_settlement).
narrative_ontology:cs_drift_state('50933a76-586d-4ea5-adf5-f8875a733edc', contemporary_electoral_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50933a76-586d-4ea5-adf5-f8875a733edc', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_civil_society_coalitions).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, constitutional_court_and_election_commission).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_professional_political_class).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_parliamentary_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_conservative_electorate).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, islamist_student_organizations).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_constitutionalism_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_supremacy_over_armed_forces).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and lobbied for the Charter's secular clauses in the aftermath of the uprising, framing religious-party participation and military guardianship as the two threats to a durable transition. They administer the interpretive consensus that keeps the Charter read this way and benefit from the resulting institutional dominance of parties and NGOs aligned with their program.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_civil_society_coalitions, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_civil_society_coalitions, beneficiary).

% Enforce candidate vetting, party registration rules, and eligibility bars that operationalize the secular reading — disqualifying candidates and parties found incompatible with the Charter's secular clauses. Their rulings are the actual enforcement mechanism translating the text into exclusion.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, constitutional_court_and_election_commission, agenda_setter,
    institutional, generational, analytical, national).

% Occupy the electoral space vacated by disqualified or constrained religious parties and by a military formally barred from direct rule. They gain durable governing advantage from a settlement they did not single-handedly write but now depend on for their political survival.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_parliamentary_parties, beneficiary,
    powerful, biographical, constrained, national).

% A mass religious-political organization with genuine electoral constituency, now facing registration challenges, media exclusion, and doctrinal disqualification under the secular reading of the Charter. Cannot appeal to a body outside the same courts and commissions administering the exclusion; framed by the settlement's proponents as a threat to the constitutional order it claims to compete within.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, generational, trapped, national).

% Loses the discretionary latitude it exercised during and immediately after the transition; is subordinated on paper to civilian oversight of budgets, appointments, and deployment decisions. Retains informal leverage through patronage networks and the coercive apparatus, but the Charter text denies it the guardianship role it and its allies argue is necessary for stability.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    institutional, civilizational, constrained, national).

% Voters whose preferred political vehicles are constrained or barred from full participation. They experience the Charter's secular framework as a foreclosure of legitimate electoral choice rather than as neutral procedural rule-making, but have no forum within the current settlement to contest the disqualifications.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_conservative_electorate, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_conservative_electorate, excluded).

% Campus-based organizing wings of the excluded political current, subject to bans and monitoring under the secular reading's public-order provisions. Were active participants in the uprising that produced the Charter but are now treated as its internal enemy under this reading.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, islamist_student_organizations, excluded,
    powerless, biographical, trapped, regional).

% Professional, NGO-linked, and diaspora-connected elites whose worldview and international legitimacy narratives are vindicated by the secular reading; they gain international donor confidence and domestic institutional standing from the Charter being read this way rather than as guided-nationalist or military-custodian.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_professional_political_class, beneficiary,
    powerful, generational, mobile, national).

% Foreign governments and multilateral institutions evaluate whether the Charter's secular-democratic framing satisfies conditions for aid, trade, and diplomatic normalization. Their assessments do not administer the Charter directly but shape which reading receives external validation and resourcing.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_donors_and_partners, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_parliamentary_parties).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a genuine post-uprising coordination solution: a single constitutional text that multiple factions (civilian parties, courts, international partners) can point to as the settled basis for elections, government formation, and civil-military relations, avoiding a return to open contest over who holds sovereign authority.
% TRANSFER_FUNCTION: Moves political participation rights and institutional legitimacy away from religious-political organizations and the military's autonomous command structure, and toward secular civilian parties, courts, and the professional political class administering the settlement.
% ABSENT_VOICES: Jamaat-e-Islami and allied religious-conservative constituencies, and factions within the military leadership who view guardianship as their institutional inheritance, are structurally positioned as the excluded parties this reading is built against; they were active participants in the founding moment but are written out of its authoritative interpretation.
% DISAPPEARANCE_RATIONALE: If the secular-democratic reading lost its enforcement apparatus (courts stopped applying eligibility bars, military patronage networks resumed open political role), civilian parties and secular civil society would lose their structural electoral advantage and would experience this as constitutional collapse; Jamaat-e-Islami and military guardianship advocates would experience it as restoration of rightful standing. Whether 'the world rearranges' depends entirely on which reading one holds — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The uprising that produced the Charter needed to resolve, simultaneously, how to prevent both a return to military-backed authoritarian rule and the consolidation of power by a single religious-political faction, while establishing some sovereign basis for the new order.
% FOUNDING_PROBLEM_CORROBORATION: Secular civil society coalitions and the urban professional political class attest the founding problem (dual threat of military rule and religious-party dominance) remains live and the Charter's secular clauses are its necessary solution. Independent international legal observers and comparative-transition scholars outside the domestic beneficiary coalition corroborate that the dual-threat framing was a real feature of the transition moment, but dispute whether continued exclusion of Jamaat-e-Islami still serves that founding problem or has become a standing advantage detached from it — no fully external corroborator confirms the present-tense necessity of the exclusion.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.38 -> 0.58) as the interpretive consensus hardens from a contested founding moment into settled case law — early disqualifications were framed as narrow public-order measures; by the end of the interval the exclusion has become a durable structural feature of electoral eligibility. Theater ratio climbs moderately (0.20 -> 0.40) reflecting a growing gap between the stated public-order rationale for exclusions and the accumulating pattern of rulings that track political convenience for the incumbent civilian coalition. Suppression is authored as a raw structural property (0.62) — the eligibility-bar and registration-challenge machinery — and is not scaled by scope in this base value; the engine will scale extractiveness, not suppression, by the national spatial_scope carried by most stakeholders. Resistance is high (0.68) because Jamaat-e-Islami and allied constituencies actively contest disqualifications through courts, street mobilization, and international appeals rather than acquiescing.
 *
 * DIRECTIONALITY LOGIC:
 *   Secular civil society coalitions and the constitutional court/election commission sit as agenda-setters: they wrote and now administer the interpretive rule that operationalizes the secular reading, giving them low directionality toward extraction (they are net beneficiaries of the settlement they maintain). Civilian parliamentary parties and the urban professional political class are beneficiaries with mobile or constrained exit — they did not necessarily draft the rule but structurally profit from the exclusion of rival factions. Jamaat-e-Islami, the military's autonomous authority, religious-conservative voters, and Islamist student organizations are the payers: trapped or constrained exit, high effective extraction, because the same institutions administering the Charter are the only forum available to contest their exclusion. International donors are an analytical observer seat whose validation shapes which reading gets external resourcing without directly administering the text.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding both a relapse to military rule and religious-monopoly capture) was genuinely live at the moment of drafting — this is what prevents the story from being pure snare. But the founding-problem-status is authored as contested rather than resolved-live, because independent observers outside the beneficiary coalition question whether continued disqualification of a mass political organization still serves that founding problem or has calcified into a standing advantage for the parties now administering the Charter. Classifying this as tangled_rope rather than snare preserves the coordination function's reality while still registering the asymmetric, enforcement-dependent cost imposed on Jamaat-e-Islami and the military's autonomous command layer — collapsing it to snare would erase the genuine transition-stabilizing function; collapsing it to rope would erase the identifiable victims and the enforcement dependency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the July Charter''s text itself specify a secular-democratic reading as authoritative, or is that reading one contestable interpretation among the guided-nationalist and military-custodian readings, each backed by different factions who participated in the same founding moment?',
    'Textual and drafting-history analysis of the Charter''s actual clauses, cross-referenced against contemporaneous statements by drafting-committee factions; judicial interpretation patterns over subsequent years as courts either converge on or diverge from the secular reading.',
    'If the text underdetermines the reading and the secular construction was substantially the product of which coalition controlled implementation, then the exclusion of Jamaat-e-Islami and constraint on military autonomy are better read as a contested political victory encoded as constitutional necessity, strengthening the tangled_rope (rather than rope) classification. If the text is genuinely univocal toward secular democracy, the coordination function is more clearly load-bearing and less purely a cover for faction advantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the secular reading is textually compelled or one contestable construction among the kernel''s declared sibling readings.').

omega_variable(
    military_leverage_persistence,
    'Does formal subordination of the military to civilian authority under this reading actually reduce the military''s real extractive leverage, or does the institution retain effective autonomous power through patronage networks and coercive capacity regardless of the constitutional text?',
    'Track military budget transparency, appointment authority, and instances of military non-compliance with civilian directives over the measurement interval; compare formal subordination clauses against observed institutional behavior.',
    'If real leverage persists despite formal subordination, the ''victim'' status assigned to military_autonomous_authority in this reading is partly nominal — the constraint extracts less from the military in practice than the text implies, which would lower the military''s effective directionality toward the target end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_leverage_persistence, empirical, 'Whether constitutional subordination of the military translates into actual reduced institutional autonomy.').

omega_variable(
    founding_problem_obsolescence,
    'Has the dual threat (military relapse, religious-party monopoly) that justified the original exclusionary provisions actually receded, such that continued enforcement now serves incumbent advantage rather than the founding problem?',
    'Comparative assessment of whether Jamaat-e-Islami''s current electoral program and organizational capacity still present the specific monopoly risk the founders feared, versus independent polling and organizational analysis from scholars outside the secular coalition.',
    'If the founding problem has substantially receded, the founding_problem_status should shift from contested toward dead, which would strengthen a piton or snare reading of the ongoing exclusion machinery rather than tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the original dual-threat justification for exclusion still holds or has become a legacy justification for entrenched advantage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(july_tr_t32, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(july_tr_t48, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 48, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(july_be_t32, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(july_be_t48, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 48, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(july_su_t32, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(july_su_t48, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 48, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the july_charter_sovereign_legitimacy kernel, each instantiated as a separate story with its own ε and beneficiary/victim structure per the ε-invariance principle. The guided_nationalism_reading grounds sovereignty in religious-national identity and would treat this reading's exclusion of Jamaat-e-Islami as the extractive act; the military_custodian_reading treats the military's guardianship role as a stabilizing constitutional office and would treat this reading's subordination clause as the extractive transfer. All three readings share the same founding text and founding moment but diverge on which actors are coordinated and which are extracted from.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
