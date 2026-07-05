% ============================================================================
% CONSTRAINT STORY: border_normative_status__qualified_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__qualified_sovereignty, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: border_normative_status__qualified_sovereignty
 *   human_readable: Proportionality-Constrained Border Control Authority
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint models the 'qualified sovereignty' reading of border
 *   normative status: states retain the authority to exclude, but must
 *   exercise it proportionately, consistently with human rights obligations,
 *   subject to necessity and least-restrictive-means testing. This is a
 *   distinct constraint from a pure-sovereignty reading (border control as
 *   unconstrained self-determination) and a freedom-primary reading (movement
 *   as a right borders impermissibly restrict) — each of those is a separate
 *   constraint with its own ε and its own victim set, linked here only as
 *   sibling readings of the same underlying kernel about what a border
 *   normatively is. The qualified-sovereignty reading's structural signature
 *   is the adjudication burden it creates: it does not abolish exclusion, it
 *   procedurally regulates it, which generates a large administrative
 *   apparatus (courts, tribunals, review boards) whose growth is itself a
 *   measurable extraction independent of the underlying exclusion decisions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, 0.52).
domain_priors:suppression_score(border_normative_status__qualified_sovereignty, 0.58).
domain_priors:theater_ratio(border_normative_status__qualified_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, extractiveness, 0.52).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(border_normative_status__qualified_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__qualified_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_normative_status__qualified_sovereignty, "Proportionality-Constrained Border Control Authority").
narrative_ontology:topic_domain(border_normative_status__qualified_sovereignty, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__qualified_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__qualified_sovereignty, 'ab7bad9d-2849-4211-adb7-d746e33ace30').
narrative_ontology:cs_kernel_codification('ab7bad9d-2849-4211-adb7-d746e33ace30', distributed).
narrative_ontology:cs_authority_grounding('ab7bad9d-2849-4211-adb7-d746e33ace30', distributed).
narrative_ontology:cs_reading_relation('ab7bad9d-2849-4211-adb7-d746e33ace30', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('ab7bad9d-2849-4211-adb7-d746e33ace30', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_axiom('ab7bad9d-2849-4211-adb7-d746e33ace30', foundational, sovereign_authority_conditional_on_justification).
narrative_ontology:cs_axiom_status(sovereign_authority_conditional_on_justification, holdable).
narrative_ontology:cs_axiom_grounding('ab7bad9d-2849-4211-adb7-d746e33ace30', sovereign_authority_conditional_on_justification, conventional).
narrative_ontology:cs_axiom('ab7bad9d-2849-4211-adb7-d746e33ace30', foundational, proportionality_test_binds_exclusionary_discretion).
narrative_ontology:cs_axiom_status(proportionality_test_binds_exclusionary_discretion, holdable).
narrative_ontology:cs_axiom_grounding('ab7bad9d-2849-4211-adb7-d746e33ace30', proportionality_test_binds_exclusionary_discretion, instrumental).
narrative_ontology:cs_reference_frame('ab7bad9d-2849-4211-adb7-d746e33ace30', post_ww2_human_rights_qualified_sovereignty_settlement).
narrative_ontology:cs_drift_state('ab7bad9d-2849-4211-adb7-d746e33ace30', contemporary_securitized_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ab7bad9d-2849-4211-adb7-d746e33ace30', '').
narrative_ontology:cs_kernel_id(border_normative_status__qualified_sovereignty, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, receiving_state_citizenries).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, border_enforcement_agencies).
narrative_ontology:constraint_beneficiary(border_normative_status__qualified_sovereignty, state_administrative_apparatus).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, displaced_citizens_denied_reentry_or_protection).
narrative_ontology:constraint_victim(border_normative_status__qualified_sovereignty, asylum_seekers_in_prolonged_adjudication).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer border control regimes, claiming authority to exclude non-members while accepting a legal obligation to justify each exercise of that authority as necessary and proportionate to a legitimate interest and consistent with human rights treaties. Builds the adjudication machinery (courts, tribunals, review boards) that this obligation requires, and bears the administrative cost of defending its decisions.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, receiving_state_governments, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a border regime that manages entry to the national community and its labor market and welfare systems, tempered by a proportionality requirement that is presented as legitimating the state's authority over them as well — the same rule that could exclude a migrant could, in principle, be turned against a citizen's own mobility claims if the state misclassifies status.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, receiving_state_citizenries, beneficiary,
    organized, biographical, constrained, national).

% Operate detention, interdiction, and screening infrastructure; their institutional budget, mandate, and legal cover expand with the proportionality doctrine because every exclusion decision now generates a documentation and review requirement that justifies further staffing and process. They benefit from the coordination function directly and are shielded from strict liability by the 'balancing' language of proportionality.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, border_enforcement_agencies, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, border_enforcement_agencies, agenda_setter).

% Seek entry or protection and are turned back, detained, or deported under a legal standard that promises individualized proportionality review but in practice is administered through expedited, high-volume processes with limited access to counsel. The proportionality requirement gives them a legal claim in principle but almost no practical leverage — appeals are slow, underfunded, and often moot by the time they resolve, if they can be filed at all from outside the territory.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Nominal citizens or long-term residents caught by exclusionary practices — denationalization-adjacent measures, denial of consular repatriation, statelessness produced by documentation gaps — who are told the state's border authority does not apply to them but experience functionally identical exclusion, and must litigate their own citizenship status to access the proportionality protections that are supposed to already cover them.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, displaced_citizens_denied_reentry_or_protection, payer,
    powerless, biographical, trapped, national).

% Live for years in legal limbo — neither admitted nor finally excluded — while the adjudication apparatus that proportionality doctrine requires works through backlogged caseloads. The doctrine's promise of individualized justification becomes, in practice, a mechanism for indefinite deferral: the state satisfies the letter of the obligation by having a review process, regardless of how long that process takes or how substantively it examines each case.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, asylum_seekers_in_prolonged_adjudication, payer,
    powerless, biographical, trapped, national).

% Adjudicate individual complaints and issue proportionality standards (necessity, suitability, least-restrictive-means tests) that states are supposed to internalize into domestic border law. They have no direct enforcement power over sovereign border decisions and depend on state compliance, creating a structural gap between the doctrine's ambition and its practical bite.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, human_rights_courts_and_treaty_bodies, observer,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(border_normative_status__qualified_sovereignty, human_rights_courts_and_treaty_bodies, agenda_setter).

% Document individual cases, litigate test cases, and lobby for stricter proportionality enforcement, but are structurally outside the state's decision-making process — they can contest outcomes after the fact but cannot participate in the initial classification of who is admitted, screened, or excluded.
narrative_ontology:constraint_stakeholder(border_normative_status__qualified_sovereignty, migrant_rights_organizations, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework in which states can exercise a real coordination function — managing population flows, labor markets, security screening, and resource allocation at the territorial boundary — while subjecting that authority to a check meant to prevent arbitrary or discriminatory exclusion, coordinating expectations between states, migrants, and human rights bodies about what counts as a legitimate versus illegitimate border decision.
% TRANSFER_FUNCTION: Moves the burden of justification from migrants (who under a pure sovereignty regime would have no claim at all) partly onto states, while moving the practical cost of that justification process — delay, detention, legal uncertainty — onto migrants and displaced citizens who must survive the adjudication period; state administrative and enforcement capacity expands correspondingly, funded by the public and legitimated by the proportionality framework itself.
% ABSENT_VOICES: Excluded migrants themselves rarely have standing or resources to contest specific exclusion decisions before they are executed; migrant rights organizations attempt to represent this interest but operate after the fact, litigating patterns rather than preventing individual harms. Sending states, whose citizens are the ones excluded, have limited standing in receiving-state proportionality review.
% DISAPPEARANCE_RATIONALE: If the proportionality qualification vanished and pure sovereignty control were restored, receiving states would lose no operational capacity — they already control the border in practice — but migrants and displaced citizens would lose their only legal foothold for contesting individual exclusions, and human rights courts would lose their doctrinal basis for review. States would experience little disruption; excluded populations would experience the loss of a (currently weak but real) constraint on arbitrary exclusion. Whether the world 'rearranges' therefore depends on which seat is asked — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: Post-WWII and post-colonial international law needed to reconcile two conflicting commitments already embedded in the state system: the principle of territorial sovereignty (state consent to admission) and the emerging human rights regime's commitment to non-arbitrary treatment of individuals, refugee protection, and non-discrimination. Pure sovereignty doctrine had produced or tolerated mass expulsions, statelessness, and refusal of refuge; the qualified framework was built to prevent the worst of these outcomes without abolishing border control itself.
% FOUNDING_PROBLEM_CORROBORATION: Human rights treaty bodies and international law scholars outside any state's border enforcement apparatus attest that the founding problem (preventing arbitrary exclusion and statelessness) remains substantially live — documented cases of pushbacks, prolonged detention, and denationalization continue at scale. Migrant rights organizations, also outside the benefiting state apparatus, corroborate that the doctrine's protective function is real but severely under-enforced in practice, functioning more as a legitimating vocabulary for state discretion than as an effective check — a live problem the arrangement has not yet solved, per both an institutional and a civil-society source outside the beneficiary set.
narrative_ontology:disappearance_verdict(border_normative_status__qualified_sovereignty, contested).
narrative_ontology:founding_problem_status(border_normative_status__qualified_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__qualified_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__qualified_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__qualified_sovereignty, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__qualified_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__qualified_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__qualified_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) and suppression (0.58) sit at a moderate-high midpoint reflecting a genuine coordination function (managing entry, screening, resource allocation) fused with real asymmetric cost-bearing (migrants and displaced citizens absorb detention, delay, and legal uncertainty while the doctrine's protective promise is inconsistently realized). Theater ratio (0.42, rising over the interval) captures the growing gap between the proportionality doctrine's procedural apparatus (case files, review hearings, published standards) and its substantive bite — the process increasingly exists to demonstrate that a balancing test was performed rather than to reliably prevent disproportionate exclusion. Accessibility collapse (0.45) is moderate: legal alternatives (litigation, treaty complaints) exist on paper but are practically inaccessible to most excluded migrants, so alternatives are not fully collapsed but are severely degraded. Resistance (0.6) reflects the active contestation this doctrine generates — litigation, activism, and treaty-body pressure are constant features, unlike a mountain that would meet little resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the state agenda-setter seat, the constraint looks like principled self-limitation — sovereignty voluntarily constrained by law. From the excluded-migrant payer seat, the same structure looks like sovereignty largely intact, dressed in procedural language that rarely changes outcomes. The engine should register this divergence structurally: institutional power + arbitrage exit for the state seat versus powerless + trapped exit for the payer seats produces very different effective extraction even though the underlying legal text is identical.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state governments and enforcement agencies sit near the beneficiary end: they set the terms of proportionality, administer the review process, and their institutional capacity expands under the doctrine's documentation requirements. Excluded migrants, displaced citizens denied reentry, and asylum seekers in prolonged adjudication sit near the full-target end: trapped exit options, powerless structural position, and the doctrine's protections are theoretically available to them but practically inaccessible given resource and access asymmetries. Citizens of receiving states are genuine but attenuated beneficiaries — they get border management without bearing its administrative costs directly, though the same legal machinery could in principle misclassify their own status.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing arbitrary exclusion and statelessness) is contested as live or dead depending on the seat: state administrators would say the problem is substantially managed by the doctrine's existence, while human rights bodies and rights organizations outside the state apparatus attest that pushbacks, prolonged detention, and denationalization continue at scale — the problem the doctrine was built to solve remains active, but the doctrine's practical enforcement has not kept pace, producing a widening theater/substance gap rather than either full resolution or full mandatrophy. This is exactly the divergence the classification is meant to surface: a tangled_rope reading holds both a real coordination function and a real, asymmetric extraction simultaneously, rather than forcing the constraint into either a pure-coordination (rope) or pure-extraction (snare) box.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_doctrine_capture_vs_genuine_constraint,
    'Is the proportionality requirement a genuine, binding constraint on state exclusionary power, or has it been substantially captured by states into a procedural legitimation ritual that changes little about actual outcomes?',
    'Comparative outcome analysis: track reversal rates of exclusion decisions under proportionality review across jurisdictions and over time. A doctrine functioning as genuine constraint would show meaningful reversal rates and behavioral change in state screening practices; a captured doctrine would show near-uniform affirmance regardless of case specifics.',
    'If captured, the qualified-sovereignty reading is closer in practice to the sovereignty_primary reading than its formal legal text suggests, and this constraint''s classification would drift toward snare over time as the coordination cover thins. If genuine, the tangled_rope classification with real coordination function is well-supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_doctrine_capture_vs_genuine_constraint, empirical, 'Whether proportionality review meaningfully constrains state exclusion or merely legitimates it procedurally.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is qualified_sovereignty the correct single reading of the border_normative_status kernel for a given real-world border regime, or do different institutional actors within the same state (courts vs. executive agencies) operate on different readings simultaneously, making the kernel''s instantiation itself contested within a single jurisdiction?',
    'Institutional mapping: compare judicial doctrine (which often explicitly adopts proportionality language) against executive agency operational practice (which may function closer to unconstrained discretion) within the same state over the same period.',
    'If different institutional actors within one state genuinely operate on different kernel readings, the qualified_sovereignty constraint as authored here may itself need to be split further — into a judicial-doctrine reading and an executive-practice reading — rather than treated as one coherent state-level constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether qualified_sovereignty is a single coherent reading or masks further internal state-level reading divergence.').

omega_variable(
    displaced_citizen_boundary_ambiguity,
    'Where exactly does the line sit between a ''displaced citizen'' whose exclusion is a due-process failure within the qualified-sovereignty framework versus a genuine statelessness case that falls outside any state''s proportionality obligation entirely?',
    'Case-law review of denationalization and non-admission cases to identify where courts locate this boundary and how consistently.',
    'A narrow boundary would mean fewer cases genuinely test the doctrine''s citizen-protection function; a wide boundary would mean the doctrine''s victim set is larger than commonly recognized, strengthening the case for classifying more denationalization practice under this constraint rather than treating it as an external gap.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displaced_citizen_boundary_ambiguity, conceptual, 'Ambiguity in where displaced-citizen protection under this doctrine ends and unprotected statelessness begins.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__qualified_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__qualified_sovereignty, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__qualified_sovereignty, theater_ratio, 8, 0.29).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__qualified_sovereignty, theater_ratio, 16, 0.33).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__qualified_sovereignty, theater_ratio, 24, 0.37).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__qualified_sovereignty, theater_ratio, 32, 0.4).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__qualified_sovereignty, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__qualified_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bord_be_t8, border_normative_status__qualified_sovereignty, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(bord_be_t16, border_normative_status__qualified_sovereignty, base_extractiveness, 16, 0.46).
narrative_ontology:measurement(bord_be_t24, border_normative_status__qualified_sovereignty, base_extractiveness, 24, 0.49).
narrative_ontology:measurement(bord_be_t32, border_normative_status__qualified_sovereignty, base_extractiveness, 32, 0.51).
narrative_ontology:measurement(bord_be_t40, border_normative_status__qualified_sovereignty, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__qualified_sovereignty, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(bord_su_t8, border_normative_status__qualified_sovereignty, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(bord_su_t16, border_normative_status__qualified_sovereignty, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(bord_su_t24, border_normative_status__qualified_sovereignty, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(bord_su_t32, border_normative_status__qualified_sovereignty, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(bord_su_t40, border_normative_status__qualified_sovereignty, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__qualified_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__qualified_sovereignty, border_normative_status__freedom_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the border_normative_status kernel. sovereignty_primary treats border control as foundational and largely unconstrained; freedom_primary treats movement as a right borders impermissibly restrict; qualified_sovereignty (this story) institutionalizes the tension between the two as a proportionality balancing test, producing a distinctive adjudication-burden signature (rising theater_ratio, administrative apparatus growth) absent from either pole reading. Each reading has a different ε, different beneficiary/victim structure, and different classification-appropriate metrics; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
