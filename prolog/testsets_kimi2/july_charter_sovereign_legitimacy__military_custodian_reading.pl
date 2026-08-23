% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__military_custodian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__military_custodian_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__military_custodian_reading
 *   human_readable: Military Custodianship Constitutional Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   A post-revolutionary charter ratifies the military as a permanent
 *   institutional guardian tasked with ensuring stability. The military
 *   custodian reading treats this provision as a constitutional mandate for
 *   ongoing military veto over civilian authority, subordinating parliament,
 *   parties, and the judiciary to security assessments. The expected
 *   structural delta centers on permanent civilian subordination rather than
 *   transitional stabilization: autonomous political parties and student
 *   movements bear the costs of bounded contestation, while the military
 *   hierarchy and security apparatus collect budgetary autonomy, legal
 *   impunity, and sovereign prerogative.
 *
 * KEY AGENTS:
 *   - military_hierarchy: Primary beneficiary/agenda_setter (institutional/identity_locked) â extracts political veto and budgetary autonomy.
 *   - security_apparatus: Secondary beneficiary/enforcer (organized/identity_locked) â enforces boundaries, collects impunity.
 *   - autonomous_political_parties: Primary target (organized/constrained) â bears subordination and red-line restrictions.
 *   - student_movement: Primary target (powerless/constrained) â bears suppression of contestation.
 *   - civilian_institutions: Secondary target (moderate/constrained) â bears reversible authority.
 *   - international_human_rights_observers: Analytical observer (organized/analytical) â sees full structure without enforcement power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, 0.84).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__military_custodian_reading, 0.89).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__military_custodian_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, extractiveness, 0.84).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__military_custodian_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__military_custodian_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__military_custodian_reading, "Military Custodianship Constitutional Reading").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__military_custodian_reading, "constitutional/political").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__military_custodian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__military_custodian_reading, 'c73f3a89-1212-463e-bd6d-574420a016b3').
narrative_ontology:cs_kernel_codification('c73f3a89-1212-463e-bd6d-574420a016b3', formalized).
narrative_ontology:cs_authority_grounding('c73f3a89-1212-463e-bd6d-574420a016b3', extraction).
narrative_ontology:cs_interpretation_layer_present('c73f3a89-1212-463e-bd6d-574420a016b3').
narrative_ontology:cs_reading_relation('c73f3a89-1212-463e-bd6d-574420a016b3', july_charter_sovereign_legitimacy__secular_democratic_reading, forecloses).
narrative_ontology:cs_reading_relation('c73f3a89-1212-463e-bd6d-574420a016b3', july_charter_sovereign_legitimacy__guided_nationalism_reading, coexists_with).
narrative_ontology:cs_axiom('c73f3a89-1212-463e-bd6d-574420a016b3', foundational, military_guardianship_constitutional_prerogative).
narrative_ontology:cs_axiom_status(military_guardianship_constitutional_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('c73f3a89-1212-463e-bd6d-574420a016b3', military_guardianship_constitutional_prerogative, conventional).
narrative_ontology:cs_axiom('c73f3a89-1212-463e-bd6d-574420a016b3', foundational, civilian_supremacy_entails_state_fragility).
narrative_ontology:cs_axiom_status(civilian_supremacy_entails_state_fragility, holdable).
narrative_ontology:cs_axiom_grounding('c73f3a89-1212-463e-bd6d-574420a016b3', civilian_supremacy_entails_state_fragility, empirically_contingent).
narrative_ontology:cs_reference_frame('c73f3a89-1212-463e-bd6d-574420a016b3', military_guardianship_framework).
narrative_ontology:cs_drift_state('c73f3a89-1212-463e-bd6d-574420a016b3', post_charter_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c73f3a89-1212-463e-bd6d-574420a016b3', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, military_hierarchy).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_institutions).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, military_guardianship_doctrine).
narrative_ontology:constraint_vindicates(july_charter_sovereign_legitimacy__military_custodian_reading, stability_over_sovereignty_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls constitutional interpretation and holds veto authority over legislation and executive appointments; derives budgetary autonomy, institutional privileges, and legal immunity from the custodian role; professional identity is fused with national survival.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, military_hierarchy, agenda_setter,
    institutional, generational, identity_locked, national).

% Enforces the boundaries of permissible political activity through surveillance, licensing, and judicial processes; receives budgetary allocations and legal impunity; organizational survival depends on maintaining threat narratives.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__military_custodian_reading, security_apparatus, agenda_setter).

% Formally permitted to compete but face dissolution or disqualification if they challenge military prerogatives; must accept red lines that reserve sovereign decisions to the military; cannot translate electoral success into control over security or budget policy.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, autonomous_political_parties, payer,
    organized, biographical, constrained, national).

% Organizes protests and advocacy against military custodianship; subject to campus surveillance, preventive detention, and legal harassment; their ability to assemble is bounded by security assessments rather than electoral rules.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, student_movement, payer,
    powerless, biographical, constrained, national).

% Parliament, judiciary, and bureaucracy retain formal existence but operate under implicit or explicit military veto; their decisions on security appointments, budget, and foreign policy are reversible by the military.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, civilian_institutions, payer,
    moderate, generational, constrained, national).

% Document restrictions on political participation and military trials of civilians; lack enforcement power but provide external legitimacy assessments that can pressure or shield the regime.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__military_custodian_reading, international_human_rights_observers, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__military_custodian_reading, military_hierarchy).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__military_custodian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes authority during post-revolutionary transition to prevent state fragmentation and civil conflict; claims to provide order while civilian capacity develops.
% TRANSFER_FUNCTION: Transfers political autonomy and veto power from elected civilian institutions and social movements to the military-security hierarchy.
% ABSENT_VOICES: Secular democrats advocating full civilian supremacy, liberal constitutionalists who reject a permanent military role, and regional actors who view permanent militarization as illegitimate are excluded from the charter drafting process or subsequent amendment procedures.
% DISAPPEARANCE_RATIONALE: Without the military custodian clause, civilian institutions would regain full sovereignty, political contestation would expand beyond security red lines, and the military would revert to subordinate status or directly seize power â the constitutional order would reorganize.
% FOUNDING_PROBLEM: Post-revolutionary collapse of the prior regime left a vacuum of legitimate authority; risk of civil war, sectarian conflict, or state fragmentation required a stabilizing force during transition.
% FOUNDING_PROBLEM_CORROBORATION: Military leadership attests instability remains live. Civilian opposition and some international observers attest the founding crisis has passed and the custodianship persists as institutionalized extraction; however, regional instability and coup cycles provide genuine corroboration from outside the beneficiary set that fragility remains.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__military_custodian_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__military_custodian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__military_custodian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__military_custodian_reading, 0.84, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__military_custodian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__military_custodian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.84) because the charter permanently transfers veto authority and sovereign prerogatives from civilian institutions to the military, decoupling political power from electoral accountability. Suppression is higher (0.89) because the constraint depends on actively excluding civilian supremacy alternatives through military courts, party licensing, and security surveillance. Theater is moderate (0.46): the stability narrative retains some genuine believers but an increasing share of institutional activity is performative maintenance of a guardianship role that has drifted into direct political management. Resistance (0.72) reflects sustained opposition from students and political parties despite repression. Accessibility collapse (0.78) indicates that alternatives such as civilian supremacy or transitional sunset clauses have been structurally closed by constitutional design and enforcement practice.
 *
 * PERSPECTIVAL GAP:
 *   The military hierarchy and security apparatus experience the constraint as legitimate constitutional order and necessary stability provision; their computed seat type will lean toward coordination or low-extraction governance. The autonomous political parties, student movement, and civilian institutions experience the same structure as the active suppression of self-determination; their computed seat type will lean toward extraction or snare. The engine derives this divergence from the same structural data via directionality: beneficiaries with identity-locked exit sit near the low-d end, while trapped or constrained payers with national scope sit near the high-d end.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to the military hierarchy and security apparatus: they collect budgetary autonomy, legal impunity, and veto power. Victim declarations map to political parties, students, and civilian institutions: they bear the costs of subordinated sovereignty and bounded contestation. The military hierarchy's identity-locked exit (professional identity fused with guardianship) does not negate its beneficiary status; the engine derives low directionality for this seat. The student movement's constrained exit and powerless status amplify its effective extraction. International observers have analytical exit and no material stake, placing them outside the extraction vector.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â post-revolutionary vacuum and fragmentation risk â was plausibly live at the moment of charter ratification. However, the constraint's classification as tangled rope rather than scaffold hangs on the absence of a sunset clause and the permanence of military prerogatives. If the founding problem is dead and the arrangement persists without credible transition, mandatrophy is present: the mandate has outlived its function. The measurements show rising theater and extractiveness over time, consistent with a coordination function atrophying into steady-state extraction. The classification prevents mislabeling by requiring both beneficiaries and victims: a pure scaffold would have only beneficiaries and a sunset; a pure snare would have no genuine coordination claim. The temporal record of increasing theater and extraction supports the tangled rope diagnosis â a real coordination function captured and converted into permanent asymmetric transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_foreclosure,
    'Does the military custodian reading logically foreclose the secular democratic reading within any single constitutional framework, or can both readings coexist as live interpretive options?',
    'Structural analysis of whether permanent military veto and civilian supremacy can coexist in one constitutional order; evidence from attempted hybrid regimes.',
    'If genuinely foreclosed, the constraint family represents mutually exclusive regimes; if not, the dispute is political contestation within a shared text.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relationship between military custodianship and civilian supremacy.').

omega_variable(
    permanence_vs_transitional_mandate,
    'Is the military custodianship a permanent constitutional structure or a transitional mandate that lost its sunset clause?',
    'Historical comparison with other post-revolutionary custodianships; identification of any constitutional exit procedure or scheduled review.',
    'If transitional, the constraint is a degraded scaffold or tangled rope with a missing sunset; if permanent, it is steady-state extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_vs_transitional_mandate, empirical, 'Whether custodianship is permanent or failed transition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of political contestation primarily structural (military courts, party licensing) or internalized (self-censorship, resignation)?',
    'Trajectory analysis after periods of liberalization: rapid expansion of contestation indicates structural suppression; persistent caution indicates internalization.',
    'Internalized suppression raises effective extraction above structural measures and indicates cognitive capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    nationalist_reading_coexistence,
    'Does the guided nationalism reading share sufficient institutional interests with the military custodian reading to form a stable coalition, or do they compete for sovereign authority?',
    'Factional mapping within the charter coalition; whether religious-nationalist legitimacy is subordinate to or rival with military authority.',
    'If rival, the tangled rope may tighten into a snare as the military purges nationalist competitors; if allied, extraction is distributed across a broader beneficiary base.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nationalist_reading_coexistence, conceptual, 'Relationship between military and nationalist sovereignty claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__military_custodian_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(july_tr_t32, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 32, 0.44).
narrative_ontology:measurement(july_tr_t40, july_charter_sovereign_legitimacy__military_custodian_reading, theater_ratio, 40, 0.46).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 16, 0.75).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 24, 0.8).
narrative_ontology:measurement(july_be_t32, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 32, 0.83).
narrative_ontology:measurement(july_be_t40, july_charter_sovereign_legitimacy__military_custodian_reading, base_extractiveness, 40, 0.84).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 16, 0.82).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 24, 0.86).
narrative_ontology:measurement(july_su_t32, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 32, 0.88).
narrative_ontology:measurement(july_su_t40, july_charter_sovereign_legitimacy__military_custodian_reading, suppression_requirement, 40, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__military_custodian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__secular_democratic_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__military_custodian_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).

% DUAL FORMULATION NOTE:
% The july_charter_sovereign_legitimacy kernel decomposes into three structurally distinct readings: secular democratic (civilian supremacy), military custodian (permanent military veto), and guided nationalism (Islamic-nationalist sovereignty). Each reading has a different beneficiary/victim structure and epsilon profile. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
