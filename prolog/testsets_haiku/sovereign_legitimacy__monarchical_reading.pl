% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Legitimacy: Divine-Right Authority via Inherited Succession
 *   domain: political/constitutional
 *
 * SUMMARY:
 *   This is the monarchical reading of the contested sovereign_legitimacy
 *   kernel. It asserts that legitimate political authority flows downward
 *   through inherited bloodline, sanctified by divine anointing and religious
 *   doctrine. The ruling class consists of hereditary nobility whose
 *   legitimacy is defended through both theological endorsement (clergy
 *   provide sacred validation) and coercive machinery (military and legal
 *   system suppress alternative claims). The constraint coordinates a unified
 *   command structure and establishes recognized succession procedures,
 *   solving the problem of stable authority in pre-constitutional realms. It
 *   simultaneously extracts massive revenue and service from the subject
 *   population, who are taught that questioning the arrangement is blasphemy
 *   and treason. The reading is one node in a constraint family: its sibling
 *   readings—the republican reading (authority upward from popular consent)
 *   and the constitutional hybrid reading (dual-source legitimacy with
 *   constitutional mediation)—occupy different parties' positions in ongoing
 *   political struggle. The kernel itself is contested; the readings coexist
 *   as live alternatives in different regimes and as competing claims within
 *   reform movements. The measured metrics (0.78 extractiveness, 0.86
 *   suppression) reflect the reading's historical operation: high extraction
 *   because the bloodline mechanism concentrates benefit, high suppression
 *   because the framework must actively prevent alternative legitimacy claims
 *   and succession-contending lineages from gaining traction.
 *
 * KEY AGENTS:
 *   - Reigning Monarch: institutional power, sets agenda through divinely-sanctioned authority. Identity-locked to role by succession law; cannot exit without delegitimizing the entire framework.
 *   - Aristocratic Nobility: organized power, hold beneficiary + secondary agenda-setter role. Participate in rule through court, regional governance, and advisory parliament. Structurally dependent on monarch for title confirmation.
 *   - Clergy and Religious Authorities: institutional power, provide legitimacy through coronation rite, theological sanction, and pulpit endorsement. Constrained because delegitimizing the crown risks their own authority.
 *   - Subject Population: powerless, structurally payers. Expected to obey, pay taxes, serve military. Taught that questioning the order is blasphemy. Identity-locked through religious narrative and coercive suppression of alternatives.
 *   - Merchant and Craft Classes: moderate power, constrained payers. Possess economic leverage but no formal political voice. License-dependent on crown, hence structurally subordinate.
 *   - Religious Minorities: powerless, actively suppressed. Identity-locked by religious affiliation and legal status. Excluded from sacred legitimation sources and often subject to conversion pressure or exile.
 *   - Succession Contenders: excluded from decision frame unless militarily strong. The bloodline rule creates structural ambiguity about collateral lines, generating recurring crisis moments.
 *   - Foreign Powers: institutional observers. Recognition of legitimacy affects effective power but does not alter formal claim within the framework.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.78).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.86).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.86).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Legitimacy: Divine-Right Authority via Inherited Succession").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political/constitutional").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, 'd8463944-a735-40d1-b30a-e0daad9de099').
narrative_ontology:cs_kernel_codification('d8463944-a735-40d1-b30a-e0daad9de099', fixed_text).
narrative_ontology:cs_authority_grounding('d8463944-a735-40d1-b30a-e0daad9de099', lineage).
narrative_ontology:cs_interpretation_layer_present('d8463944-a735-40d1-b30a-e0daad9de099').
narrative_ontology:cs_reading_relation('d8463944-a735-40d1-b30a-e0daad9de099', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8463944-a735-40d1-b30a-e0daad9de099', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('d8463944-a735-40d1-b30a-e0daad9de099', foundational, authority_descends_through_blood).
narrative_ontology:cs_axiom_status(authority_descends_through_blood, holdable).
narrative_ontology:cs_axiom_grounding('d8463944-a735-40d1-b30a-e0daad9de099', authority_descends_through_blood, conventional).
narrative_ontology:cs_axiom('d8463944-a735-40d1-b30a-e0daad9de099', foundational, divine_sanction_legitimates_heredity).
narrative_ontology:cs_axiom_status(divine_sanction_legitimates_heredity, holdable).
narrative_ontology:cs_axiom_grounding('d8463944-a735-40d1-b30a-e0daad9de099', divine_sanction_legitimates_heredity, theological).
narrative_ontology:cs_axiom('d8463944-a735-40d1-b30a-e0daad9de099', secondary, subjects_owe_obedience_to_anointed_sovereign).
narrative_ontology:cs_axiom_status(subjects_owe_obedience_to_anointed_sovereign, holdable).
narrative_ontology:cs_axiom_grounding('d8463944-a735-40d1-b30a-e0daad9de099', subjects_owe_obedience_to_anointed_sovereign, deontological).
narrative_ontology:cs_reference_frame('d8463944-a735-40d1-b30a-e0daad9de099', divinely_sanctioned_hereditary_succession).
narrative_ontology:cs_drift_state('d8463944-a735-40d1-b30a-e0daad9de099', enlightenment_and_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d8463944-a735-40d1-b30a-e0daad9de099', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_class).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, aristocratic_hierarchy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, subject_population).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, merchant_and_craft_classes).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, religious_minorities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint concentrates benefit to hereditary class (monarch + nobility + clergy) at the expense of the subject population, merchant classes, and religious minorities. The monarchy does provide genuine coordination (unified command, recognized succession) so the extraction is not pure snare; it is tangled rope—genuine coordination function coupled with asymmetric benefit distribution. Suppression (0.86) is very high because the constraint's persistence depends on active enforcement machinery (army, law, inquisition, execution of heresy) to prevent alternative legitimacy claims from gaining traction. The bloodline rule itself generates succession crises (ambiguous collateral claims), so suppression intensity rises during succession moments (T=150-250 shows suppression_requirement peaking). Theater ratio (0.62) is substantial because as the constraint matures, an increasing share of enforcement activity defends the bloodline's exclusive claim to legitimacy rather than the coordination function itself. Coronation rituals, processions, and theological pronouncements proliferate as the framework ossifies. Accessibility collapse (0.81) is high because alternatives to the monarchical legitimacy structure are actively suppressed (heresy, republicanism, constitutionalism are capital crimes) and subjects are taught that alternative authority structures are metaphysically impossible (divinely forbidden). Resistance (0.72) reflects the fact that the constraint DOES meet active resistance from excluded nobility (succession contenders), from religious minorities (quiet non-compliance, emigration), from merchant classes (charter negotiations, smuggling), and from subject populations (peasant rebellions, folk traditions that preserve memory of lost autonomy). The measurement series shows extractiveness and suppression rising over time (T0 to T250), plateauing after T250, which models the constraint's maturation and institutional hardening. Suppression intensity at every level is very high (individual-level suppression peaks at 0.89 at T=400), indicating internalizing mechanisms (identity fusion, theological conditioning) work alongside external enforcement. Class-level resistance also rises (from 0.45 to 0.79), indicating that organized class consciousness (craft guilds, merchant associations, peasant solidarity) develops over time and poses growing structural pressure.
 *
 * PERSPECTIVAL GAP:
 *   The monarch's and clergy's seat should compute as beneficiary/coordinating (they set the rules, collect the benefit, see the constraint as genuinely necessary stability structure). The subject population's seat should compute as target/constrained (they pay through taxation and obedience, see the constraint as coercive extraction dressed in theology). The merchant and craft classes' seat should compute as mixed (they benefit from unified currency and trade law, but pay through taxation and license-dependence; their directionality sits around 0.5, symmetric to slightly target-leaning). The succession contender seat is structurally unable to access the framework's legitimacy sources, so they compute as outside the constraint's normal operation (excluded rather than seated). The clergy's seat is ambiguous: they provide legitimacy (beneficiary function) but are constrained by dependence on the crown's power to enforce their doctrine. If the crown loses military strength, clergy cannot independently sustain the sacred legitimation. This directionality ambiguity is captured in the directionality_overrides section below.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the hereditary ruling class (monarch, aristocratic nobility, clergy). They are institutional and powerful. Their directionality is near the beneficiary end: d~0.15-0.25. They set the rules, collect the revenue, and control the narrative. They have low exit options (trapped and identity-locked: an aristocrat who renounces title and lineage becomes powerless; a clergy member who rejects divine-right theology loses their authority). Their low exit options and beneficiary role keep them on the beneficiary side of directionality. Victims are the subject population (powerless), merchant classes (moderate power but constrained), and religious minorities (powerless, actively suppressed). Their directionality is near the target end: d~0.75-0.90. They pay through taxation, obedience, and labor service. They have trapped or identity-locked exit (subjects cannot easily emigrate; religious minorities cannot change their religious identity without apostasy). The merchant classes occupy an intermediate position: d~0.55-0.65 (moderate power, constrained exit, mixed benefit/cost). Religious minorities are purely targeted: d~0.88 (powerless, identity-locked, no benefit, active suppression). Succession contenders are outside the normal structure (excluded) and would compute as neither beneficiary nor target unless they successfully challenge the reigning monarch—their directionality is contingent on military outcome, not structural position within the constraint. No directionality overrides are needed; the structural derivation from beneficiary/victim + exit + power correctly produces the directional picture.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint solves a real coordination problem (stable succession in a realm without written constitution or electoral mechanism). However, as merchant power rises, literacy spreads, and alternative legitimacy narratives (republicanism, constitutionalism) become articulated in written form, the founding problem statement ('how do we establish recognized authority and succession procedures') becomes increasingly separable from the monarchical solution. The theater ratio rising (0.48 to 0.62) models the constraint's mandatrophy: increasing share of enforcement activity defends the bloodline's exclusive legitimacy rather than the coordination function itself. In the later measurements (T=250-400), the crown is suppressing alternative legitimacy frameworks (republican clubs, constitutional reform societies, heretical theology) rather than merely administering a universally-accepted order. This is the signature of a constraint whose founding mandate has atrophied but whose extraction and suppression machinery persists. The fact that suppression rises even as the constraint matures (rather than settling to a plateau) indicates institutional lock-in: the aristocracy and clergy have built careers and identities around the monarchical frame, so they defend it not because it solves the founding problem but because dismantling it would dissolve their power. This is classic piton drift. However, the constraint still claims tangled_rope because the coordination function is real and measurable in the early period (T=0-100), and even in the later period (T=250-400) a residual coordination benefit persists. The classification reflects that tension: the engine may compute per-seat differences (the monarch's seat as rope, the subject seat as snare) that expose the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_sanction_vs_institutional_power,
    'Is the constraint''s persistence grounded in genuine belief in divine sanction, or in the institutional machinery that claims to represent it?',
    'Historical comparison of legitimacy erosion in regimes where theological education spread versus those where it remained monopolized; correlation between clergy loyalty and succession stability.',
    'If grounded in genuine belief, widespread literacy and theological contestation can dissolve the constraint (disenchantment). If grounded in machinery, it persists through institutional inertia (piton dynamics) even as belief erodes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_sanction_vs_institutional_power, empirical, 'Legitimacy grounded in genuine collective belief versus institutional theater.').

omega_variable(
    succession_fragility,
    'Does the bloodline-based succession rule resolve succession disputes, or does it structurally generate them by creating ambiguity about which collateral line holds the legitimate claim?',
    'Empirical count of succession crises per century under monarchical vs. elective vs. designated-successor regimes; historical record of civil wars triggered by succession ambiguity.',
    'If the rule generates more disputes than it resolves, the coordinating function it claims to provide is illusory and extractiveness is pure rent-seeking; the constraint should compute toward snare. If it reduces disputes relative to the alternative, the coordination benefit is real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(succession_fragility, empirical, 'Whether blood-succession rule stabilizes or destabilizes authority.').

omega_variable(
    subject_belief_vs_coercion,
    'Do subjects obey the monarchical constraint because they believe the divine-right narrative, or because the suppression machinery makes rebellion lethal?',
    'Behavioral measure: when suppression capacity declines (civil war, weakened military, plague), does compliance collapse immediately (indicating coercion-based) or persist (indicating belief-based)? Qualitative evidence from rebellion manifestos, sermon records, and exile writings.',
    'If coercion-based, the constraint is fragile to enforcement decay and should show rapid unraveling when suppression fails. If belief-based, it persists longer across enforcement gaps. Mixed evidence suggests the suppression metric understates actual resilience (belief acts as invisible enforcement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subject_belief_vs_coercion, empirical, 'Whether obedience is enforced or genuinely believed.').

omega_variable(
    kernel_contest_sibling_readings,
    'This constraint is one reading of the contested sovereign_legitimacy kernel. Which alternative readings (republican_reading, constitutional_hybrid_reading) are live possibilities, and what structural conditions cause realms to drift between readings?',
    'Historical case analysis of institutional drift: England (monarchical → constitutional hybrid → elements of republican); France (monarchical → attempted republican → restoration → hybrid); Switzerland (scattered monarchical → cantonal republics). Identify the structural pressures (class composition, literacy, merchant power, foreign models) that nudge legitimacy frameworks.',
    'If the readings coexist as permanent alternatives held by different factions, classify them as coexisting. If one reading has been historically superseded by another within the same jurisdiction, the earlier reading has entered inertial (piton) territory relative to the successor. This omega documents the kernel-level reading relations that cs_structure.reading_relations must specify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_sibling_readings, conceptual, 'Inter-reading structural relationships and drift conditions.').

omega_variable(
    aristocratic_dependence_on_monarch,
    'Are the aristocratic nobility genuine beneficiaries (collecting from extraction), or are they also structurally payers (depending on the monarch for their titles and hence vulnerable to losing everything)?',
    'Historical evidence of arbitrary title revocation, confiscation, and favor-seeking behavior by nobility toward the monarch. If frequent, nobility are subordinate payers despite their power level. If rare, they are secure beneficiaries.',
    'If nobility are partly vulnerable payers (identity-locked to the hierarchy, unable to exit without losing status), the beneficiary/victim line becomes blurred. Some nobles become secondary victims. The directionality for the organized power level shifts toward target-side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aristocratic_dependence_on_monarch, empirical, 'Whether nobility are beneficiaries or secondarily vulnerable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sove_tr_t50, sovereign_legitimacy__monarchical_reading, theater_ratio, 50, 0.52).
narrative_ontology:measurement(sove_tr_t100, sovereign_legitimacy__monarchical_reading, theater_ratio, 100, 0.56).
narrative_ontology:measurement(sove_tr_t150, sovereign_legitimacy__monarchical_reading, theater_ratio, 150, 0.59).
narrative_ontology:measurement(sove_tr_t200, sovereign_legitimacy__monarchical_reading, theater_ratio, 200, 0.6).
narrative_ontology:measurement(sove_tr_t250, sovereign_legitimacy__monarchical_reading, theater_ratio, 250, 0.61).
narrative_ontology:measurement(sove_tr_t300, sovereign_legitimacy__monarchical_reading, theater_ratio, 300, 0.62).
narrative_ontology:measurement(sove_tr_t350, sovereign_legitimacy__monarchical_reading, theater_ratio, 350, 0.62).
narrative_ontology:measurement(sove_tr_t400, sovereign_legitimacy__monarchical_reading, theater_ratio, 400, 0.62).

% Extraction over time
narrative_ontology:measurement(sove_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(sove_be_t50, sovereign_legitimacy__monarchical_reading, base_extractiveness, 50, 0.71).
narrative_ontology:measurement(sove_be_t100, sovereign_legitimacy__monarchical_reading, base_extractiveness, 100, 0.74).
narrative_ontology:measurement(sove_be_t150, sovereign_legitimacy__monarchical_reading, base_extractiveness, 150, 0.76).
narrative_ontology:measurement(sove_be_t200, sovereign_legitimacy__monarchical_reading, base_extractiveness, 200, 0.77).
narrative_ontology:measurement(sove_be_t250, sovereign_legitimacy__monarchical_reading, base_extractiveness, 250, 0.78).
narrative_ontology:measurement(sove_be_t300, sovereign_legitimacy__monarchical_reading, base_extractiveness, 300, 0.79).
narrative_ontology:measurement(sove_be_t350, sovereign_legitimacy__monarchical_reading, base_extractiveness, 350, 0.78).
narrative_ontology:measurement(sove_be_t400, sovereign_legitimacy__monarchical_reading, base_extractiveness, 400, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.73).
narrative_ontology:measurement(sove_su_t50, sovereign_legitimacy__monarchical_reading, suppression_requirement, 50, 0.77).
narrative_ontology:measurement(sove_su_t100, sovereign_legitimacy__monarchical_reading, suppression_requirement, 100, 0.81).
narrative_ontology:measurement(sove_su_t150, sovereign_legitimacy__monarchical_reading, suppression_requirement, 150, 0.83).
narrative_ontology:measurement(sove_su_t200, sovereign_legitimacy__monarchical_reading, suppression_requirement, 200, 0.84).
narrative_ontology:measurement(sove_su_t250, sovereign_legitimacy__monarchical_reading, suppression_requirement, 250, 0.85).
narrative_ontology:measurement(sove_su_t300, sovereign_legitimacy__monarchical_reading, suppression_requirement, 300, 0.86).
narrative_ontology:measurement(sove_su_t350, sovereign_legitimacy__monarchical_reading, suppression_requirement, 350, 0.86).
narrative_ontology:measurement(sove_su_t400, sovereign_legitimacy__monarchical_reading, suppression_requirement, 400, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sovereign_legitimacy__monarchical_reading, 0.12).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The sovereign_legitimacy kernel is contested across three readings: monarchical (this file), republican, and constitutional-hybrid. Each reading instantiates a different constraint with distinct beneficiary/victim structures, persistence mechanisms, and historical trajectories. The readings coexist as live alternatives in political struggle. The monarchical reading dominates early-modern European governance and benefits hereditary nobility through divine-right theology and coercive suppression of alternatives. The republican reading emerges as merchant and educated classes gain power; it threatens the monarchical framework by asserting authority flows upward from popular consent. The constitutional-hybrid reading arises as a compromise formation, preserving ceremonial monarchy while delegating political authority to representative institutions mediated by written law. No single reading forecloses the others logically; rather, institutional dominance and coercive suppression determine which reading governs any given polity at any time. All three stories are linked via network.affects_constraints to indicate they are readings of the same kernel and structurally influence each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
