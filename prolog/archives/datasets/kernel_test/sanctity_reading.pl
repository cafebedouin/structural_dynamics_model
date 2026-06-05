% ============================================================================
% CONSTRAINT STORY: sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sanctity_reading, []).

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
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sanctity_reading
 *   human_readable: Sanctity of Life Reading: End-of-Life Decision Authority
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity-of-life reading grounds end-of-life decision authority in
 *   the claim that human life possesses intrinsic value independent of
 *   individual will or choice. Intentional life-ending, under this reading,
 *   violates that value regardless of the individual's suffering,
 *   preferences, or circumstances. This constraint instantiates one
 *   authoritative reading of a contested kernel about which framework should
 *   govern end-of-life decisions. The kernel (end_of_life_decision_authority)
 *   admits three competing readings: the sanctity reading (this one), the
 *   autonomy reading (individual choice is paramount), and the
 *   vulnerability-protection reading (protection of pressured agents is
 *   paramount). These readings coexist in contemporary bioethics and medical
 *   law, producing different institutional outcomes across jurisdictions. The
 *   sanctity reading is dominant in many religious medical institutions and
 *   in jurisdictions where physician-assisted death remains prohibited. The
 *   constraint exhibits all six types from different perspectives: it appears
 *   as a snare to powerless patients whose suffering is made metaphysically
 *   meaningful by the sanctity doctrine; as coordination (rope) to religious
 *   institutions that use the principle to organize medical practice and
 *   preserve physician authority; as a degraded principle (piton) in medical
 *   ethics systems that profess sanctity while practicing de facto
 *   autonomy-respect through palliative care discussions; as a temporary
 *   problem being solved by legal change (would be scaffold from right-to-die
 *   advocates); and as a natural law (mountain) from the perspective that
 *   intrinsic value is metaphysical rather than constructed. The theater
 *   ratio (0.65) reflects growing distance between formal commitment to
 *   sanctity and actual clinical practice that respects patient autonomy
 *   through advance directives, do-not-resuscitate orders, and palliative
 *   care redirection.
 *
 * KEY AGENTS:
 *   - Pressured-Vulnerable Patient: Primary victim (powerless/trapped) — bears cost of denied exit; suffering is metaphysically reframed as meaningful
 *   - Religious Medical Institution: Primary beneficiary (institutional/arbitrage) — preserves authority over end-of-life decisions; maintains doctrinal coherence
 *   - Physician: Beneficiary in sanctity frame (institutional/arbitrage) — authority derives from role as life-preserver, not patient autonomy facilitator
 *   - Family Member: Secondary victim/constrained participant (moderate/constrained) — provides unpaid care labor; normalizes suffering as redemptive
 *   - Right-to-Die Advocates: Organized opposition (organized/constrained) — benefit from mobilization against the constraint; high cost to directly challenge within medical institutions
 *   - Traditional Medical Ethics System: Institutional maintainer (institutional/arbitrage) — perpetuates principle through ritual even as practice has degraded
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional commitment as metaphysical fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sanctity_reading, 0.58).
domain_priors:suppression_score(sanctity_reading, 0.72).
domain_priors:theater_ratio(sanctity_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sanctity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sanctity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sanctity_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sanctity_reading, tangled_rope).
narrative_ontology:human_readable(sanctity_reading, "Sanctity of Life Reading: End-of-Life Decision Authority").
narrative_ontology:topic_domain(sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sanctity_reading, 'ae053295-daf9-43b5-81e7-9532ff5bf7de').
narrative_ontology:cs_created_at('ae053295-daf9-43b5-81e7-9532ff5bf7de', '').
narrative_ontology:cs_kernel_codification('ae053295-daf9-43b5-81e7-9532ff5bf7de', formalized).
narrative_ontology:cs_authority_grounding('ae053295-daf9-43b5-81e7-9532ff5bf7de', lineage).
narrative_ontology:cs_interpretation_layer_present('ae053295-daf9-43b5-81e7-9532ff5bf7de').
narrative_ontology:cs_kernel_id(sanctity_reading, end_of_life_decision_authority).
narrative_ontology:cs_reading_relation('ae053295-daf9-43b5-81e7-9532ff5bf7de', autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae053295-daf9-43b5-81e7-9532ff5bf7de', vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('ae053295-daf9-43b5-81e7-9532ff5bf7de', foundational, human_life_intrinsic_value).
narrative_ontology:cs_axiom_status(human_life_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('ae053295-daf9-43b5-81e7-9532ff5bf7de', human_life_intrinsic_value, deontological).
narrative_ontology:cs_axiom('ae053295-daf9-43b5-81e7-9532ff5bf7de', foundational, physician_role_life_preservation).
narrative_ontology:cs_axiom_status(physician_role_life_preservation, holdable).
narrative_ontology:cs_axiom_grounding('ae053295-daf9-43b5-81e7-9532ff5bf7de', physician_role_life_preservation, deontological).
narrative_ontology:cs_reference_frame('ae053295-daf9-43b5-81e7-9532ff5bf7de', sacred_personhood_framework).
narrative_ontology:cs_drift_state('ae053295-daf9-43b5-81e7-9532ff5bf7de', contemporary_autonomy_era, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sanctity_reading, religious_medical_institutions).
narrative_ontology:constraint_beneficiary(sanctity_reading, physician_authority_preservationists).
narrative_ontology:constraint_victim(sanctity_reading, pressured_vulnerable_patients).
narrative_ontology:constraint_victim(sanctity_reading, autonomy_constrained_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRESSURED-VULNERABLE PATIENT (SNARE) — Under the sanctity reading, patient exit is categorically denied. Euthanasia/physician-assisted death is framed as violation of intrinsic value, not as option. Patient bears full cost: must endure suffering deemed 'sacred' or 'meaningful'; cannot exit through choice. No alternatives available. Suppression enforced through moral framework that redefines refusal to end life as dignified, regardless of actual suffering. Maximum extraction — the constraint traps suffering into the victim's body and soul.
constraint_indexing:constraint_classification(sanctity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILY MEMBER (TANGLED ROPE) — Constrained by both the sanctity framework (moral obligation to support patient's 'meaningful suffering') and by resource barriers (palliative care costs, time to provide support). Genuinely benefits from coordination function: the framework provides moral language to process grief, ritual meaning for dying, and community support structure. But also bears extraction: expected to provide unpaid care labor, to normalize suffering as redemptive, to suppress their own desires to end the patient's pain. Mixed experience — some benefit, significant cost.
constraint_indexing:constraint_classification(sanctity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS MEDICAL INSTITUTION (ROPE) — Primary beneficiary. The sanctity reading preserves institutional authority over end-of-life decisions, maintains coherence with theological doctrine, and provides moral justification for resource allocation (investing in 'life-preserving' care rather than palliative comfort). Experiences the constraint as coordination: it organizes medical practice around a coherent ethical framework. Experiences extraction running toward this agent — institutional legitimacy is amplified by the sanctity doctrine.
constraint_indexing:constraint_classification(sanctity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHYSICIAN AUTHORITY PRESERVER (ROPE) — Institutional beneficiary. The sanctity reading positions the physician as guardian of intrinsic value, not as agent of patient choice. Physician authority derives from medical expertise in preserving life, not from respecting autonomy. This preserves physician control over end-of-life decisions and exempts physicians from the patient-autonomy constraint that would otherwise require them to facilitate death. Experiences extraction flowing toward institutional physician authority.
constraint_indexing:constraint_classification(sanctity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: AUTONOMY-FOCUSED PATIENT ADVOCATES (TANGLED ROPE) — Organized agents (right-to-die organizations, disability rights coalitions) see the sanctity reading as enforced constraint that limits patient choice. But they also benefit from the constraint through coalition-building: the sanctity reading's prohibition creates visible harm that motivates organization and generates legal/political pressure for alternatives. Constrained exit: high cost to directly challenge the framework within medical institutions that hold it, but clear causal pathway to change through law/policy. Mixed experience.
constraint_indexing:constraint_classification(sanctity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL MEDICAL ETHICS SYSTEM (PITON) — The sanctity reading persists through institutional inertia and ritual maintenance. Medical schools and professional codes still invoke 'sanctity of life' as core principle, but the actual professional practice has degraded in function: physicians now discuss palliative care, advance directives, and quality-of-life limitations with patients, functionally accepting that sanctity is not absolute. The principle is maintained through performative commitment — emphasized in codes and oaths but not fully operative in practice. Theater ratio 0.65 captures this: significant gap between professed principle and actual professional behavior.
constraint_indexing:constraint_classification(sanctity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the sanctity reading claims that intrinsic value of human life is a metaphysical fact, not a constructed institutional claim. This perspective sees the sanctity principle as emerging naturally from the structure of personhood itself — an irreducible, unchangeable limit that no individual will or choice can override. However, the structural data contradicts the mountain classification: identifiable beneficiaries exist (religious institutions, physician authority), and the constraint requires active enforcement through institutional power. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(sanctity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sanctity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sanctity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sanctity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sanctity_reading, TR),
    TR >= 0.70.

:- end_tests(sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The sanctity reading extracts from pressured-vulnerable patients by denying them exit options while reframing their suffering as metaphysically meaningful. However, it is not as severe as pure extraction (snare-level ≥0.66) because some patients may genuinely find the spiritual framing coherent with their own values, and family/community support benefits exist. The extractiveness reflects the constraint's primary effect: it forces continued suffering on those who would otherwise choose death, while benefiting religious institutions and preserving physician authority. Suppression (0.72): High. Multiple suppression mechanisms: (1) institutional power — sanctity doctrine is embedded in medical codes, religious institution authority, and legal prohibitions on euthanasia; (2) cognitive reframing — suffering is redefined as meaningful rather than tragic, limiting alternatives perception; (3) relational pressure — family and community reinforce the norm; (4) resource barriers — palliative care investment is often inadequate, making death seem like the only exit when pain becomes unbearable. Suppression is high but not total (not 0.85+) because some jurisdictions have moved to legalize euthanasia/assisted death, and some patients/families successfully resist the norm through legal challenge or covert action. Theater ratio (0.65): Moderate-high. The gap between formal institutional commitment to sanctity and actual clinical practice has increased over the measurement interval. Medical schools and professional codes still invoke sanctity as core principle, but practicing physicians increasingly discuss advance directives, quality-of-life limitations, and palliative redirection with patients — functionally accepting that sanctity is not absolute. The theater increased over 40 years as legal and social pressure mounted and patients gained more voice in decision-making, yet institutional code-level commitment to the principle persisted. This is a classic piton signature: principle maintained through ritual despite degraded function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival gap. The religious institution experiences the sanctity principle as coordination (rope) — organizing medical practice around coherent doctrine. The pressured patient experiences it as pure extraction (snare) — denied exit while suffering is metaphysically reframed. The physician experiences it as authority-preserving coordination (rope) — allowing them to act as life-preservers rather than death-facilitators. The right-to-die advocates experience it as a constraint they are organized to overcome (tangled rope) — it both constrains them and motivates their coalition. The medical ethics system experiences it as a degraded principle (piton) — maintained through ritual despite functional decline. The civilizational analytical observer risks seeing it as natural law (mountain) — intrinsic value as metaphysical fact — but the structural data reveals it as a false summit: identifiable beneficiaries exist, active enforcement is required, and the principle's institutional grip depends on specific theological commitments, not metaphysical necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from the agent's structural position relative to extraction flow. The pressured-vulnerable patient (powerless + trapped) experiences maximum directionality toward extraction: d ≈ 0.95, yielding f(d) ≈ 1.42 times the base extractiveness. The religious institution (institutional + arbitrage exit) experiences extraction flowing toward them: d ≈ 0.05, yielding f(d) ≈ -0.12, making their effective extraction negative (they receive benefit, not burden). The moderately-powered family member (moderate + constrained) sits at d ≈ 0.65, experiencing meaningful but not maximal extraction. The organized advocates (organized + constrained) have d ≈ 0.40, giving them some structural leverage that prevents classification as pure snare. The analytical observer (analytical + analytical) derives canonical d ≈ 0.73, recognizing the constraint from outside any of the institutional positions. The directionality overrides are not needed — the structural derivation captures the real asymmetries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy not by showing that one type is correct but by showing that the sanctity reading itself is one legitimate reading among a contested kernel. The mandatrophy question becomes: 'Which reading of end-of-life authority should govern medical practice?' rather than 'Which type is the correct classification?' The sanctity reading produces tangled rope (mixed coordination and extraction) at the institutional level because it does provide genuine coordination (moral language, ritual meaning, community support) while also extracting from pressured patients and constraining physician judgment. The false summit risk is real: the analytical observer may naturalize what is actually a contingent institutional commitment. The resolution is to recognize that no single reading foreclosed the others — the three readings coexist as live options held by different institutional actors and jurisdictions. The extract from the sanctity reading is the pressure it places on vulnerable patients to accept suffering; this is visible and measurable. The coordination benefit is real for institutions and communities that cohere around the doctrine. The constraint is neither pure mountain (not natural law) nor pure rope (real extraction happens) nor pure snare (genuine coordination exists). Tangled rope is the correct classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intrinsic_vs_constructed_value,
    'Is intrinsic value of human life a metaphysical fact (natural law) or a constructed institutional norm grounded in specific religious/philosophical traditions?',
    'Comparative analysis across cultural frameworks: does sanctity doctrine appear universally or only in specific theological traditions? Can the norm be derived from secular premises or does it require theological grounding? Historical emergence data: when did this principle become institutionalized in medical ethics?',
    'If metaphysical fact: mountain classification is correct, beneficiaries are cosmically irrelevant. If constructed: sanctity reading is contingent institutional arrangement, false summit classification applies, beneficiaries are causally real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intrinsic_vs_constructed_value, conceptual, 'Whether intrinsic value is metaphysical or constructed institutional norm').

omega_variable(
    physician_role_definition_contest,
    'What is the primary role of the physician: healer/life-preserver (sanctity reading) or patient autonomy facilitator (autonomy reading)?',
    'Historical analysis of medical oaths and ethical codes; ethnographic study of how physicians actually allocate attention and time when facing end-of-life decisions; comparison with patient preferences in actual clinical encounters.',
    'If healer-only: sanctity reading''s constraint on physician decision-making is legitimate authority boundary. If facilitator: sanctity reading is extracting from both patient (denied choice) and physician (denied their own judgment). Classification would shift toward snare from physician perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physician_role_definition_contest, empirical, 'Primary institutional role of the physician').

omega_variable(
    suffering_externalization_mechanism,
    'Does the sanctity reading''s prohibition on euthanasia actually reduce suffering or externalize it by making individual suffering metaphysically meaningful?',
    'Comparative outcome data: palliative care availability and quality in sanctity-dominant vs autonomy-friendly jurisdictions; patient quality-of-life measures; prevalence of covert hastened death practices (as proxy for unmet suffering-relief demand).',
    'If reduces suffering: the constraint''s suppression of autonomy is traded for genuine alleviation. If externalizes suffering: the constraint imposes suffering on individuals while claiming redemptive framing. Changes victim group definition and severity assessment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suffering_externalization_mechanism, empirical, 'Whether sanctity framework reduces or externalizes suffering').

omega_variable(
    kernel_reading_contest,
    'Which reading of the end-of-life decision authority kernel is operative: sanctity, autonomy, or vulnerability-protection?',
    'This omega documents the contest itself. The three readings coexist in public discourse and institutional practice. No single reading forecloses the others — different jurisdictions and institutions hold different commitments. The question is which authority structure is gaining institutional force over time.',
    'This is the master omega for the committer frame. The other omegas (intrinsic value, physician role, suffering externalization) are resolutions that would shift dominance between readings. They are not independent questions but jointly determine which reading''s legitimacy framework is empirically grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of end-of-life authority kernel is operative').

omega_variable(
    pressured_vulnerable_identification,
    'Which patients should be classified as pressured-vulnerable in the victim set: those with inadequate palliative care? those with unsupported family dynamics? those with both? those lacking economic alternatives to dying?',
    'Empirical definition of vulnerability in medical ethics literature; comparative study of which patient characteristics predict high regret after euthanasia/assisted death in permissive jurisdictions; analysis of coercion indicators.',
    'Narrow vulnerability definition: fewer victims in the snare perspective. Broad definition: many victims, higher perceived extraction. Changes the severity of suppression measurement and the credibility of the powerless agent''s snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pressured_vulnerable_identification, empirical, 'Criteria for classifying patients as pressured-vulnerable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sanc_tr_t0, sanctity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sanc_tr_t20, sanctity_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(sanc_tr_t40, sanctity_reading, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(sanc_be_t0, sanctity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(sanc_be_t20, sanctity_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(sanc_be_t40, sanctity_reading, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sanctity_reading, identity_coordination).
narrative_ontology:affects_constraint(sanctity_reading, autonomy_reading).
narrative_ontology:affects_constraint(sanctity_reading, vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% The end-of-life decision authority kernel admits three structurally distinct constraint stories: sanctity_reading (this file, ε=0.58, Tangled Rope), autonomy_reading (ε varies, Scaffold or Rope from autonomy perspective), and vulnerability_protection_reading (ε varies, emphasizes safeguards). All three are readings of the same kernel but have different epsilon values reflecting different empirical claims about what prevents harm. Network links establish that sanctity_reading and autonomy_reading are mutual influences: each creates structural pressure on the other through law/policy competition. Vulnerability_protection_reading influences both by providing middle-ground institutional framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
