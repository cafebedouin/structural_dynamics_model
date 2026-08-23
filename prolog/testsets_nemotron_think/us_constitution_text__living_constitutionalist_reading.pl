% ============================================================================
% CONSTRAINT STORY: us_constitution_text__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_text__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of US Constitution
 *   domain: legal/constitutional
 *
 * SUMMARY:
 *   The living constitutionalist reading holds that constitutional meaning
 *   evolves with society and that judges must adapt fixed principles to
 *   contemporary circumstances. This reading became dominant after the 1937
 *   constitutional revolution and powered the Warren and Burger Courts'
 *   rights expansions. It claims to solve the coordination problem of
 *   constitutional obsolescence, but from the originalist seat it extracts
 *   democratic legitimacy by empowering unelected judges to override
 *   legislative judgments. The constraint has beneficiaries (rights claimants
 *   in changed social contexts) and victims (claims to fixed meaning as
 *   democratic constraint), requires active enforcement (judicial review),
 *   and shows moderate extraction that peaked in the mid-20th century before
 *   originalist pushback.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__living_constitutionalist_reading, 0.45).
domain_priors:suppression_score(us_constitution_text__living_constitutionalist_reading, 0.25).
domain_priors:theater_ratio(us_constitution_text__living_constitutionalist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_text__living_constitutionalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__living_constitutionalist_reading, "Living Constitutionalist Reading of US Constitution").
narrative_ontology:topic_domain(us_constitution_text__living_constitutionalist_reading, "legal/constitutional").

domain_priors:requires_active_enforcement(us_constitution_text__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__living_constitutionalist_reading, '45883ce9-e125-4d39-8681-2af3c0469d75').
narrative_ontology:cs_kernel_codification('45883ce9-e125-4d39-8681-2af3c0469d75', fixed_text).
narrative_ontology:cs_authority_grounding('45883ce9-e125-4d39-8681-2af3c0469d75', lineage).
narrative_ontology:cs_interpretation_layer_present('45883ce9-e125-4d39-8681-2af3c0469d75').
narrative_ontology:cs_reading_relation('45883ce9-e125-4d39-8681-2af3c0469d75', us_constitution_text__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('45883ce9-e125-4d39-8681-2af3c0469d75', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('45883ce9-e125-4d39-8681-2af3c0469d75', foundational, constitutional_meaning_evolves_with_society).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_society, holdable).
narrative_ontology:cs_axiom_grounding('45883ce9-e125-4d39-8681-2af3c0469d75', constitutional_meaning_evolves_with_society, deontological).
narrative_ontology:cs_axiom('45883ce9-e125-4d39-8681-2af3c0469d75', secondary, judicial_adaptation_serves_democratic_legitimacy).
narrative_ontology:cs_axiom_status(judicial_adaptation_serves_democratic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('45883ce9-e125-4d39-8681-2af3c0469d75', judicial_adaptation_serves_democratic_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('45883ce9-e125-4d39-8681-2af3c0469d75', ratification_era_understanding_as_starting_point).
narrative_ontology:cs_drift_state('45883ce9-e125-4d39-8681-2af3c0469d75', contemporary_rights_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('45883ce9-e125-4d39-8681-2af3c0469d75', '').
narrative_ontology:cs_kernel_id(us_constitution_text__living_constitutionalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, rights_claimants_changed_contexts).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, fixed_meaning_democratic_constraint_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__living_constitutionalist_reading, democratic_majorities_seeking_self_governance).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, fixed_meaning_advocates).
narrative_ontology:constraint_victim(us_constitution_text__living_constitutionalist_reading, democratic_majorities_seeking_self_governance).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, constitutional_adaptability_principle).
narrative_ontology:constraint_vindicates(us_constitution_text__living_constitutionalist_reading, judicial_role_in_rights_expansion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercise interpretive authority to adapt constitutional principles to contemporary circumstances through judicial review. Their decisions establish binding precedent that expands or contracts constitutional protections. They face minimal professional exit costs — the interpretive methodology is mainstream in legal academia and the judiciary.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, living_constitutionalist_judges, agenda_setter,
    institutional, generational, arbitrage, national).

% Individuals and groups seeking constitutional protection for claims unrecognized at ratification (e.g., abortion access, same-sex marriage, gender identity protections). They benefit when courts recognize new rights through evolving interpretation. Their exit is constrained — they cannot practically amend the Constitution and depend on judicial adaptation.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, rights_claimants_changed_contexts, beneficiary,
    moderate, biographical, constrained, national).

% Originalist judges, scholars, and political actors who argue constitutional meaning is fixed at ratification. They bear the cost of living constitutionalist decisions that override their preferred interpretive methodology. Their exit is constrained — they remain subject to binding precedent but can work to appoint originalist judges and shift doctrine over decades.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, fixed_meaning_advocates, payer,
    powerful, generational, constrained, national).

% Citizens and legislatures whose policy preferences are constrained when courts invalidate laws under evolving constitutional standards. They also benefit when courts protect minority rights from majority overreach. Their exit is constrained — they can pursue constitutional amendment but face Article V's high threshold.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, democratic_majorities_seeking_self_governance, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__living_constitutionalist_reading, democratic_majorities_seeking_self_governance, beneficiary).

% Judges committed to originalist methodology who must operate within a doctrinal landscape dominated by living constitutionalist precedent. They cannot adopt the living constitutionalist frame without abandoning their interpretive identity. Their professional identity is fused to the originalist methodology.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, originalist_judges, excluded,
    institutional, generational, identity_locked, national).

% Academic observers who analyze, critique, and theorize both living constitutionalist and originalist methodologies. They neither collect rents nor bear direct costs from judicial decisions, but their professional standing depends on the interpretive debate's continuation.
narrative_ontology:constraint_stakeholder(us_constitution_text__living_constitutionalist_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a method for constitutional meaning to adapt to social change without requiring formal amendment under Article V, allowing the Constitution to govern unforeseen circumstances.
% TRANSFER_FUNCTION: Moves interpretive authority from fixed historical meaning to contemporary judicial judgment, transferring power from ratification-era understandings to present-day rights claimants and the judges who recognize their claims.
% ABSENT_VOICES: Future generations whose rights claims do not yet exist and cannot participate in current interpretive debates; citizens who prefer democratic amendment over judicial adaptation but face Article V's near-insurmountable threshold; state legislatures whose policy autonomy is constrained by judicially recognized unenumerated rights.
% DISAPPEARANCE_RATIONALE: If living constitutionalism vanished overnight, the entire structure of modern substantive due process, equal protection, and unenumerated rights doctrine (Roe/Casey/Dobbs lineage, Obergefell, Lawrence, Griswold) would collapse or revert to originalist footings, dramatically reshaping the constitutional rights landscape and the balance of power between courts and legislatures.
% FOUNDING_PROBLEM: The Constitution's fixed text cannot anticipate future social conditions, technological changes, or evolving moral understandings; a mechanism is needed to keep it relevant and legitimate across centuries without constant formal amendment.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists and historians outside the beneficiary set (e.g., Bruce Ackerman's 'constitutional moments,' Jack Balkin's 'living originalism') attest the founding problem of constitutional obsolescence is real; originalist scholars (e.g., Scalia, Barrett, Whittington) contest it, arguing the amendment process solves the problem democratically.
narrative_ontology:disappearance_verdict(us_constitution_text__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__living_constitutionalist_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_text__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects that the living constitutionalist reading transfers interpretive authority from fixed text to judicial judgment, benefiting rights claimants while imposing costs on fixed-meaning advocates. The peak in the 1960s-70s (0.48) corresponds to the height of substantive due process expansion. Theater ratio (0.15) is low — the methodology is genuinely used, not performative — though it rose during periods of intense originalist critique. Suppression (0.25) is low for adaptive interpretation but higher for originalist methodology, which faces professional and institutional pressure. Accessibility collapse (0.4) is moderate — originalism remains a live, well-resourced alternative. Resistance (0.55) is significant — the originalist counter-movement has captured the federal judiciary at the highest level.
 *
 * PERSPECTIVAL GAP:
 *   From the living constitutionalist judge's seat, the constraint is a Rope — genuine coordination solving constitutional obsolescence. From the originalist judge's seat, it is a Snare — pure extraction of democratic legitimacy. From rights claimants' seat, it is a Scaffold — transitional support until democratic recognition catches up (though the transition has lasted 80+ years). The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the analytical view that both coordination and asymmetric extraction are structurally present.
 *
 * DIRECTIONALITY LOGIC:
 *   Living constitutionalist judges are structural beneficiaries (d ~ 0.15) — they gain interpretive authority and institutional prestige. Rights claimants in changed contexts are beneficiaries (d ~ 0.2) — they gain constitutional protections unavailable under originalism. Fixed-meaning advocates are targets (d ~ 0.85) — the constraint directly displaces their interpretive methodology. Democratic majorities are near-symmetric (d ~ 0.5) — constrained when courts strike down laws, benefited when courts protect minority rights. Originalist judges are identity-locked targets (d ~ 0.9) — their professional identity is constituted by opposition to this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (constitutional obsolescence) remains contested — living constitutionalists say it's live, originalists say Article V solves it. The constraint has outlived its original justification if one accepts the originalist view, but living constitutionalists deny mandatrophy. The mandatrophy question is exactly the contested classification: is this still solving a coordination problem, or has it become extraction of judicial power? The framework captures this by computing different types for different seats rather than forcing a single verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_ambiguity,
    'Is the living constitutionalist methodology a genuine coordination mechanism for constitutional adaptation, or an extraction mechanism that transfers power from democratic processes to courts?',
    'Compare outcomes in living constitutionalist vs. originalist regimes on measures of democratic responsiveness, rights protection stability, and constitutional legitimacy over time. Cross-national comparison of constitutional courts with different interpretive mandates.',
    'If coordination, the constraint is a Rope/Tangled Rope with legitimate function. If extraction, it is a Snare masquerading as adaptation. The classification determines whether the constraint''s persistence is justified or requires reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ambiguity, conceptual, 'Whether the constraint''s core function is coordination or extraction — the central dispute between living constitutionalists and originalists.').

omega_variable(
    contemporary_circumstances_indeterminacy,
    'Does ''contemporary circumstances'' provide a determinate standard for judicial decision-making, or does it function as an open gate for judicial preference?',
    'Analyze the variance in living constitutionalist outcomes across judges facing similar social conditions. Test whether the methodology constrains judges more than their policy preferences would predict.',
    'If indeterminate, the constraint''s low suppression metric is misleading — suppression operates through professional socialization rather than textual constraint. If determinate, the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_circumstances_indeterminacy, empirical, 'Whether the adaptive standard is constraining or permissive.').

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s classification change when analyzed as one reading of a contested kernel versus a standalone constraint?',
    'Compare the engine''s per-seat classifications for this reading alone versus the full kernel family (living constitutionalist, originalist, positivist). The kernel frame may reveal structural dependencies invisible in isolation.',
    'If the reading''s type shifts under kernel analysis, the committer frame is analytically necessary. If stable, the kernel frame is interpretive overlay.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Commitment-system framing of this constraint as one reading of the us_constitution_text kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__living_constitutionalist_reading, 0, 87).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__living_constitutionalist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(us_c_tr_t15, us_constitution_text__living_constitutionalist_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_text__living_constitutionalist_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(us_c_tr_t45, us_constitution_text__living_constitutionalist_reading, theater_ratio, 45, 0.18).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_text__living_constitutionalist_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(us_c_tr_t75, us_constitution_text__living_constitutionalist_reading, theater_ratio, 75, 0.15).
narrative_ontology:measurement(us_c_tr_t87, us_constitution_text__living_constitutionalist_reading, theater_ratio, 87, 0.15).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(us_c_be_t15, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(us_c_be_t30, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(us_c_be_t45, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 45, 0.48).
narrative_ontology:measurement(us_c_be_t60, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(us_c_be_t75, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement(us_c_be_t87, us_constitution_text__living_constitutionalist_reading, base_extractiveness, 87, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(us_c_su_t15, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(us_c_su_t30, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(us_c_su_t45, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 45, 0.28).
narrative_ontology:measurement(us_c_su_t60, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(us_c_su_t75, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 75, 0.25).
narrative_ontology:measurement(us_c_su_t87, us_constitution_text__living_constitutionalist_reading, suppression_requirement, 87, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__living_constitutionalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_text__living_constitutionalist_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint and its siblings form the us_constitution_text constraint family. Each reading instantiates a different constraint from the same kernel: living constitutionalist (adaptive, moderate extraction), originalist (fixed, low extraction from living constitutionalist view but high from its own), positivist (procedural, minimal extraction). The living constitutionalist reading's coordination function (adapting text to circumstance) structurally influences the originalist reading's defensive posture and the positivist reading's procedural focus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__living_constitutionalist_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
