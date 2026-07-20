% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist Millet Marriage Authority
 *   domain: legal/political/social
 *
 * SUMMARY:
 *   In polities with deep religious pluralism, marriage and family law
 *   authority is deliberately fragmented across recognized religious
 *   communities rather than centralized under a uniform civil code. This
 *   reading treats the fragmentation not as a transitional anomaly or a
 *   concession to tradition, but as a consociational constitutional mechanism
 *   designed to prevent majoritarian domination and secure minority
 *   allegiance to the state. The constraint is a low-extraction coordination
 *   rope: minority communities retain normative autonomy, the majority
 *   forgoes legislative imposition, and the central state enforces the
 *   boundary. The expected structural delta relative to the communal-autonomy
 *   reading is an elite-bargain framingâauthority is recognized because it
 *   stabilizes the polity, not because it authentically expresses
 *   pre-political community will. Legislative paralysis on personal-law
 *   reform is a stability feature, not a democratic bug.
 *
 * KEY AGENTS:
 *   - Minority religious communities: Primary beneficiaries (organized/identity_locked) â receive protective autonomy over marriage law.
 *   - Majority community: Secondary beneficiaries (powerful/constrained) â forgo legislative dominance in exchange for stability and minority buy-in.
 *   - Central state: Agenda-setter (institutional/constrained) â administers recognition of personal law systems and enforces the anti-majoritarian boundary.
 *   - Minority women: Excluded (powerless/trapped) â subject to personal law but absent from the elite bargain; reform efforts blocked by consociational paralysis.
 *   - Uniform civil code advocates: Excluded (moderate/constrained) â blocked from legislative reform by the consociational geometry.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.23).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.25).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.23).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist Millet Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/political/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '3652ae26-d71c-4e79-ac0c-727473e8db2e').
narrative_ontology:cs_kernel_codification('3652ae26-d71c-4e79-ac0c-727473e8db2e', formalized).
narrative_ontology:cs_authority_grounding('3652ae26-d71c-4e79-ac0c-727473e8db2e', lineage).
narrative_ontology:cs_interpretation_layer_present('3652ae26-d71c-4e79-ac0c-727473e8db2e').
narrative_ontology:cs_reading_relation('3652ae26-d71c-4e79-ac0c-727473e8db2e', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('3652ae26-d71c-4e79-ac0c-727473e8db2e', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('3652ae26-d71c-4e79-ac0c-727473e8db2e', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('3652ae26-d71c-4e79-ac0c-727473e8db2e', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('3652ae26-d71c-4e79-ac0c-727473e8db2e', foundational, fragmentation_as_anti_tyranny_mandate).
narrative_ontology:cs_axiom_status(fragmentation_as_anti_tyranny_mandate, holdable).
narrative_ontology:cs_axiom_grounding('3652ae26-d71c-4e79-ac0c-727473e8db2e', fragmentation_as_anti_tyranny_mandate, instrumental).
narrative_ontology:cs_axiom('3652ae26-d71c-4e79-ac0c-727473e8db2e', foundational, legislative_paralysis_as_stability_feature).
narrative_ontology:cs_axiom_status(legislative_paralysis_as_stability_feature, holdable).
narrative_ontology:cs_axiom_grounding('3652ae26-d71c-4e79-ac0c-727473e8db2e', legislative_paralysis_as_stability_feature, conventional).
narrative_ontology:cs_reference_frame('3652ae26-d71c-4e79-ac0c-727473e8db2e', consociational_elite_bargain).
narrative_ontology:cs_drift_state('3652ae26-d71c-4e79-ac0c-727473e8db2e', contemporary_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3652ae26-d71c-4e79-ac0c-727473e8db2e', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, majority_community).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociational_democracy_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Govern marriage and family relations under their own recognized personal law codes, insulated from majoritarian legislation by constitutional or political design. Their communal legal identity is protected by the state; exit from the communal system entails loss of both legal recognition and social standing, making the arrangement identity-locked.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_religious_communities, beneficiary,
    organized, generational, identity_locked, national).

% Retains demographic and electoral dominance but is structurally prevented from legislating a uniform marriage code. Bears the opportunity cost of foregone cultural uniformity, yet receives minority allegiance and democratic stability in exchange for restraint.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majority_community, beneficiary,
    powerful, generational, constrained, national).

% Administers the recognition of distinct personal law systems and enforces the constitutional or political barrier to majoritarian override of marriage authority. Cannot unilaterally abolish the pluralist framework without risking minority withdrawal of consent and systemic instability.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, central_state, agenda_setter,
    institutional, generational, constrained, national).

% Subject to the personal law of their community but largely excluded from the elite consociational bargain that maintains it. Reform efforts they support are regularly blocked by the legislative paralysis that stabilizes the inter-communal pact.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_women, excluded,
    powerless, biographical, trapped, national).

% Advocate for a single civil code to replace fragmented personal laws. Their legislative path is blocked by the consociational design, and they are treated as destabilizing forces in the elite bargain despite having democratic support in principle.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, uniform_civil_code_advocates, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents majoritarian domination of marriage law in deeply divided plural societies by fragmenting authority across recognized religious communities, thereby securing minority allegiance to the state and avoiding zero-sum cultural conflict.
% TRANSFER_FUNCTION: Moves legislative authority over marriage and family law from a centralized majoritarian legislature to decentralized community-level personal law systems; transfers stability and minority consent to the polity in exchange for majority restraint.
% ABSENT_VOICES: Women within minority communities seeking gender-egalitarian reforms, and secularist advocates of a uniform civil code, are structurally underrepresented in the consociational bargain because their demands would destabilize the elite pact that the arrangement protects.
% DISAPPEARANCE_RATIONALE: If the fragmented authority vanished overnight and a uniform majoritarian code were imposed, minority communities would face cultural imposition, the consociational elite bargain would collapse, and the polity would likely face sectarian polarization or minority withdrawal of consent.
% FOUNDING_PROBLEM: How to maintain a multi-religious state's cohesion when no single community holds a consensus on family law, and majoritarian rule would alienate minorities into non-integration or secession.
% FOUNDING_PROBLEM_CORROBORATION: Political scientists of consociationalism and comparative constitutional lawyers attest the problem from outside the benefiting communities; minority community leaders corroborate the ongoing need for protection from majoritarian override.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.23, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.23) because the constraint redistributes authority rather than material wealth; its primary effect is to block majoritarian legislation, not to extract rents. Suppression is low (0.25) because persistence depends on constitutional inertia and elite consensus rather than active coercion against alternativesâthe uniform civil code remains speakable but is politically blocked by the consociational geometry. Theater ratio is low (0.14) because the coordination is largely functional: personal law courts and community authorities genuinely administer marriage. Accessibility collapse is moderate (0.35) because a uniform code is a visible alternative that is kept off the legislative agenda by the structure of the bargain, not by informational obscurity. Resistance is low (0.25) because the arrangement delivers stability and minority buy-in, though gender-equality and secularist advocates mount limited resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the minority-community seat, the constraint is protective autonomy (rope-like, even mountain-like in its stability). From the excluded gender-rights or secularist seat, the same structure reads as an entrenched barrier to reform. The engine computes this divergence from the same structural data: the excluded seats lack beneficiary status and have high fallback directionality due to powerless/trapped and moderate/constrained positions, while the beneficiary seats have low directionality and Îµ is low enough that the seat classification stays rope for beneficiaries but may drift toward piton or tangled rope for excluded seats if theater or suppression were higher.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority religious communities and the majority community are both declared beneficiaries: the former receives normative protection, the latter receives stability. Their directionality is on the beneficiary side, damping any effective extraction. The central state is the agenda-setter and administrator with constrained exit; it is not a beneficiary but also not a victim, so it reverts to the institutional power fallback (moderate d). Excluded agents (minority women, UCC advocates) are not declared victims in base properties because the constraint does not actively extract from them; however, their powerless/trapped and moderate/constrained positions place them on the target side of the fallback distribution, meaning they experience the constraint's blocking effect most acutely despite the low base extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy risk here is misclassifying the constraint as a snare because it blocks reform. But the blocking is not the extraction of value for a concentrated capturer; it is the side effect of a coordination arrangement that prevents a worse collective outcome (majoritarian imposition leading to minority secession or conflict). The absence of a concentrated beneficiary capturing rent, the low theater ratio, and the genuine coordination function (peaceful coexistence of deeply divided communities) prevent the snare classification. If the elite bargain were to atrophy into pure inertia with no live coordination function, it would become a piton; if reform-blockade became the primary function serving a specific group's interest, it would become a tangled rope. Neither is the current structural reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elite_bargain_vs_authentic_autonomy,
    'Does the federalist reading describe an elite bargain imposed for stability, or does it authentically instantiate communal self-determination?',
    'Historical analysis of constitutional founding debates and contemporary community-level preference alignment with personal law norms.',
    'If elite-imposed, the coordination may conceal extraction by elites (tangled_rope); if authentic autonomy, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_bargain_vs_authentic_autonomy, conceptual, 'Elite bargain versus authentic autonomy framing ambiguity.').

omega_variable(
    paralysis_reform_exclusion,
    'Does legislative paralysis on personal law reform structurally exclude gender-equality advocates to the point of converting the rope into a tangled rope?',
    'Track reform bill history and judicial intervention rates; measure whether excluded groups face escalating barriers.',
    'If exclusion is systematic and benefits communal patriarchal elites, the constraint drifts toward tangled_rope; if incidental, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paralysis_reform_exclusion, empirical, 'Reform exclusion as extraction mechanism.').

omega_variable(
    kernel_reading_stability,
    'Is the federalist consociational reading stable against the secularist and gender-rights sibling readings, or is it a transitional scaffold?',
    'Observe constitutional amendment pressures and judicial drift toward harmonization.',
    'If the reading is transitional, reclassify as scaffold; if stable, it remains rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Stability of the federalist reading against competing kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__federalist_millet_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__federalist_millet_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__federalist_millet_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__federalist_millet_reading, theater_ratio, 50, 0.14).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(marr_be_t10, marriage_authority__federalist_millet_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(marr_be_t20, marriage_authority__federalist_millet_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(marr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(marr_be_t40, marriage_authority__federalist_millet_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(marr_be_t50, marriage_authority__federalist_millet_reading, base_extractiveness, 50, 0.23).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority__federalist_millet_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority kernel. The federalist_millet_reading decomposes from the same natural-language label as communal_autonomy_reading, secularist_reading, gender_rights_reading, and judicial_harmonization_reading. Each reading has a distinct epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
