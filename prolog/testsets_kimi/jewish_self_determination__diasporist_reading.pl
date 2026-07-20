% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__diasporist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__diasporist_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: jewish_self_determination__diasporist_reading
 *   human_readable: Diasporist Jewish Self-Determination (Atrophied Alternative)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This constraint instantiates the diasporist reading of the contested
 *   jewish_self_determination kernel: the claim that Jewish collective
 *   survival and flourishing are best secured through diaspora pluralism and
 *   minority rights rather than territorial sovereignty, and that Zionism
 *   represents a dangerous deviation. The constraint is authored as a piton
 *   because the diasporist framework has atrophied from a once-viable
 *   coordination mechanism into a degraded institutional residue maintained
 *   largely by inertia and theatrical anti-Zionist discourse, while Zionist
 *   territorial sovereignty has become hegemonic. The claim/metric
 *   independence is preserved: the constraint is claimed as piton while
 *   metrics describe moderate extraction, substantial theater, and incomplete
 *   accessibility collapse.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_communities (beneficiary/organized/constrained): receive identity validation from the persistence of diasporist discourse but lack effective political infrastructure
 *   - jews_coerced_into_zionism (payer/moderate/identity_locked): bear the costs of the atrophied alternative, locked into Zionist frameworks without viable non-territorial Jewish political identity at scale
 *   - diaspora_institutional_leadership (agenda_setter/moderate/constrained): administer the piton through institutional inertia, could reorient but identity cost exceeds benefit
 *   - postcolonial_theorists (observer/analytical/analytical): analytical seat observing the kernel contest from outside benefiting parties
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, 0.52).
domain_priors:suppression_score(jewish_self_determination__diasporist_reading, 0.38).
domain_priors:theater_ratio(jewish_self_determination__diasporist_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jewish_self_determination__diasporist_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__diasporist_reading, piton).
narrative_ontology:human_readable(jewish_self_determination__diasporist_reading, "Diasporist Jewish Self-Determination (Atrophied Alternative)").
narrative_ontology:topic_domain(jewish_self_determination__diasporist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__diasporist_reading, '71039584-6c4d-4fa3-8fae-f831e24ec680').
narrative_ontology:cs_kernel_codification('71039584-6c4d-4fa3-8fae-f831e24ec680', distributed).
narrative_ontology:cs_authority_grounding('71039584-6c4d-4fa3-8fae-f831e24ec680', distributed).
narrative_ontology:cs_reading_relation('71039584-6c4d-4fa3-8fae-f831e24ec680', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('71039584-6c4d-4fa3-8fae-f831e24ec680', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('71039584-6c4d-4fa3-8fae-f831e24ec680', jewish_self_determination__settler_colonial_reading, influences).
narrative_ontology:cs_reading_relation('71039584-6c4d-4fa3-8fae-f831e24ec680', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_axiom('71039584-6c4d-4fa3-8fae-f831e24ec680', foundational, diaspora_pluralism_secures_flourishing).
narrative_ontology:cs_axiom_status(diaspora_pluralism_secures_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('71039584-6c4d-4fa3-8fae-f831e24ec680', diaspora_pluralism_secures_flourishing, instrumental).
narrative_ontology:cs_axiom('71039584-6c4d-4fa3-8fae-f831e24ec680', foundational, territorial_sovereignty_is_dangerous_deviation).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_dangerous_deviation, holdable).
narrative_ontology:cs_axiom_grounding('71039584-6c4d-4fa3-8fae-f831e24ec680', territorial_sovereignty_is_dangerous_deviation, instrumental).
narrative_ontology:cs_reference_frame('71039584-6c4d-4fa3-8fae-f831e24ec680', diaspora_pluralism_as_self_determination).
narrative_ontology:cs_drift_state('71039584-6c4d-4fa3-8fae-f831e24ec680', zionist_hegemonic_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('71039584-6c4d-4fa3-8fae-f831e24ec680', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__diasporist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__diasporist_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish communities living outside Israel who maintain distinct religious, cultural, and political identities across multiple host societies. They gain identity validation and political framework from the diasporist reading, which legitimates non-territorial Jewish existence as a valid expression of self-determination. Their exit options are constrained by assimilationist pressures and the atrophy of diasporist political infrastructure.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Jews residing in or politically committed to the Israeli territorial framework, including those conscripted into military service, those whose Jewish identity has become fused with Zionist statehood, and those endangered by antisemitism linked to Israeli state actions. They bear the cost of the diasporist alternative's atrophy, which leaves them without a viable non-territorial Jewish political identity at scale.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, jews_coerced_into_zionism, payer,
    moderate, biographical, identity_locked, national).

% Leaders of diaspora Jewish communal organizations, cultural institutions, and religious movements outside Israel. They administer the remaining diasporist infrastructure and rhetoric out of institutional inertia and historical mission, despite the shift of most Jewish political energy and material resources toward Zionist sovereignty. They could reorient toward Zionism or assimilation but the organizational identity cost exceeds the perceived benefit.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, diaspora_institutional_leadership, agenda_setter,
    moderate, generational, constrained, national).

% Academic and political theorists who analyze Jewish self-determination through postcolonial and minority-rights frameworks. They observe the tension between diasporist and Zionist readings from an analytical seat, often sympathetic to diasporism but without institutional stake in its persistence.
narrative_ontology:constraint_stakeholder(jewish_self_determination__diasporist_reading, postcolonial_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__diasporist_reading, diffuse).
narrative_ontology:fixing_cost_class(jewish_self_determination__diasporist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinated Jewish survival and political agency across dispersed host societies through minority-rights advocacy, cultural institution-building, and transnational solidarity without territorial concentration.
% TRANSFER_FUNCTION: Moves political legitimacy and institutional resources from diaspora-based Jewish self-determination toward Zionist territorial sovereignty, while maintaining symbolic diasporist discourse without effective coordination infrastructure.
% ABSENT_VOICES: Palestinians are structurally peripheral to this reading's primary focus on Jewish political forms; secular Israeli Jews seeking non-Zionist Jewish identity lack institutional voice within Zionist hegemony; host-state majorities with capacity to guarantee minority rights are not organized around Jewish diasporist outcomes.
% DISAPPEARANCE_RATIONALE: If the diasporist framework vanished entirely, Jewish political life would lose its only structured non-territorial alternative, leaving Zionist sovereignty as the sole hegemonic form of Jewish self-determination; diaspora Jewish communities would lose their validating political theology and likely accelerate assimilation or Zionist reorientation.
% FOUNDING_PROBLEM: Jewish survival and flourishing in the absence of territorial sovereignty, facing dispersion, minority status, and periodic persecution across multiple host societies.
% FOUNDING_PROBLEM_CORROBORATION: Zionist institutions assert the founding problem is solved by statehood. Postcolonial theorists and diaspora historians outside Zionist frameworks attest that minority-status vulnerability persists; host-state civil liberties organizations corroborate that diaspora Jewish communities continue to require active minority-rights protection. No party outside the benefiting diaspora communities unambiguously corroborates that diasporism alone remains sufficient.
narrative_ontology:disappearance_verdict(jewish_self_determination__diasporist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__diasporist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__diasporist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__diasporist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__diasporist_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__diasporist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__diasporist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__diasporist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the atrophied framework still extracts political possibility and identity-commitment from those who might otherwise pursue non-Zionist Jewish politics, but its weakness limits the extraction ceiling. Theater_ratio is high (0.78) because the diasporist framework persists primarily through symbolic discourse, academic conferences, and cultural rhetoric rather than live coordination infrastructure. Suppression is moderate-low (0.38): the constraint is not actively enforced by its own machinery; rather, its alternative has been suppressed by external Zionist hegemony. Accessibility_collapse (0.62) reflects that viable non-Zionist Jewish political identities are known but socially and institutionally marginalized. Resistance (0.42) captures moderate pushback from Zionist institutions and mainstream Jewish organizations that treat diasporism as obsolete or threatening.
 *
 * PERSPECTIVAL GAP:
 *   From the diasporist intellectual seat, the constraint is a tragically atrophied but still valid political vision that should be revived; from the Zionist Jew seat, it is an obsolete nostalgia that offers no security and implicitly delegitimizes their existence; from the diaspora community member seat, it is comforting identity validation without effective political infrastructure. The engine computes these divergences from the structural data rather than adjudicating them.
 *
 * DIRECTIONALITY LOGIC:
 *   The diaspora_jewish_communities seat derives low directionality as structural beneficiary of continued diasporist discourse (identity validation, political framework), though the benefit is diffuse and not rent-capture. The jews_coerced_into_zionism seat derives high directionality as structural target: the atrophied framework fails to provide them a viable alternative, leaving them exposed to the costs of Zionist militarization and identity fusion. The diaspora_institutional_leadership sits near symmetric: they administer the constraint but do not capture its extraction, operating out of inertia rather than profit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as piton prevents mislabeling this constraint as a snare (there is no concentrated beneficiary capturing rents from its persistence) or as a rope (it no longer functions as live coordination). The diasporist framework's founding problemâJewish survival without territorial sovereigntyâhas been substantially displaced by Zionist statehood, yet the arrangement persists through institutional inertia and theatrical maintenance, satisfying the mandatrophy-resolved signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zionist_hegemony_vs_assimilation,
    'Is the diasporist framework''s atrophy caused primarily by Zionist institutional suppression, or by secular assimilationist pressures and host-state integration?',
    'Comparative analysis of diaspora Jewish community vitality across host states with varying Zionist institutional penetration and secular integration pressures.',
    'If Zionist suppression is primary, the constraint may be better classified as a snare victim rather than a piton; if assimilation is primary, the atrophy is endogenous and the piton classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zionist_hegemony_vs_assimilation, empirical, 'Driver of diasporist framework atrophy').

omega_variable(
    identity_lock_dynamics,
    'To what extent is Jewish identity fusion with Zionism internalized rather than structurally enforced?',
    'Ethnographic study of Jews who exit Zionist frameworks for diasporist or non-Zionist identities; persistence of Zionist identity commitments after structural exit indicates internalization.',
    'If internalized, effective suppression of diasporist alternatives exceeds the structural measure, and the victim group''s directionality sits nearer full-target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_dynamics, empirical, 'Internalized vs structural suppression of diasporism').

omega_variable(
    diasporist_viability_contingency,
    'Can diaspora pluralism empirically secure Jewish collective survival in contemporary geopolitical conditions, or has the historical context made territorial sovereignty structurally necessary?',
    'Comparative historical analysis of diaspora Jewish community persistence versus destruction across different host-state regimes, weighted by contemporary geopolitical conditions.',
    'If diasporism is viable, the piton classification may indicate premature atrophy from suppression rather than genuine functional obsolescence; if non-viable, the piton is correctly classified as atrophied.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diasporist_viability_contingency, conceptual, 'Empirical viability of diasporist survival strategy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__diasporist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_self_determination__diasporist_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(jewi_tr_t10, jewish_self_determination__diasporist_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(jewi_tr_t20, jewish_self_determination__diasporist_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(jewi_tr_t30, jewish_self_determination__diasporist_reading, theater_ratio, 30, 0.66).
narrative_ontology:measurement(jewi_tr_t40, jewish_self_determination__diasporist_reading, theater_ratio, 40, 0.72).
narrative_ontology:measurement(jewi_tr_t50, jewish_self_determination__diasporist_reading, theater_ratio, 50, 0.78).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_self_determination__diasporist_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(jewi_be_t10, jewish_self_determination__diasporist_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement(jewi_be_t20, jewish_self_determination__diasporist_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(jewi_be_t30, jewish_self_determination__diasporist_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(jewi_be_t40, jewish_self_determination__diasporist_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(jewi_be_t50, jewish_self_determination__diasporist_reading, base_extractiveness, 50, 0.52).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_self_determination__diasporist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__diasporist_reading, jewish_self_determination__religious_covenant_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'jewish_self_determination'. The kernel decomposes into multiple structurally distinct constraints because each reading assigns a different locus of Jewish self-determination (diaspora, territorial state, indigenous return, settler-colonial project, divine covenant) with different beneficiary/victim structures and epsilon values. This reading instantiates the diasporist position as a piton; siblings instantiate liberal nationalist, indigenous return, settler-colonial, and religious covenant readings. Each reading has its own epsilon and its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
