% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__settler_colonial_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jewish_self_determination__settler_colonial_reading
 *   human_readable: Zionist Settlement and Palestinian Displacement (Settler-Colonial Reading)
 *   domain: political_philosophy/postcolonial_theory/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the settler-colonial reading of the
 *   kernel 'jewish_self_determination.' Under this reading, Zionism is
 *   analyzed as a European settler-colonial project that systematically
 *   dispossessed indigenous Palestinians through legal land acquisition,
 *   military expulsion, and ongoing occupation enforced by asymmetric law
 *   (most notably the Law of Return, granting automatic citizenship to Jewish
 *   immigrants while denying Palestinian refugees return rights). The
 *   constraint is structured as a snare: it extracts land, property,
 *   political authority, and freedom of movement from Palestinians to benefit
 *   European Jewish settlers and the Israeli state. The extraction persists
 *   through active enforcement (military occupation, settlement expansion,
 *   permit systems, differential legal status) and through suppression of
 *   Palestinian exit options (displacement means statelessness, remaining
 *   means subordination). The constraint's theater ratio is
 *   moderate—humanitarian and security narratives frame the project, but the
 *   core function is territorial acquisition and indigenous elimination.
 *
 * KEY AGENTS:
 *   - european_jewish_settlers: acquire land, consolidate institutional control, establish arbitrage exit (European citizenship + Palestinian foothold)
 *   - israeli_state: formalizes settlement control through law (Law of Return), military force, and administrative authority; enforces differential legal status and expansion
 *   - palestinian_arabs: experience dispossession through land loss, military expulsion, legal exclusion, and ongoing occupation; trapped with no return option
 *   - palestinian_refugees: millions displaced and denied return; live in permanent legal limbo in camps or neighboring countries
 *   - british_mandate_authority: facilitates settler land acquisition and institution-building while nominally protecting indigenous rights; withdraws 1948
 *   - postcolonial_scholars: apply settler-colonial theory frameworks to analyze structural parallels with North America, Australia, South Africa
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, 0.88).
domain_priors:suppression_score(jewish_self_determination__settler_colonial_reading, 0.91).
domain_priors:theater_ratio(jewish_self_determination__settler_colonial_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_self_determination__settler_colonial_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_self_determination__settler_colonial_reading, "Zionist Settlement and Palestinian Displacement (Settler-Colonial Reading)").
narrative_ontology:topic_domain(jewish_self_determination__settler_colonial_reading, "political_philosophy/postcolonial_theory/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_self_determination__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__settler_colonial_reading, '33de1df7-5a6e-4b94-a00e-96a5f897912a').
narrative_ontology:cs_kernel_codification('33de1df7-5a6e-4b94-a00e-96a5f897912a', formalized).
narrative_ontology:cs_authority_grounding('33de1df7-5a6e-4b94-a00e-96a5f897912a', extraction).
narrative_ontology:cs_interpretation_layer_present('33de1df7-5a6e-4b94-a00e-96a5f897912a').
narrative_ontology:cs_reading_relation('33de1df7-5a6e-4b94-a00e-96a5f897912a', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('33de1df7-5a6e-4b94-a00e-96a5f897912a', jewish_self_determination__indigenous_return_reading, forecloses).
narrative_ontology:cs_reading_relation('33de1df7-5a6e-4b94-a00e-96a5f897912a', jewish_self_determination__religious_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('33de1df7-5a6e-4b94-a00e-96a5f897912a', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('33de1df7-5a6e-4b94-a00e-96a5f897912a', foundational, zionism_european_settler_colonial_project).
narrative_ontology:cs_axiom_status(zionism_european_settler_colonial_project, holdable).
narrative_ontology:cs_axiom_grounding('33de1df7-5a6e-4b94-a00e-96a5f897912a', zionism_european_settler_colonial_project, empirically_contingent).
narrative_ontology:cs_axiom('33de1df7-5a6e-4b94-a00e-96a5f897912a', foundational, palestinian_arabs_indigenous_population).
narrative_ontology:cs_axiom_status(palestinian_arabs_indigenous_population, holdable).
narrative_ontology:cs_axiom_grounding('33de1df7-5a6e-4b94-a00e-96a5f897912a', palestinian_arabs_indigenous_population, empirically_contingent).
narrative_ontology:cs_axiom('33de1df7-5a6e-4b94-a00e-96a5f897912a', secondary, dispossession_active_extraction_not_natural_outcome).
narrative_ontology:cs_axiom_status(dispossession_active_extraction_not_natural_outcome, holdable).
narrative_ontology:cs_axiom_grounding('33de1df7-5a6e-4b94-a00e-96a5f897912a', dispossession_active_extraction_not_natural_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('33de1df7-5a6e-4b94-a00e-96a5f897912a', pre_zionist_palestine_ottoman_arab_majority).
narrative_ontology:cs_drift_state('33de1df7-5a6e-4b94-a00e-96a5f897912a', contemporary_2024_occupation_settlement_expansion, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('33de1df7-5a6e-4b94-a00e-96a5f897912a', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__settler_colonial_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, european_jewish_settlers).
narrative_ontology:constraint_beneficiary(jewish_self_determination__settler_colonial_reading, israeli_state).
narrative_ontology:constraint_victim(jewish_self_determination__settler_colonial_reading, palestinian_arabs).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__settler_colonial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__settler_colonial_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures 0.88 at interval end (2024), reflecting: (1) asymmetric transfer of land (Palestinian property → Jewish settlers), (2) resource priority (water, electricity, building materials allocated preferentially to settlements), (3) legal status asymmetry (Law of Return grants Jews automatic citizenship; Palestinians denied return), (4) ongoing settlement expansion as continuous extraction. Suppression measures 0.91 (highest among grid metrics), reflecting: (1) military occupation and checkpoint systems restricting Palestinian movement, (2) permit regimes controlling Palestinian economic activity, (3) curfews and collective punishment, (4) elimination of Palestinian political voice in state decision-making affecting their lives. Theater ratio (0.42) reflects genuine but diminishing coordination narratives (security, refuge from persecution) layered over core extraction function. The measurement series show monotonic increase in extraction and suppression from 1880–2024, with acceleration at 1948 (state formation) and 1967 (occupation begins). The coercion grid shows suppression highest at structural and individual levels (state apparatus + checkpoints + permit denial), with resistance persistent at class level (BDS, intifada) and organizational level (Palestinian political movements). Accessibility collapse rises dramatically after 1948, as legal barriers and territorial fragmentation close off Palestinian exit options.
 *
 * PERSPECTIVAL GAP:
 *   The settler and Israeli-state seats compute this constraint as coordination or security (genuine threat response) with incidental displacement costs. From the Palestinian seat, it computes as pure extraction—the state apparatus serves not to coordinate but to consolidate settler control and Palestinian subordination. Postcolonial analysts occupy an observer seat and compute the constraint as snare from structural data (beneficiary/victim asymmetry, active enforcement, suppression-driven persistence). The six_questions battery captures this: the Israeli framing answers 'coordination_function' as refuge from persecution; this reading's structural analysis answers 'transfer_function' as displacement, property seizure, and legal subordination. The engine should compute per-seat types and flag divergence (snare from Palestinian/analyst seats vs. rope from settler/state seats) as a measured manifestation of the same structure experienced oppositely.
 *
 * DIRECTIONALITY LOGIC:
 *   european_jewish_settlers (d ≈ 0.1–0.2): beneficiaries with arbitrage exit (European citizenship + Palestinian institutional foothold); their mobility is the defining feature that distinguishes them from trapped Palestinians. israeli_state (d ≈ 0.05): principal beneficiary (collects territorial authority, tax base, legitimacy), but trapped—the state cannot exit without dissolution, making it structurally invested in perpetual suppression. palestinian_arabs (d ≈ 0.95): full targets; trapped exit (displacement = statelessness, staying = subordination), subjected to extraction on every axis (land, labor, movement, political voice). The trapped exit for Palestinians is the structural key: it eliminates the mobile arbitrage option that beneficiaries enjoy and locks Palestinians into bearing suppression costs indefinitely.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues the founding problem (European Jewish persecution) has been resolved through immigration, Holocaust memorialization, and international law protections on genocide. The constraint persists not because it solves the founding problem but because it concentrates power and extraction. The 1948 state formation should have marked a transition from 'refuge from persecution' to 'established nation-state,' at which point the foundational justification shifts. This reading classifies the post-1948 continuation as mandatrophy: the founding problem is dead (Jews are no longer fleeing persecution into Palestine; they are establishing a state and expelling indigenous people), but the constraint persists because beneficiaries maintain it. The measurement series support this: theater_ratio remains relatively low (0.42, under the 0.5 threshold for pure piton), indicating functional enforcement is still active, not merely performative. But the (founding_problem_status = dead) + (disappearance_verdict = world_rearranges) + (extraction rising while founding problem becomes obsolete) mismatch flags a mandatrophy condition worthy of reclassification pressure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jewish_indigeneity_contested,
    'Are Jewish people indigenous to Palestine/Israel, and does historical presence in antiquity establish modern territorial claims?',
    'Historical and archaeological scholarship on Jewish presence 70 CE–1800s; geological/demographic analysis of population continuity; comparative analysis with other indigenous-return claims (e.g., Armenian claims to Anatolia, Greek claims to Anatolia); legal precedent on how long territorial gap (1,800+ years) affects indigeneity claims.',
    'If Jewish people are judged indigenous, the settler-colonial framing collapses and the indigenous-return reading gains structural support. If Palestinian Arabs (continuous inhabitance for 1,300+ years, until 1948) are judged the indigenous population, the settler-colonial framing is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jewish_indigeneity_contested, empirical, 'Whether historical Jewish presence establishes modern indigenous rights to Palestine.').

omega_variable(
    dispossession_mechanism_alternative_framing,
    'Does the Land of Return / settlement expansion constitute settler-colonialism, or is it legitimate national self-determination by a historically persecuted people?',
    'Comparative institutional analysis: do the mechanisms (preferential citizenship law, land acquisition, military occupation, legal exclusion) match settler-colonial patterns in North America, Australia, South Africa? Or are they distinguishable because framed as security/nationalism rather than explicit racial/ethnic dominion? Do victim testimonies, demographic displacement statistics, and resource allocation asymmetries support one framing or the other?',
    'If the mechanisms match settler-colonial patterns regardless of subjective intent or narrative framing, the snare classification holds. If the mechanisms are judged structurally distinguishable (nationalism ≠ colonialism), the liberal-nationalist reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispossession_mechanism_alternative_framing, conceptual, 'Whether the institutional mechanisms constitute settler-colonialism or legitimate nationalism.').

omega_variable(
    suppression_mechanism_internalization,
    'Is Palestinian suppression under Israeli law primarily structural (external barriers: checkpoints, permits, military force) or internalized (psychological impact of dispossession, identity fusion with displacement)?',
    'Post-exit trajectory analysis: Palestinian political movements and refugee testimony on whether suppression persists after removal from Israeli control (e.g., in diaspora, in autonomous Palestinian areas). Psychological and sociological studies on identity-formation under occupation; comparison with internalized suppression in other contexts (caste systems, slavery aftermath).',
    'If suppression is primarily structural, the measured 0.91 reflects external coercion that would diminish with constraint removal. If substantially internalized, the effective suppression is higher (targets carry the constraint with them post-exit), and removal is costlier. This affects mandatrophy analysis: internalized suppression suggests longer-term ecosystem damage from the constraint''s operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Suppression mechanism: structural barriers vs. internalized constraints on Palestinian agency.').

omega_variable(
    founding_problem_european_vs_universal,
    'Does the founding problem (European Jewish persecution) justify a specifically Palestinian-territorial solution, or were alternative solutions (diaspora pluralism, European legal protection, international minority-rights regimes) structurally viable?',
    'Counterfactual historical analysis: what were the institutional and political constraints on minority-rights protection in mid-20th-century Europe? What was the actual uptake of diasporist solutions? What historical moment would have allowed different paths (e.g., post-1945 when European legal regimes shifted toward minority protection)?',
    'If Palestinian territory was the only viable solution, the founding-problem framing provides some justification for the constraint. If alternatives were viable and deliberately rejected, the constraint''s targeting of Palestinians appears structurally chosen rather than necessary—strengthening the snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_european_vs_universal, conceptual, 'Whether Palestinian dispossession was the only structurally viable solution to European Jewish persecution.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly do the settler-colonial, indigenous-return, liberal-nationalist, religious-covenant, and diasporist readings diverge structurally?',
    'Formal comparative analysis of each reading''s axioms: settler-colonial places weight on dispossession mechanism + active enforcement + indigenous elimination. Indigenous-return places weight on historical Jewish presence + unbroken connection + rightful return. Liberal-nationalist places weight on nationalist self-determination + equal competing claims. Religious-covenant places weight on divine land grant + theological obligation. Diasporist places weight on minority-rights alternatives + dangers of territorial state. The divergence points are: (1) indigeneity (who counts as indigenous?), (2) necessity (was Palestinian territory necessary?), (3) mechanism (does transfer constitute settler-colonialism?), (4) justification-source (nationalism vs. indigeneity vs. theology vs. minority rights). This omega documents the reading contest at the structural level.',
    'Clarifying the divergence loci helps identify which empirical/conceptual questions are outcome-dispositive for each reading. A single dominant question (e.g., ''Are Jews indigenous?'') might resolve the entire contest; alternatively, each reading might rest on different unresolvable premises (axioms, values, theological claims), making the kernel genuinely contested at the normative level rather than the empirical level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Structural divergence points across the five readings of the jewish_self_determination kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__settler_colonial_reading, 1880, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_self_determination__settler_colonial_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(jewi_tr_t1920, jewish_self_determination__settler_colonial_reading, theater_ratio, 1920, 0.12).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__settler_colonial_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__settler_colonial_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(jewi_tr_t2000, jewish_self_determination__settler_colonial_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__settler_colonial_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement(jewi_be_t1920, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1920, 0.52).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__settler_colonial_reading, base_extractiveness, 1967, 0.83).
narrative_ontology:measurement(jewi_be_t2000, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2000, 0.86).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__settler_colonial_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1880, 0.42).
narrative_ontology:measurement(jewi_su_t1920, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement(jewi_su_t1948, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1948, 0.82).
narrative_ontology:measurement(jewi_su_t1967, jewish_self_determination__settler_colonial_reading, suppression_requirement, 1967, 0.87).
narrative_ontology:measurement(jewi_su_t2000, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(jewi_su_t2024, jewish_self_determination__settler_colonial_reading, suppression_requirement, 2024, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__settler_colonial_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__settler_colonial_reading, 0.22).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__diasporist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, palestinian_statehood_constraint).
narrative_ontology:affects_constraint(jewish_self_determination__settler_colonial_reading, law_of_return_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel jewish_self_determination. The kernel decomposes into five structurally distinct constraints based on competing legitimacy framings: (1) settler-colonial reading (this file): frames Zionism as European settler-colonialism extracting from indigenous Palestinians; (2) liberal-nationalist reading: frames Zionism as equal competing nationalism; (3) indigenous-return reading: frames Zionism as decolonization/return to ancestral homeland; (4) religious-covenant reading: frames Zionism as religious obligation grounded in divine land grant; (5) diasporist reading: frames Zionism as dangerous deviation from diaspora security. Each reading instantiates a different ε value, different beneficiary/victim structure, and different type classification. They are linked via network.affects_constraints because each reading's legitimacy depends on rejecting or reframing the others' core premises. All five stories must be authored in the corpus for the kernel contest to be represented.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__settler_colonial_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
