% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__colonial_census_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__colonial_census_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: jati_practice_norm__colonial_census_reading
 *   human_readable: Colonial Census Jati Reification
 *   domain: social_anthropology/religious_studies/political_economy
 *
 * SUMMARY:
 *   This constraint is the colonial census reading of the contested
 *   jati_practice_norm kernel. Under British colonial rule, fluid and locally
 *   negotiated jati boundaries were stabilized into fixed administrative
 *   categories through the decennial census and district gazetteers. The
 *   resulting constraint is a tangled rope: it delivered genuine coordination
 *   value to the colonial state (taxation, judicial administration, political
 *   representation) while asymmetrically extracting social autonomy from
 *   local communities and freezing itinerant populations into rigid
 *   occupational-residential identities. The claim/metric independence is
 *   maintained: the constraint is claimed as tangled_rope, and the metrics
 *   describe moderate but rising extractiveness backed by substantial active
 *   enforcement.
 *
 * KEY AGENTS:
 *   - colonial_administration: Primary agenda setter (institutional/arbitrage) â designs and enforces the census apparatus
 *   - census_bureaucracy: Institutional beneficiary (organized/constrained) â expands through category maintenance
 *   - local_communities: Primary payer (moderate/constrained) â loses autonomy over social boundary-making
 *   - mobile_artisans: Secondary payer (powerless/trapped) â criminalized mobility, forced into fixed categories
 *   - indigenous_elites: Conditional beneficiary (powerful/constrained) â gains colonial access by accepting reified framework
 *   - contemporary_scholars: Analytical observer (analytical/analytical) â documents structural transformation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, 0.62).
domain_priors:suppression_score(jati_practice_norm__colonial_census_reading, 0.7).
domain_priors:theater_ratio(jati_practice_norm__colonial_census_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jati_practice_norm__colonial_census_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__colonial_census_reading, tangled_rope).
narrative_ontology:human_readable(jati_practice_norm__colonial_census_reading, "Colonial Census Jati Reification").
narrative_ontology:topic_domain(jati_practice_norm__colonial_census_reading, "social_anthropology/religious_studies/political_economy").

domain_priors:requires_active_enforcement(jati_practice_norm__colonial_census_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__colonial_census_reading, '5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a').
narrative_ontology:cs_kernel_codification('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', formalized).
narrative_ontology:cs_authority_grounding('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', extraction).
narrative_ontology:cs_interpretation_layer_present('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a').
narrative_ontology:cs_reading_relation('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', jati_practice_norm__localized_practice_reading, influences).
narrative_ontology:cs_reading_relation('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', jati_practice_norm__orthodox_textual_reading, coexists_with).
narrative_ontology:cs_axiom('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', foundational, administrative_enumeration_supersedes_local_negotiation).
narrative_ontology:cs_axiom_status(administrative_enumeration_supersedes_local_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', administrative_enumeration_supersedes_local_negotiation, conventional).
narrative_ontology:cs_axiom('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', foundational, governance_legibility_trumps_social_fluidity).
narrative_ontology:cs_axiom_status(governance_legibility_trumps_social_fluidity, holdable).
narrative_ontology:cs_axiom_grounding('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', governance_legibility_trumps_social_fluidity, instrumental).
narrative_ontology:cs_reference_frame('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', colonial_governance_legibility).
narrative_ontology:cs_drift_state('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', postcolonial_succession_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('5c247bf6-36c6-4dd3-abb4-51e1dd5ccd5a', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__colonial_census_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, colonial_administration).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, census_bureaucracy).
narrative_ontology:constraint_beneficiary(jati_practice_norm__colonial_census_reading, indigenous_elites).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, local_communities).
narrative_ontology:constraint_victim(jati_practice_norm__colonial_census_reading, mobile_artisans).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, governance_legibility_doctrine).
narrative_ontology:constraint_vindicates(jati_practice_norm__colonial_census_reading, administrative_positivism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Imposed census classifications and administrative jati categories to render the population legible for revenue extraction, labor recruitment, and political control. Maintained the schedule through district gazetteers, legal recognition, and enforcement machinery.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Professional cadre whose authority, employment, and institutional budgets expanded through the creation and maintenance of ethnographic and census knowledge. Their careers depend on the stability of the categories they enumerate and refine.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, census_bureaucracy, beneficiary,
    organized, biographical, constrained, national).

% Found their fluid, context-bound social identities frozen into fixed census entries that determined access to land, courts, marriage licenses, and public goods. Administrative identity became a hard constraint on everyday social practice.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, local_communities, payer,
    moderate, generational, constrained, regional).

% Itinerant craftsmen and service providers whose livelihoods depended on crossing localized jati boundaries. Colonial census and attached settlement policies criminalized their mobility and forced them into fixed residential and occupational categories.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, mobile_artisans, payer,
    powerless, biographical, trapped, regional).

% Certain local elites who successfully claimed high-status census categories gained privileged access to colonial institutions, representative bodies, and land tenure. Their benefit is conditional on accepting the reified framework and policing community boundaries.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, indigenous_elites, beneficiary,
    powerful, generational, constrained, regional).

% Historians and anthropologists who document how colonial enumeration transformed jati from fluid practice into rigid administrative identity, tracing the structural effects of census categories on post-colonial society.
narrative_ontology:constraint_stakeholder(jati_practice_norm__colonial_census_reading, contemporary_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__colonial_census_reading, colonial_administration).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Rendered a highly diverse and locally varying population administratively legible for taxation, judicial administration, and political representation by assigning every individual to a fixed jati category in census and gazetteer.
% TRANSFER_FUNCTION: Transferred autonomy over social boundary-making from local communities and context-specific negotiation to colonial administrative enumeration; transferred material advantage to census-recognized high-status groups and the colonial bureaucracy itself.
% ABSENT_VOICES: Local practitioners of fluid boundary-making, itinerant occupational groups, and women whose status was often misrecorded or simplified were excluded from the census design process; their objections surface only in petition archives and later ethnography.
% DISAPPEARANCE_RATIONALE: If the colonial census categories and their legal enforcement vanished, administrative identity would revert to local negotiation, land and marriage records would require renegotiation, and the post-colonial affirmative-action infrastructure tied to jati enumeration would lose its primary source of categorical stability.
% FOUNDING_PROBLEM: Colonial rule required knowledge of the population for revenue, security, and governance in a territory where existing social identities were fluid, localized, and illegible to centralized administration.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary post-colonial historians and subaltern studies scholars attest that the colonial governance problem is gone, but the categorical infrastructure persists; colonial administrators themselves documented the artificiality of the categories they imposed.
narrative_ontology:disappearance_verdict(jati_practice_norm__colonial_census_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__colonial_census_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__colonial_census_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__colonial_census_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__colonial_census_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__colonial_census_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__colonial_census_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__colonial_census_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.62â0.65) because the colonial state did obtain real coordination goods from legibility, but the price was paid by communities whose fluid practices were frozen. Suppression is high (0.70â0.75) because persistence required active enumeration, legal enforcement, and policing of boundary violations. Theater_ratio rises from 0.20 to 0.45 as early pragmatic categorization hardened into a performed naturalism. Accessibility_collapse is 0.60 because once categories were inscribed in census and law, alternative self-identifications became administratively invisible. Resistance is 0.55 because local evasion, petitioning, and renegotiation were persistent but insufficient to dismantle the apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The colonial administration seat perceives necessary governance infrastructure that makes a diverse empire governable; the local community and mobile artisan seats perceive an externally imposed extraction of naming rights, occupational freedom, and social autonomy. The indigenous elite seat is split, experiencing both conditional benefit and constraint. The engine computes these divergent classifications from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Colonial administration sits near the full-beneficiary end (low d) because it both designs the constraint and can exit or revise it at will. Census bureaucracy and indigenous elites sit at low-to-moderate d because they collect benefits but are institutionally or socially constrained. Local communities sit at high d (payer, constrained exit), and mobile artisans sit nearest full-target (payer, trapped). The observer seat carries no directional load.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â centralized governance legibility over a diverse subcontinent â was genuinely live in 1881. By 1951 the colonial authority itself had eroded, yet the categorical infrastructure persisted and was inherited by the post-colonial state. This is a classic mandatrophy risk: founding_problem_status is dead, but disappearance_verdict is world_rearranges, flagging that the arrangement has become a zombie constraint whose persistence exceeds its original justification. The tangled_rope classification captures this by requiring both coordination function and asymmetric extraction; a pure snare classification would miss the genuine governance problem the census initially addressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    colonial_intent_ambiguity,
    'Was the freezing of jati categories an intentional strategy of divide-and-rule, or an unintended byproduct of bureaucratic coordination imperatives?',
    'Archival analysis of colonial policy deliberations and internal correspondence between provincial governors and the Census Commissioner.',
    'If intentional extraction, the constraint trends toward snare; if an unintended byproduct of legibility, it is better modeled as scaffold or piton drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_intent_ambiguity, conceptual, 'Whether colonial extraction was designed or emergent').

omega_variable(
    postcolonial_instrumentalization,
    'To what extent does the independent Indian state maintain colonial census categories because they serve post-colonial governance (reservation, political representation), versus genuine community attachment?',
    'Comparative analysis of states that modified versus retained colonial schedules, correlated with political mobilization around reserved categories.',
    'If instrumental, the constraint has transitioned from colonial tangled rope to post-colonial snare or scaffold; if community-driven, extraction is lower than the colonial-period metric suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(postcolonial_instrumentalization, empirical, 'Whether post-colonial persistence is state instrumentality or social demand').

omega_variable(
    suppression_internalization,
    'Is the persistence of reified jati due to ongoing state coercion or internalized identity that persists after state pressure recedes?',
    'Ethnographic observation of inter-group relations and self-identification in regions with historically weak colonial and post-colonial state presence.',
    'If internalized, effective suppression exceeds the structural measure because the target communities carry the constraint with them; if purely structural, suppression should track state capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__colonial_census_reading, 1881, 1951).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_colonial_tr_t1881, jati_practice_norm__colonial_census_reading, theater_ratio, 1881, 0.2).
narrative_ontology:measurement(jati_colonial_tr_t1891, jati_practice_norm__colonial_census_reading, theater_ratio, 1891, 0.25).
narrative_ontology:measurement(jati_colonial_tr_t1901, jati_practice_norm__colonial_census_reading, theater_ratio, 1901, 0.3).
narrative_ontology:measurement(jati_colonial_tr_t1911, jati_practice_norm__colonial_census_reading, theater_ratio, 1911, 0.35).
narrative_ontology:measurement(jati_colonial_tr_t1921, jati_practice_norm__colonial_census_reading, theater_ratio, 1921, 0.38).
narrative_ontology:measurement(jati_colonial_tr_t1931, jati_practice_norm__colonial_census_reading, theater_ratio, 1931, 0.4).
narrative_ontology:measurement(jati_colonial_tr_t1941, jati_practice_norm__colonial_census_reading, theater_ratio, 1941, 0.42).
narrative_ontology:measurement(jati_colonial_tr_t1951, jati_practice_norm__colonial_census_reading, theater_ratio, 1951, 0.45).

% Extraction over time
narrative_ontology:measurement(jati_colonial_be_t1881, jati_practice_norm__colonial_census_reading, base_extractiveness, 1881, 0.45).
narrative_ontology:measurement(jati_colonial_be_t1891, jati_practice_norm__colonial_census_reading, base_extractiveness, 1891, 0.5).
narrative_ontology:measurement(jati_colonial_be_t1901, jati_practice_norm__colonial_census_reading, base_extractiveness, 1901, 0.55).
narrative_ontology:measurement(jati_colonial_be_t1911, jati_practice_norm__colonial_census_reading, base_extractiveness, 1911, 0.6).
narrative_ontology:measurement(jati_colonial_be_t1921, jati_practice_norm__colonial_census_reading, base_extractiveness, 1921, 0.62).
narrative_ontology:measurement(jati_colonial_be_t1931, jati_practice_norm__colonial_census_reading, base_extractiveness, 1931, 0.63).
narrative_ontology:measurement(jati_colonial_be_t1941, jati_practice_norm__colonial_census_reading, base_extractiveness, 1941, 0.64).
narrative_ontology:measurement(jati_colonial_be_t1951, jati_practice_norm__colonial_census_reading, base_extractiveness, 1951, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jati_colonial_su_t1881, jati_practice_norm__colonial_census_reading, suppression_requirement, 1881, 0.5).
narrative_ontology:measurement(jati_colonial_su_t1891, jati_practice_norm__colonial_census_reading, suppression_requirement, 1891, 0.58).
narrative_ontology:measurement(jati_colonial_su_t1901, jati_practice_norm__colonial_census_reading, suppression_requirement, 1901, 0.65).
narrative_ontology:measurement(jati_colonial_su_t1911, jati_practice_norm__colonial_census_reading, suppression_requirement, 1911, 0.7).
narrative_ontology:measurement(jati_colonial_su_t1921, jati_practice_norm__colonial_census_reading, suppression_requirement, 1921, 0.72).
narrative_ontology:measurement(jati_colonial_su_t1931, jati_practice_norm__colonial_census_reading, suppression_requirement, 1931, 0.73).
narrative_ontology:measurement(jati_colonial_su_t1941, jati_practice_norm__colonial_census_reading, suppression_requirement, 1941, 0.74).
narrative_ontology:measurement(jati_colonial_su_t1951, jati_practice_norm__colonial_census_reading, suppression_requirement, 1951, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__colonial_census_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__colonial_census_reading, jati_practice_norm__orthodox_textual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jati_practice_norm kernel, which decomposes into structurally distinct claims: colonial administrative reification (this file), localized practice negotiation, and orthodox textual derivation. The epsilon values and stakeholder structures differ across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
