% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Household-Village Ritual as Diffuse Divine Legitimacy (Folk Syncretistic Reading)
 *   domain: religious/social/institutional
 *
 * SUMMARY:
 *   This constraint story instantiates the folk_syncretistic_reading of the
 *   divine_legitimacy_substrate kernel. In this reading, divine legitimacy is
 *   generated and experienced through decentralized household and village
 *   ritual practice that pragmatically incorporates multiple deities.
 *   Authority is diffuse: there is no single adjudicator, fixed textual
 *   kernel, or centralized beneficiary. The pharaonic court and the Amun
 *   priesthood are structurally backgrounded as distant elites whose claims
 *   do not penetrate the domestic sphere. Because the coordination is enacted
 *   through practice rather than enforced through hierarchy, extraction is
 *   minimal and suppression is low. The reading is decomposed from the
 *   Amun-polytheistic and Atenist-monotheistic readings due to its
 *   structurally distinct authority grounding (practice-based, not
 *   lineage-based or extraction-based), its distributed kernel, and its lack
 *   of concentrated beneficiaries.
 *
 * KEY AGENTS:
 *   - household_practitioners (moderate/local): Primary beneficiaries who generate legitimacy through direct ritual performance.
 *   - pharaonic_court (institutional/national): Excluded state actor whose centralized legitimacy claims are irrelevant to the domestic sphere.
 *   - amun_priesthood (institutional/national): Excluded temple actor whose specialized cult does not regulate household practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.12).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.08).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Household-Village Ritual as Diffuse Divine Legitimacy (Folk Syncretistic Reading)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "religious/social/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '91e73f48-9246-474d-8c1e-8fcd1a2cc4c2').
narrative_ontology:cs_kernel_codification('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', distributed).
narrative_ontology:cs_authority_grounding('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', practice).
narrative_ontology:cs_reading_relation('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', divine_legitimacy_substrate__atenist_monotheistic_reading, coexists_with).
narrative_ontology:cs_axiom('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', foundational, direct_household_divine_access).
narrative_ontology:cs_axiom_status(direct_household_divine_access, holdable).
narrative_ontology:cs_axiom_grounding('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', direct_household_divine_access, theological).
narrative_ontology:cs_axiom('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', foundational, pragmatic_deity_incorporation).
narrative_ontology:cs_axiom_status(pragmatic_deity_incorporation, holdable).
narrative_ontology:cs_axiom_grounding('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', pragmatic_deity_incorporation, conventional).
narrative_ontology:cs_reference_frame('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', household_centric_legitimacy).
narrative_ontology:cs_drift_state('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', new_kingdom_centralization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('91e73f48-9246-474d-8c1e-8fcd1a2cc4c2', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Make offerings at domestic shrines, invoke deities for childbirth, illness, and harvest, and gather with neighbors for local festivals. They exchange ritual labor and symbolic goods within the village; what they receive is divine favor and social solidarity. They could theoretically adopt temple-centered practice or follow a roving prophet, but doing so would cut them off from kin networks and village identity.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Collects taxes, fields armies, and stages royal rituals that assert cosmic order. In the villages, its presence is felt through tax demands and corvÃ©e labor, not through participation in household religion. It does not gain symbolic revenue from local cults and does not attempt to direct them.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaonic_court, excluded,
    institutional, generational, analytical, national).

% Manages the great temple estates, interprets oracles, and provides grand offerings for the state. Their services are distant and expensive for ordinary villagers. They do not regulate household shrines and receive no income from them; their theological claims simply do not map onto the domestic ritual landscape.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, amun_priesthood, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains shared cosmology, agricultural timing, and social solidarity in decentralized settlements where centralized infrastructure is distant or absent, by allowing households to generate and access divine legitimacy directly through enacted ritual.
% TRANSFER_FUNCTION: Moves ritual labor, votive offerings, and symbolic attention from individual households and villages into a shared but locally administered religious economy; no central rent is extracted.
% ABSENT_VOICES: The pharaonic court and the Amun priesthood would assert that divine legitimacy requires their exclusive mediation; they are structurally backgrounded in this reading.
% DISAPPEARANCE_RATIONALE: If household and village ritual practice disappeared, social coordination around agriculture, life-cycle events, and crisis response would lose its immediate cosmological framework; practitioners would need to reconstruct meaning-making through alternative channels, likely centralizing toward temple or state mediation.
% FOUNDING_PROBLEM: How to maintain cosmological order, agricultural timing, and social solidarity in decentralized rural settlements where state and temple infrastructure is distant, intermittent, or absent.
% FOUNDING_PROBLEM_CORROBORATION: Archaeological evidence of domestic shrines and votive deposits predating the New Kingdom state; anthropological parallels from other pre-modern societies attest that household ritual solves coordination problems independent of centralized religious authority. The pharaonic court and temple priesthoods do not corroborate this founding problem because they assert the problem is solved only through their mediation.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because household rituals are performed by the beneficiaries themselves without a rent-collecting intermediary. Suppression is very low (0.08) because alternatives (temple cult, royal ideology) are not actively suppressed; they are simply ignored or backgrounded. Theater ratio is minimal (0.05) because the practice is functionally oriented (childbirth, harvest, healing) rather than performative maintenance of an institution. Accessibility collapse is modest (0.25): once a practitioner understands the household framework, centralized alternatives appear costly and distant, but they do not collapse entirely. Resistance is negligible (0.05) because the arrangement is participatory and diffuse. The temporal series show stable low values, reflecting the persistence of domestic cult across political upheavals.
 *
 * PERSPECTIVAL GAP:
 *   The household practitioner experiences the constraint as autonomous religious practice and social coordination. The pharaonic court and temple priesthood, from their own institutional seats, experience the same domain as one where legitimacy ought to flow through them; they do not recognize the household sphere as a rival authority but simply as an ungoverned space. The engine will compute divergent per-seat classifications: a low-directionality beneficiary seat for practitioners and a high-directionality (or analytically neutral) seat for excluded institutional actors who neither pay nor benefit within this specific constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Household practitioners are declared beneficiaries because the constraint coordinates their social and cosmological environment without extracting from them; their directionality sits near the beneficiary end. The pharaonic court and Amun priesthood are not victims in this constraint because the constraint does not extract from them; rather, they are excluded from the legitimacy flow. No directionality override is needed because the structural derivation (beneficiary = low d, excluded actors = no derived d from this constraint) matches the analytical picture.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a case of mandatrophy because the founding problemâhow to maintain cosmological and social order in decentralized settlementsâremains live. The domestic cult did not outlive its function; it persisted because it continued to solve coordination problems (agricultural timing, life-cycle transitions, health crises) that centralized institutions could not address at the village level. There is no decayed function maintained by inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_concentration_ambiguity,
    'Is the absence of concentrated beneficiaries in the folk reading evidence of genuine diffuse coordination, or does it conceal extraction by local elites (headmen, healers) whose role is invisible in the source record?',
    'Archaeological identification of wealth differentiation within village ritual contexts; textual evidence of payment for domestic religious services.',
    'If local elites extract rents, the constraint shifts toward tangled_rope or snare; if not, it remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_concentration_ambiguity, empirical, 'Whether diffuse household practice hides concentrated local extraction.').

omega_variable(
    suppression_as_state_capacity,
    'Does the low measured suppression reflect genuine autonomy of household practice, or merely the state''s temporary incapacity to penetrate the domestic sphere?',
    'Comparative analysis of state-centralization episodes (e.g., Amarna, Ptolemaic registration) and their impact on household cult.',
    'If suppression is low only due to state incapacity, then under stronger centralization the constraint would register as more actively enforced or suppressed; if autonomy is structural, the metric holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_as_state_capacity, conceptual, 'Whether low suppression is structural autonomy or state capacity limit.').

omega_variable(
    kernel_reading_boundary,
    'What structural element most clearly distinguishes the folk reading from its Amun and Atenist siblings: the distributed kernel, the practice-based authority, or the absence of a designated beneficiary?',
    'Cross-reading comparison of authority_grounding and beneficiary arrays.',
    'Determines which variable is load-bearing for decomposition; if the kernel alone suffices, the readings are epistemic variants; if authority grounding is decisive, they are institutional competitors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Which structural feature differentiates the folk reading from siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(divi_tr_t10, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(divi_tr_t30, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(divi_tr_t40, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 40, 0.06).
narrative_ontology:measurement(divi_tr_t50, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(divi_be_t10, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 10, 0.11).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(divi_be_t30, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(divi_be_t40, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 40, 0.13).
narrative_ontology:measurement(divi_be_t50, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 50, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(divine_legitimacy_substrate__folk_syncretistic_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% This constraint is the folk-syncretistic reading of the divine legitimacy substrate kernel, decomposed from the Amun-priestly and Atenist-monotheistic readings due to structurally distinct beneficiary, authority, and enforcement profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
