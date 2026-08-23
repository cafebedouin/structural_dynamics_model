% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Communal Autonomy Marriage Authority
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint instantiates the communal_autonomy_reading of the
 *   marriage_authority kernel: family law authority rests with religious
 *   communities, the state enforces but does not author norms. The
 *   arrangement presents as coordination (pluralism accommodation) but
 *   operates with asymmetric extraction — religious leadership gains
 *   authority rents while intra-community dissenters (especially women) bear
 *   unequal norms with identity-locked exit. State courts increasingly impose
 *   constitutional equality floors, creating tension between the reading's
 *   reference frame (communal autonomy) and drift (authority erosion). The
 *   claimed type is tangled_rope: genuine coordination function (preventing
 *   majoritarian UCC) plus extraction (dissenters pay).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.45).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.42).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Communal Autonomy Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, 'b282dd12-be77-4d0b-994e-5d1078e38fca').
narrative_ontology:cs_kernel_codification('b282dd12-be77-4d0b-994e-5d1078e38fca', distributed).
narrative_ontology:cs_authority_grounding('b282dd12-be77-4d0b-994e-5d1078e38fca', lineage).
narrative_ontology:cs_interpretation_layer_present('b282dd12-be77-4d0b-994e-5d1078e38fca').
narrative_ontology:cs_reading_relation('b282dd12-be77-4d0b-994e-5d1078e38fca', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('b282dd12-be77-4d0b-994e-5d1078e38fca', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('b282dd12-be77-4d0b-994e-5d1078e38fca', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('b282dd12-be77-4d0b-994e-5d1078e38fca', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('b282dd12-be77-4d0b-994e-5d1078e38fca', foundational, communal_autonomy_over_family_law).
narrative_ontology:cs_axiom_status(communal_autonomy_over_family_law, holdable).
narrative_ontology:cs_axiom_grounding('b282dd12-be77-4d0b-994e-5d1078e38fca', communal_autonomy_over_family_law, conventional).
narrative_ontology:cs_axiom('b282dd12-be77-4d0b-994e-5d1078e38fca', secondary, state_enforcement_without_authorship).
narrative_ontology:cs_axiom_status(state_enforcement_without_authorship, holdable).
narrative_ontology:cs_axiom_grounding('b282dd12-be77-4d0b-994e-5d1078e38fca', state_enforcement_without_authorship, conventional).
narrative_ontology:cs_reference_frame('b282dd12-be77-4d0b-994e-5d1078e38fca', communal_legal_autonomy).
narrative_ontology:cs_drift_state('b282dd12-be77-4d0b-994e-5d1078e38fca', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b282dd12-be77-4d0b-994e-5d1078e38fca', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, community_members).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, communal_self_governance_in_family_law).
narrative_ontology:constraint_vindicates(marriage_authority__communal_autonomy_reading, legal_pluralism_as_constitutional_accommodation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and interpret community personal law norms (marriage, divorce, inheritance). Derive institutional legitimacy and material support from their gatekeeping role over family law. State courts defer to their rulings absent constitutional challenge. Exit would mean losing authority over the community's legal identity.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, religious_leadership, beneficiary).

% Enforce community personal law decisions through civil recognition mechanisms. Do not author the norms but provide the coercive backend (registration, enforcement of decrees). Increasingly pressured to impose constitutional equality floors. Cannot exit the enforcement role without legislative reform.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_courts, agenda_setter,
    institutional, generational, analytical, national).

% Subject to personal law norms they reject (e.g., unequal divorce rights, polygamy, inheritance discrimination). Exit requires leaving the community entirely — losing family, social network, economic support, and often religious identity. State civil law alternatives exist but are socially inaccessible.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, local).

% Gain recognized marriage status, inheritance certainty, and social cohesion through community law. The system works for them as long as they conform. Exit to civil marriage is legally possible but carries severe social sanction.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, community_members, beneficiary,
    moderate, biographical, constrained, regional).

% Challenge gender-unequal personal law provisions through constitutional litigation and legislative advocacy. Structurally excluded from community law-making bodies. Their reforms require state intervention against community autonomy claims.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, gender_equality_advocates, excluded,
    organized, generational, mobile, national).

% Favor Uniform Civil Code to replace personal law pluralism. Blocked by coalition politics and minority community veto power. Would author marriage law centrally but lack legislative majority to overcome pluralism protections.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, secular_legislators, excluded,
    powerful, biographical, mobile, national).

% Analyze the tension between group rights and individual equality across comparative jurisdictions. No stake in outcome; track doctrinal evolution and state-community negotiation.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates marriage recognition across religiously diverse populations by delegating family law authority to community institutions, preventing majoritarian imposition of a single code while maintaining state-recognized legal status for all marriages.
% TRANSFER_FUNCTION: Transfers legislative authority over marriage/divorce/inheritance from the democratic legislature to religious leadership; intra-community dissenters bear compliance costs (unequal rights, restricted exit) while religious leadership collects legitimacy rents and state courts supply enforcement labor.
% ABSENT_VOICES: Gender equality advocates within communities (especially women denied equal divorce/inheritance), interfaith couples blocked by community endogamy rules, secular legislators favoring UCC — all structurally excluded from community law-making bodies where norms are authored.
% DISAPPEARANCE_RATIONALE: If communal authority vanished overnight, the state would either impose a Uniform Civil Code (majoritarian reorganization) or face a vacuum where marriages lack recognized legal status — both would radically rearrange family law for millions.
% FOUNDING_PROBLEM: Post-colonial constitutional settlement needed to accommodate deep religious diversity while preserving state recognition of marriage; communal autonomy was the negotiated compromise — communities keep family law, state provides enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Constituent assembly debates and minority community leaders attest the accommodation remains live; gender rights advocates and constitutional scholars attest the founding problem is dead (diversity accommodated, now inequality entrenched) — no consensus outside beneficiary set.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).
:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) because the primary transfer is authority/legitimacy to religious leadership, not direct material rents; dissenters pay in constrained rights. Suppression is moderate (0.42) — state enforcement is real but civil alternatives exist on paper; the real barrier is identity-locked exit. Theater is low (0.22) — community courts perform genuine adjudication, not mere performance. Accessibility collapse is partial (0.52) — civil marriage exists but is socially inaccessible for most. Resistance is moderate (0.48) — constitutional litigation and legislative UCC pushes are persistent but blocked by pluralism protections.
 *
 * PERSPECTIVAL GAP:
 *   From religious leadership's seat: genuine coordination solving pluralism. From dissenters' seat: enforced extraction with no exit. From state courts' seat: enforcement burden growing as constitutional equality demands conflict with communal norms. The engine computes this divergence from structural data — the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership sits at d~0.15 (beneficiary: collects authority, controls norms, constrained exit). State courts at d~0.45 (symmetric: enforcement labor but institutional role). Community members at d~0.35 (beneficiary: coordination gain, constrained exit). Intra-community dissenters at d~0.85 (full target: identity-locked, bear unequal norms). Gender advocates and secular legislators are excluded (d undefined — not governed by this constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-colonial pluralism accommodation) is contested — beneficiaries say live, victims say dead. The arrangement persists because religious leadership (agenda_setter) blocks reform, state courts (agenda_setter) lack legislative mandate to impose UCC, and dissenters (payer) are identity-locked. This is not a piton — the constraint has active beneficiaries and active enforcement — but mandatrophy is unresolved: the coordination function (anti-majoritarian pluralism) coexists with extraction (gender inequality).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_reading,
    'How does this reading''s structural classification change if the kernel''s authority is re-grounded from communal tradition to constitutional equality?',
    'Compare χ across readings: secularist_reading eliminates communal authority (ε→0 for religious leadership, ε↑ for state); gender_rights_reading keeps pluralism but imposes equality floor (ε↓ for dissenters); judicial_harmonization_reading creates case-by-case drift.',
    'If secularist_reading becomes dominant, this constraint dissolves (reclassified as snare historically). If gender_rights_reading dominates, this constraint''s extraction drops but coordination remains (tangled_rope→rope). The kernel''s structural identity depends on which reading''s axioms hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading, conceptual, 'This constraint is one reading of the contested marriage_authority kernel; its ε and type are reading-indexed.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the high suppression experienced by intra-community dissenters structural (state enforcement of community norms) or internalized (identity fusion making exit unthinkable)?',
    'Track post-exit trajectories: dissenters who leave community — does suppression persist? If yes, internalized component is significant. Survey deterrence: would civil marriage uptake increase if social sanctions were removed?',
    'If internalized, effective suppression > structural measure; dissenters carry the constraint after formal exit. If structural, state policy change (civil marriage access + anti-sanction law) could reduce χ substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression for identity-locked dissenters.').

omega_variable(
    coordination_extraction_boundary,
    'Is the pluralism coordination function (preventing majoritarian UCC) genuine and separable from the extraction function (gender-unequal norms), or is pluralism the cover story for extraction?',
    'Counterfactual: if gender equality were mandated within personal laws, would communities still demand autonomy? If yes, coordination is genuine. If autonomy demands vanish, pluralism was extraction''s vehicle.',
    'If separable, tangled_rope classification holds (coordination + extraction). If inseparable, the constraint is snare (coordination is pretext). Determines whether reform (equality floor) or abolition (UCC) is the structurally appropriate remedy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether pluralism and patriarchy are structurally coupled or contingently fused.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(macar_tr_t1950, marriage_authority__communal_autonomy_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(macar_tr_t1970, marriage_authority__communal_autonomy_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(macar_tr_t1990, marriage_authority__communal_autonomy_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(macar_tr_t2000, marriage_authority__communal_autonomy_reading, theater_ratio, 2000, 0.21).
narrative_ontology:measurement(macar_tr_t2010, marriage_authority__communal_autonomy_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(macar_tr_t2020, marriage_authority__communal_autonomy_reading, theater_ratio, 2020, 0.22).
narrative_ontology:measurement(macar_tr_t2024, marriage_authority__communal_autonomy_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(macar_be_t1950, marriage_authority__communal_autonomy_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(macar_be_t1970, marriage_authority__communal_autonomy_reading, base_extractiveness, 1970, 0.32).
narrative_ontology:measurement(macar_be_t1990, marriage_authority__communal_autonomy_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(macar_be_t2000, marriage_authority__communal_autonomy_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(macar_be_t2010, marriage_authority__communal_autonomy_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(macar_be_t2020, marriage_authority__communal_autonomy_reading, base_extractiveness, 2020, 0.45).
narrative_ontology:measurement(macar_be_t2024, marriage_authority__communal_autonomy_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(macar_su_t1950, marriage_authority__communal_autonomy_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(macar_su_t1970, marriage_authority__communal_autonomy_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(macar_su_t1990, marriage_authority__communal_autonomy_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(macar_su_t2000, marriage_authority__communal_autonomy_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(macar_su_t2010, marriage_authority__communal_autonomy_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement(macar_su_t2020, marriage_authority__communal_autonomy_reading, suppression_requirement, 2020, 0.42).
narrative_ontology:measurement(macar_su_t2024, marriage_authority__communal_autonomy_reading, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__communal_autonomy_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% Marriage authority kernel decomposes into five readings. This reading (communal_autonomy) asserts communal tradition as authority ground; secularist asserts legislative authority; gender_rights asserts constitutional equality; federalist_millet asserts pluralism as anti-tyranny; judicial_harmonization asserts case-by-case constitutional floor. ε values diverge: communal_autonomy (0.45), secularist (0.15 for state, 0.6 for communities), gender_rights (0.3), federalist_millet (0.25), judicial_harmonization (0.35).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__communal_autonomy_reading, organized, 0.15).
constraint_indexing:directionality_override(marriage_authority__communal_autonomy_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
