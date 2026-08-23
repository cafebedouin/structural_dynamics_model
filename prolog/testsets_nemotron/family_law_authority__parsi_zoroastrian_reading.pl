% ============================================================================
% CONSTRAINT STORY: family_law_authority__parsi_zoroastrian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__parsi_zoroastrian_reading, []).

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
 *   constraint_id: family_law_authority__parsi_zoroastrian_reading
 *   human_readable: Parsi Zoroastrian Marriage Law: Endogamy & Priestly Authority
 *   domain: religious_governance/family_law/community_preservation
 *
 * SUMMARY:
 *   Parsi Zoroastrian marriage law operates as a community-preserving
 *   constraint: marriage outside the community triggers loss of ritual
 *   status, trust benefits, and communal identity. The Anuman (community
 *   council) and hereditary priesthood (mobeds) jointly administer this
 *   boundary. The constraint is claimed as coordination (community survival)
 *   but operates with substantial extraction (asymmetric penalties on women,
 *   irreversible exclusion, priestly gatekeeping over navjote). Demographic
 *   decline (from ~114,000 in 1941 to ~57,000 in 2011) intensifies
 *   enforcement rather than prompting reform — a classic tangled rope where
 *   the coordination function (survival) and extraction function (boundary
 *   maintenance via gender-asymmetric penalties) are structurally fused.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, 0.62).
domain_priors:suppression_score(family_law_authority__parsi_zoroastrian_reading, 0.78).
domain_priors:theater_ratio(family_law_authority__parsi_zoroastrian_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(family_law_authority__parsi_zoroastrian_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__parsi_zoroastrian_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__parsi_zoroastrian_reading, "Parsi Zoroastrian Marriage Law: Endogamy & Priestly Authority").
narrative_ontology:topic_domain(family_law_authority__parsi_zoroastrian_reading, "religious_governance/family_law/community_preservation").

domain_priors:requires_active_enforcement(family_law_authority__parsi_zoroastrian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__parsi_zoroastrian_reading, 'd6a024c8-51d8-409b-82c0-6fa454692bab').
narrative_ontology:cs_kernel_codification('d6a024c8-51d8-409b-82c0-6fa454692bab', fixed_text).
narrative_ontology:cs_authority_grounding('d6a024c8-51d8-409b-82c0-6fa454692bab', lineage).
narrative_ontology:cs_interpretation_layer_present('d6a024c8-51d8-409b-82c0-6fa454692bab').
narrative_ontology:cs_reading_relation('d6a024c8-51d8-409b-82c0-6fa454692bab', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6a024c8-51d8-409b-82c0-6fa454692bab', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6a024c8-51d8-409b-82c0-6fa454692bab', family_law_authority__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('d6a024c8-51d8-409b-82c0-6fa454692bab', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('d6a024c8-51d8-409b-82c0-6fa454692bab', foundational, endogamy_necessary_for_community_survival).
narrative_ontology:cs_axiom_status(endogamy_necessary_for_community_survival, holdable).
narrative_ontology:cs_axiom_grounding('d6a024c8-51d8-409b-82c0-6fa454692bab', endogamy_necessary_for_community_survival, instrumental).
narrative_ontology:cs_axiom('d6a024c8-51d8-409b-82c0-6fa454692bab', foundational, priestly_ritual_monopoly_validates_marriage).
narrative_ontology:cs_axiom_status(priestly_ritual_monopoly_validates_marriage, holdable).
narrative_ontology:cs_axiom_grounding('d6a024c8-51d8-409b-82c0-6fa454692bab', priestly_ritual_monopoly_validates_marriage, conventional).
narrative_ontology:cs_reference_frame('d6a024c8-51d8-409b-82c0-6fa454692bab', post_sassanian_diaspora_founding).
narrative_ontology:cs_drift_state('d6a024c8-51d8-409b-82c0-6fa454692bab', contemporary_demographic_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d6a024c8-51d8-409b-82c0-6fa454692bab', '').
narrative_ontology:cs_kernel_id(family_law_authority__parsi_zoroastrian_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_anuman_community).
narrative_ontology:constraint_beneficiary(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, intermarried_parsi_individuals).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_women_marrying_out).
narrative_ontology:constraint_victim(family_law_authority__parsi_zoroastrian_reading, parsi_youth_seeking_exogamy).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, community_survival_requires_endogamy).
narrative_ontology:constraint_vindicates(family_law_authority__parsi_zoroastrian_reading, priestly_ritual_authority_preserves_orthopraxy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Parsi Anuman (community council) administers marriage law, validates priestly credentials, and maintains community rolls. It collects dues, controls trust assets (fire temples, housing colonies), and defines who counts as Parsi for institutional purposes. Its authority rests on the claim that endogamy preserves the community's survival.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_anuman_community, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_anuman_community, beneficiary).

% Hereditary priesthood (mobeds) performs marriage rituals (nirangdin) and certifies validity. Their livelihood, status, and ritual authority depend on the community's endogamous boundary. They receive dakshina (ritual fees) and hold gatekeeping power over who may marry within the faith.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_priesthood, beneficiary,
    organized, biographical, identity_locked, regional).

% Parsis who marry non-Parsis lose community status: cannot enter fire temples, cannot have children initiated (navjote), cannot be buried in Towers of Silence, lose trust benefits. Women marrying out face total exclusion; men marrying out retain limited rights but children are excluded unless mother is Parsi and navjote is performed — a contested, priest-dependent process.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, intermarried_parsi_individuals, payer,
    moderate, biographical, constrained, regional).

% Parsi women who marry non-Parsi men face automatic, irreversible excommunication under traditional interpretation. They lose all ritual, communal, and material rights. No exit option exists within the community framework — the penalty is structural and identity-fused. Some seek secular legal remedy (e.g., Goolrokh Gupta case), but community rolls remain under priestly control.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_women_marrying_out, payer,
    powerless, biographical, trapped, regional).

% Young Parsis who wish to marry outside the community face a forced choice: partner or community. They are not present in Anuman deliberations; their preferences are treated as threats to survival. Some negotiate navjote for children post-hoc, but success depends on priestly discretion and father's status — a structurally asymmetric bargaining position.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, parsi_youth_seeking_exogamy, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(family_law_authority__parsi_zoroastrian_reading, parsi_youth_seeking_exogamy, excluded).

% Indian courts (Supreme Court, High Courts) adjudicate disputes where Parsi personal law intersects constitutional rights (Articles 14, 15, 21, 25). They have upheld community autonomy in some cases (Sir Dinshaw Petit), struck down gender discrimination in others (Goolrokh Gupta 2017), but defer to community definition of membership. Their rulings create external pressure without resolving the internal logic.
narrative_ontology:constraint_stakeholder(family_law_authority__parsi_zoroastrian_reading, secular_courts_india, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a small, distinct ethno-religious community (est. 57,000 in India, declining) by binding marriage to community membership, using priestly ritual as the validation mechanism and trust assets as the material anchor.
% TRANSFER_FUNCTION: Moves ritual inclusion, trust benefits (housing, education, fire temple access), and communal identity from intermarrying individuals to the community institution. The community retains its boundary; the individual loses their place in the collective.
% ABSENT_VOICES: Children of intermarriages (especially those denied navjote), non-Parsi spouses seeking inclusion, reformist Parsis advocating patrilineal/matrilineal symmetry, and diaspora Parsis in jurisdictions where religious law has no civil force — all are structurally excluded from Anuman decision-making.
% DISAPPEARANCE_RATIONALE: If the endogamy rule and priestly monopoly vanished overnight, the Parsi community would likely dissolve as a distinct institution within 2-3 generations: intermarriage rates already exceed 30%, trust assets would face competing claims, and the priesthood would lose its ritual function. The constraint is the structural scaffold holding the community's corporate existence together.
% FOUNDING_PROBLEM: Post-Arab-conquest survival of a persecuted minority: the community needed a hard boundary to prevent assimilation, a portable priesthood to maintain ritual continuity without territory, and trust assets to sustain collective welfare under hostile regimes.
% FOUNDING_PROBLEM_CORROBORATION: Reformist Parsis (e.g., Zoroastrian Studies Group, Association of Inter-Married Zoroastrians) attest the founding problem (survival under persecution) is substantially altered — the community now faces demographic collapse from the boundary itself, not external threat. Traditionalist Anuman and priesthood attest the threat persists (assimilation = extinction). Demographers outside the community (e.g., Leela Visaria, UNFPA reports) corroborate the demographic trajectory but not the causal claim that endogamy alone preserves identity.
narrative_ontology:disappearance_verdict(family_law_authority__parsi_zoroastrian_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__parsi_zoroastrian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__parsi_zoroastrian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(family_law_authority__parsi_zoroastrian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__parsi_zoroastrian_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__parsi_zoroastrian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__parsi_zoroastrian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__parsi_zoroastrian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) reflects the asymmetric cost transfer: women marrying out bear total exclusion; men marrying out retain partial rights; children's status depends on father's lineage and priestly discretion. Suppression (0.78) is high because the constraint actively enforces exclusion — fire temple entry is physically controlled, trust benefits are contractually tied to community rolls, and priestly certification is a monopoly. Theater ratio (0.25) is moderate: the survival narrative is genuine but increasingly performs a ritual that masks demographic self-strangulation. Accessibility collapse (0.65) reflects that once the endogamy rule is internalized, alternatives (reform, civil marriage, diaspora exit) are cognitively and socially collapsed. Resistance (0.45) is moderate: court challenges (Goolrokh Gupta, 2017), reform movements, and diaspora attrition exist but have not shifted the Anuman's position.
 *
 * PERSPECTIVAL GAP:
 *   From the Anuman/priesthood seat, the constraint is a rope: genuine coordination solving a real collective-action problem (community survival). From the intermarried women's seat, it is a snare: pure extraction with no coordination benefit — the community they are excluded from is the very one that claims to need them for survival. The engine will compute this divergence from the declared power/exit/role structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The Anuman and priesthood are structural beneficiaries (d ~ 0.15-0.25): they collect ritual fees, control trust assets, and their authority is constituted by the boundary they enforce. Intermarried individuals, especially women, are full targets (d ~ 0.9): they bear irreversible costs with no structural recourse. Youth seeking exogamy are constrained targets (d ~ 0.7): they face a forced choice but retain some agency (can leave, can litigate). Secular courts are analytical observers (d ~ 0.5): they experience the constraint as a legal puzzle, not a lived extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (survival under persecution) is contested: reformists argue it is dead (the threat is now the boundary itself); traditionalists argue it is live (assimilation = extinction). The constraint persists despite demographic evidence that endogamy accelerates decline — a mandatrophy signal. The priesthood and Anuman extract status and material benefit from maintaining the boundary, creating a self-reinforcing loop where the coordination function's failure (demographic collapse) is cited as justification for intensifying the extraction function (stricter enforcement).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogamy_survival_causality,
    'Does endogamy actually cause community survival, or does it accelerate demographic collapse in modern conditions?',
    'Counterfactual demographic modeling: compare Parsi trajectory with other small endogamous communities that relaxed boundaries (e.g., Bene Israel, Syrian Christians) vs. those that maintained them (e.g., Samaritans). Longitudinal data on intermarried couples'' children''s identification.',
    'If endogamy accelerates collapse, the coordination claim is falsified and the constraint reclassifies toward snare. If endogamy is necessary for survival, the tangled_rope classification holds with coordination as the dominant function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogamy_survival_causality, empirical, 'Causal direction between endogamy enforcement and community survival.').

omega_variable(
    gender_asymmetry_legitimacy,
    'Is the gender-asymmetric penalty (women totally excluded, men partially retained) a theological necessity or a patriarchal accretion?',
    'Textual-historical analysis of Parsi rivayats (traditions) and Zoroastrian Middle Persian texts; comparison with Iranian Zoroastrian practice (where intermarriage rules differ). Theological debate within the priesthood.',
    'If theological necessity, the asymmetry is part of the coordination logic (however contested). If patriarchal accretion, it is pure extraction layered onto the coordination function — strengthening the snare component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_legitimacy, conceptual, 'Whether gender asymmetry is doctrinally grounded or socially constructed.').

omega_variable(
    priestly_monopoly_alternatives,
    'Could the community''s ritual validity function be performed without hereditary priestly monopoly (e.g., elected lay councils, women mobeds)?',
    'Iranian Zoroastrian community has experimented with women priests and lay ritual leadership. Diaspora communities (North America, Europe) use elected trustees. Comparative institutional analysis.',
    'If alternatives exist, the priestly monopoly is extractive rent-seeking. If the monopoly is structurally necessary for ritual validity (orthopraxy claim), it is part of the coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_monopoly_alternatives, empirical, 'Whether priestly gatekeeping is functionally necessary or rent-extracting.').

omega_variable(
    kernel_framing_ambiguity,
    'Is this constraint a reading of the family_law_authority kernel (shared commitment to marriage-as-status-governance) or a distinct constraint with only surface similarity to other religious marriage laws?',
    'Structural comparison: do the sibling readings share a common ε-invariant core (marriage as status-creating act governed by non-state authority) or are they structurally disjoint constraints that happen to share a label?',
    'If shared kernel, network.affects_constraints links are warranted and cross-reading contamination analysis applies. If disjoint, this is a standalone constraint and kernel framing is a category error.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the family_law_authority kernel is a genuine structural unity or a taxonomic imposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__parsi_zoroastrian_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_tr_t1900, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_tr_t1947, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1947, 0.12).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_tr_t1970, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_tr_t1990, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_tr_t2000, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_tr_t2010, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_tr_t2024, family_law_authority__parsi_zoroastrian_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_be_t1900, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_be_t1947, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1947, 0.42).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_be_t1970, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_be_t1990, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_be_t2000, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_be_t2010, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_be_t2024, family_law_authority__parsi_zoroastrian_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_su_t1900, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_su_t1947, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1947, 0.68).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_su_t1970, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_su_t1990, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 1990, 0.73).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_su_t2000, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_su_t2010, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(family_law_authority__parsi_zoroastrian_reading_su_t2024, family_law_authority__parsi_zoroastrian_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__parsi_zoroastrian_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__parsi_zoroastrian_reading, 0.08).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__christian_canonical_reading).
narrative_ontology:affects_constraint(family_law_authority__parsi_zoroastrian_reading, family_law_authority__secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This reading decomposes the 'family law authority' label into a structurally distinct constraint: the Parsi reading's unique fusion of endogamy-as-survival, priestly ritual monopoly, and gender-asymmetric penalties produces an ε (0.62) substantially higher than the secular_contractual_reading (ε ~ 0.15) and differently structured from the hindu_dharmashastra_reading (caste-endogamous, not priestly-monopoly). The kernel family is linked by the shared claim that marriage creates status via non-state authority, but each reading's extraction profile differs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(family_law_authority__parsi_zoroastrian_reading, organized, 0.2).
constraint_indexing:directionality_override(family_law_authority__parsi_zoroastrian_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
