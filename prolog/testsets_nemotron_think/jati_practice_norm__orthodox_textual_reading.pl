% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Scriptural Varna-Jati Boundary Enforcement
 *   domain: social/religious/political_economy
 *
 * SUMMARY:
 *   The orthodox textual reading of jati practice holds that caste boundaries
 *   derive from a fixed, eternal scriptural varna framework (Vedas,
 *   Manusmriti, Puranas) and that deviation constitutes ritual pollution
 *   threatening cosmic order. This reading instantiates a high-extraction
 *   snare: lower jatis and Dalits are assigned hereditary polluting
 *   occupations with blocked mobility, while dominant castes and Brahmin
 *   priesthood extract labor, surplus, and ritual authority. The authority
 *   structure (priesthood, dominant caste councils, temple institutions)
 *   benefits materially and symbolically from categorical rigidity. This is
 *   one reading of the contested kernel 'jati_practice_norm' — the other
 *   readings (localized_practice_reading, colonial_census_reading) offer
 *   structurally different accounts of the same social phenomenon.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.85).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.9).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Scriptural Varna-Jati Boundary Enforcement").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political_economy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '2774af36-ec01-40dd-9f47-9bb8d35cea76').
narrative_ontology:cs_kernel_codification('2774af36-ec01-40dd-9f47-9bb8d35cea76', fixed_text).
narrative_ontology:cs_authority_grounding('2774af36-ec01-40dd-9f47-9bb8d35cea76', lineage).
narrative_ontology:cs_interpretation_layer_present('2774af36-ec01-40dd-9f47-9bb8d35cea76').
narrative_ontology:cs_reading_relation('2774af36-ec01-40dd-9f47-9bb8d35cea76', jati_practice_norm__localized_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('2774af36-ec01-40dd-9f47-9bb8d35cea76', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('2774af36-ec01-40dd-9f47-9bb8d35cea76', foundational, varna_framework_is_eternal_and_fixed).
narrative_ontology:cs_axiom_status(varna_framework_is_eternal_and_fixed, holdable).
narrative_ontology:cs_axiom_grounding('2774af36-ec01-40dd-9f47-9bb8d35cea76', varna_framework_is_eternal_and_fixed, theological).
narrative_ontology:cs_axiom('2774af36-ec01-40dd-9f47-9bb8d35cea76', foundational, ritual_purity_is_moral_order).
narrative_ontology:cs_axiom_status(ritual_purity_is_moral_order, holdable).
narrative_ontology:cs_axiom_grounding('2774af36-ec01-40dd-9f47-9bb8d35cea76', ritual_purity_is_moral_order, theological).
narrative_ontology:cs_reference_frame('2774af36-ec01-40dd-9f47-9bb8d35cea76', scriptural_varna_eternal_order).
narrative_ontology:cs_drift_state('2774af36-ec01-40dd-9f47-9bb8d35cea76', contemporary_constitutional_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2774af36-ec01-40dd-9f47-9bb8d35cea76', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, dominant_caste_groups).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_priesthood).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, temple_institutions).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, lower_jati_groups).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, dalit_communities).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, scheduled_castes).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, varna_dharma_doctrine).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, ritual_purity_moral_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and enforce scriptural varna-jati boundaries; control temple entry, ritual performance, and purity certification; derive authority and material support from the constraint's categorical rigidity. Exit from this role requires abandoning priestly vocation and scriptural authority.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_priesthood, agenda_setter,
    institutional, generational, analytical, universal).

% Reserve clean occupations, ritual authority, land ownership, and political representation; extract labor and surplus from lower jatis through hereditary occupational assignment. Can partially exit by converting to egalitarian religions or migrating, but carry caste capital that persists across contexts.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dominant_caste_groups, beneficiary,
    powerful, generational, arbitrage, universal).

% Assigned hereditary service and artisanal occupations deemed ritually polluting; barred from temple entry, water sources, education, and public office; bear the labor cost of maintaining ritual purity for dominant castes. Exit is blocked by ascribed identity, economic dependency, and threat of violence.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, lower_jati_groups, payer,
    powerless, generational, trapped, universal).

% Placed outside the varna framework entirely as 'untouchable'; assigned the most polluting occupations (manual scavenging, dead animal disposal, cremation work); subjected to spatial segregation, physical violence, and ritual exclusion. Caste identity is internalized and socially enforced — conversion, migration, or economic success do not fully erase the stigma.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dalit_communities, payer,
    powerless, generational, identity_locked, universal).

% Legally recognized as historically oppressed groups entitled to reservations; nonetheless remain bound by the same occupational and ritual constraints in daily village life. The legal category coexists with the religious constraint — affirmative action operates within the frame the constraint creates, not outside it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, scheduled_castes, payer,
    powerless, generational, identity_locked, universal).

% Organize against the constraint through political mobilization, legal challenges, and counter-narratives (Ambedkarite Buddhism, Marxist frameworks, constitutionalism). Are structurally excluded from orthodox scriptural interpretation and traditional governance bodies. Face repression, co-optation, and epistemic dismissal.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, anti_caste_activists, excluded,
    organized, biographical, constrained, national).

% Adjudicate conflicts between constitutional equality (Articles 15, 17) and religious freedom (Article 25). Issue judgments that simultaneously uphold reservations and permit temple entry restrictions. Their rulings shape the constraint's legal enforcement but cannot dissolve its religious legitimation.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates social reproduction, occupational allocation, and ritual purity maintenance through fixed hereditary boundaries that assign each group a specific function in the cosmic-social order.
% TRANSFER_FUNCTION: Moves labor, surplus, and ritual status from lower jatis to dominant castes; assigns polluting occupations (sanitation, leather, cremation) to lower jatis while reserving ritual authority, education, land, and clean occupations for dominant castes.
% ABSENT_VOICES: Dalit and lower jati voices historically excluded from scriptural interpretation and social governance; contemporary anti-caste movements systematically marginalized in orthodox discourse; women within all jatis whose gendered labor and reproductive control are regulated by the same boundaries but who have no independent voice in the orthodox framework.
% DISAPPEARANCE_RATIONALE: The constraint structures the entire social ontology — occupation, marriage, dining, worship, political representation, and spatial organization. Its removal would collapse the hereditary occupational system, dissolve ritual pollution stigma, open temple entry and water access, and fundamentally reorganize the political economy of rural and urban India.
% FOUNDING_PROBLEM: To maintain cosmic and social order (rita/dharma) by fixing each group's ritual function and preventing pollution that threatens collective wellbeing; to provide a stable, hereditarily transmitted division of labor that ensures all necessary functions are performed without competition.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox authorities cite scriptural continuity (Vedas, Manusmriti, Dharmashastras) and guru-parampara lineage. Anti-caste historians (Ambedkar 'Who Were the Shudras?', Phule 'Gulamgiri') and colonial ethnographers (Risley, Crooke, Hutton) document the system's historical construction, functional shifts, and political instrumentalization. No independent corroboration exists outside beneficiary lineages for the 'cosmic order' claim — the founding problem is attested only by those who benefit from its persistence.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.85, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.85) because the constraint transfers the entire productive labor of polluting occupations to lower jatis while denying them the fruits; suppression is extreme (0.9) because enforcement combines religious sanction, social ostracism, economic coercion, and physical violence; theater ratio is moderate (0.4) because ritual performance (purity rites, festivals, temple ceremonies) is genuine coordination but increasingly serves as ideological cover for extraction. Accessibility collapse is near-total (0.9) — one cannot exit one's jati; resistance is significant (0.6) but heavily suppressed (Dalit movements, Bhakti traditions, constitutional challenges).
 *
 * PERSPECTIVAL GAP:
 *   From the priesthood seat, the constraint is genuine cosmic coordination maintaining rita/dharma. From the Dalit seat, it is pure extraction enforced by violence and ideology. From the dominant caste seat, it is a beneficial arrangement they defend. The engine computes this divergence from structural data — the authored claim (snare) reflects the analytical seat's assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin priesthood and dominant castes are structural beneficiaries (d near 0.0) — they collect ritual authority, land, and labor. Lower jatis and Dalits are full targets (d near 1.0) — they bear the extraction with trapped/identity_locked exit. Anti-caste activists are excluded (d undefined, not coordinated). Constitutional courts are analytical observers (d=0.5). The identity_locked exit for Dalits reflects the ascribed, inescapable nature of untouchability stigma — even conversion or migration leaves residual stigma.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (cosmic order through fixed ritual functions) is contested as dead by anti-caste movements and historians, but orthodox authorities maintain it is live. The constraint persists despite the founding problem's contested status because the authority structure extracts substantial benefit from categorical rigidity — this is mandatrophy unresolved: the mandate has outlived its coordination function (if it ever had one) but persists through extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'How does this reading''s claim of scriptural fixity relate to the sibling readings'' claims of local fluidity and colonial construction?',
    'Comparative historical analysis of textual prescriptions vs. ethnographic practice vs. colonial census categories across time; identify whether the readings describe different historical layers or competing contemporaneous framings.',
    'If the readings describe different historical layers (scriptural ideal → local practice → colonial fixation), they are not mutually exclusive. If they are competing contemporaneous framings, the orthodox reading''s claim to represent the kernel''s true nature is contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Structural relationship between this reading and its siblings in the kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (violence, legal bars, economic coercion) or internalized (Dalits believing they deserve pollution stigma, identity fusion with caste role)?',
    'Post-exit suppression trajectory: track Dalits who convert, migrate, or achieve economic mobility — if pollution stigma and self-exclusion persist, internalized suppression is significant.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after exit. This affects whether the constraint classifies as snare (structural) or has piton-like internalized maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in caste constraint').

omega_variable(
    extraction_coordination_boundary,
    'Does the varna-jati system perform any genuine coordination function (occupational specialization, social insurance, ritual scheduling) that is separable from its extractive core?',
    'Counterfactual analysis: in regions/castes where the constraint weakened (urban migration, conversion, legal reform), did coordination collapse or adapt? Compare with non-caste occupational guilds.',
    'If coordination is inseparable from extraction, the constraint is pure snare. If separable, part of the measured extraction may be the price of coordination, suggesting tangled_rope dynamics in some historical phases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_coordination_boundary, conceptual, 'Whether coordination and extraction are structurally separable in the varna-jati system').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_tr_t25, jati_practice_norm__orthodox_textual_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_tr_t50, jati_practice_norm__orthodox_textual_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_tr_t75, jati_practice_norm__orthodox_textual_reading, theater_ratio, 75, 0.4).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_tr_t100, jati_practice_norm__orthodox_textual_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_be_t25, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 25, 0.8).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_be_t50, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_be_t75, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 75, 0.84).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_be_t100, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_su_t25, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_su_t50, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 50, 0.9).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_su_t75, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 75, 0.9).
narrative_ontology:measurement(jati_practice_norm__orthodox_textual_reading_su_t100, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jati_practice_norm__orthodox_textual_reading, 0.08).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the colloquial 'caste system' into three structurally distinct readings of the same kernel 'jati_practice_norm'. The orthodox reading (this story) claims fixed scriptural origin and high extraction. The localized practice reading claims continuous renegotiation (lower extraction, rope-like). The colonial census reading claims administrative reification (scaffold/tangled_rope dynamics). Their ε values differ substantially: this reading ε≈0.85, localized ε≈0.3-0.4, colonial ε≈0.5-0.6 depending on period.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jati_practice_norm__orthodox_textual_reading, powerful, 0.15).
constraint_indexing:directionality_override(jati_practice_norm__orthodox_textual_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
