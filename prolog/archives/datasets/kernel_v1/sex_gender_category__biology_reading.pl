% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex/Gender Category Membership via Immutable Reproductive Biology
 *   domain: social_ontology/identity_politics/legal_classification
 *
 * SUMMARY:
 *   This constraint models sex/gender category membership as determined by
 *   immutable reproductive biology (chromosomes, anatomy at birth). This is
 *   the BIOLOGY READING of a contested kernel — one of three competing
 *   readings of how sex/gender categories should be constituted. The biology
 *   reading is the institutional/legal baseline in most jurisdictions,
 *   grounding legal sex classification in birth documentation and
 *   medical/biological diagnosis. The constraint exhibits tangled rope
 *   characteristics: it genuinely coordinates sex-based harm prevention
 *   (single-sex spaces, reproductive rights, sex-disaggregated health data)
 *   while simultaneously extracting through boundary enforcement labor,
 *   suppression of alternative readings (trans recognition, intersex
 *   visibility), and forced categorization of individuals who don't fit the
 *   binary. The theater ratio (0.68) reflects the increasing performative
 *   character of the 'objective biology' claim — modern reproductive biology
 *   is far more complex than binary anatomy, but institutions maintain the
 *   simplification because it serves administrative and political functions.
 *   Enforcement costs rise over the interval (suppression 0.55 → 0.72) as
 *   rival readings gain institutional and social visibility, requiring
 *   intensified suppression to maintain the biology reading's dominance.
 *
 * KEY AGENTS:
 *   - Trans Women: Structurally excluded from 'woman' category under this reading (powerless/trapped) — bear maximum extraction cost with no exit within the framework
 *   - Intersex Individuals: Forced into binary through medical and legal protocols (powerless/trapped) — the constraint's enforcement mechanism includes active modification of biology to eliminate ambiguity
 *   - Cis Women: Experience genuine coordination (sex-based harm recognition) but also extraction (boundary policing labor, alliance-blocking, ontological control) (moderate/constrained) — benefit from the category's stability but pay costs of defending its boundaries
 *   - Institutional Sex Classification Systems: Birth certificates, legal documents, census categories (institutional/arbitrage) — benefit from administrative simplicity and legal defensibility of biology-based boundary; no suppression experienced
 *   - Medical/Scientific Authority: Claims to ground categories in 'objective biology' but increasingly operates performatively — maintains the constraint through professional certification of simplified models (institutional/arbitrage) — degraded role (piton perspective)
 *   - Rival Reading Proponents: Identity-reading and hybrid-reading communities whose institutional/epistemic power is required to sustain alternatives (analytical/constrained) — suppression is devoted to containing these alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.58).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex/Gender Category Membership via Immutable Reproductive Biology").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social_ontology/identity_politics/legal_classification").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, '46cdc5d6-044b-474c-ae5f-ee6115ad0ad4').
narrative_ontology:cs_kernel_codification('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', formalized).
narrative_ontology:cs_authority_grounding('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', extraction).
narrative_ontology:cs_interpretation_layer_present('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4').
narrative_ontology:cs_reading_relation('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', sex_gender_category__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', foundational, reproductive_biology_determines_social_category).
narrative_ontology:cs_axiom_status(reproductive_biology_determines_social_category, holdable).
narrative_ontology:cs_axiom_grounding('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', reproductive_biology_determines_social_category, empirically_contingent).
narrative_ontology:cs_axiom('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', secondary, binary_sex_categories_exhaust_biological_variation).
narrative_ontology:cs_axiom_status(binary_sex_categories_exhaust_biological_variation, holdable).
narrative_ontology:cs_axiom_grounding('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', binary_sex_categories_exhaust_biological_variation, empirically_contingent).
narrative_ontology:cs_reference_frame('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', biological_essentialism).
narrative_ontology:cs_drift_state('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', contemporary_trans_visibility_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('46cdc5d6-044b-474c-ae5f-ee6115ad0ad4', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_category_gatekeepers).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, institutional_sex_classification_systems).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, sex_essentialist_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANS WOMEN (SNARE) — Structurally excluded from the 'woman' category under biology reading; no exit option within the constraint's framework. Bear extraction costs (legal subordination, institutional barrier to services, social stigma) with no exit capacity. Maximum structural powerlessness — the constraint is designed to produce their exclusion.
constraint_indexing:constraint_classification(sex_gender_category__biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERSEX INDIVIDUALS (SNARE) — The biology reading requires binary classification (chromosome/anatomy at birth must map to one of two categories). Intersex individuals face forced assignment, medical protocols to 'resolve' ambiguity, and no choice in categorization. Trapped — the constraint's enforcement mechanism includes medical intervention designed to eliminate the exit option of ambiguity.
constraint_indexing:constraint_classification(sex_gender_category__biology_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CIS WOMEN (TANGLED ROPE) — The constraint genuinely coordinates sex-based harm prevention (single-sex spaces, reproductive rights frameworks, sex-disaggregated data for health disparities). But it also extracts: boundary enforcement labor (policing who counts as 'woman'), coalition-blocking (alliance with trans women on shared gender oppression is suppressed), and ontological control (the category's meaning is fixed externally rather than through women's collective self-determination). Constrained exit — leaving the category is legally difficult, and defecting to recognize trans women carries social/institutional costs.
constraint_indexing:constraint_classification(sex_gender_category__biology_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL SEX CLASSIFICATION SYSTEMS (ROPE) — Birth certificates, driver licenses, census categories: these institutions benefit from a fixed, observable, legally defensible category boundary. The biology reading provides arbitrage for institutional actors: it is administratively simple ( 'check the birth certificate'), politically defensible ('natural/immutable'), and produces low coordination costs relative to alternative readings. No suppression experienced — the constraint works in their favor.
constraint_indexing:constraint_classification(sex_gender_category__biology_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDICAL/SCIENTIFIC AUTHORITY (PITON) — The constraint claims to ground categories in reproductive biology 'objectively discovered.' But modern sex biology is far more complex than binary reproductive anatomy — chromosomes, hormones, gonads, external genitalia, and secondary sex characteristics do not always align; intersex conditions are not rare; epigenetic and environmental factors shape developmental outcomes. The appeal to 'biology determines category' is substantially performative — the authority certifies a simplified model because the institutional/legal system needs a binary. The scientific community knows this is incomplete, but institutional inertia (the constraint persists because it works, not because the biological claim is robust) maintains the theater.
constraint_indexing:constraint_classification(sex_gender_category__biology_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, this reading instantiates a commitment to biological essentialism: the claim that reproductive biology is an immutable, discoverable fact that should determine social category membership. The observer sees both genuine coordination (sex-based harm recognition) and extraction (boundary enforcement, trans exclusion, intersex medicalization). The classification as tangled_rope reflects that the reading's core commitment — to ground category in immutable biology — both solves coordination problems (stable, observable, legally defensible categories) AND creates asymmetric extraction (those who don't fit the binary bear compulsory medical and social costs).
constraint_indexing:constraint_classification(sex_gender_category__biology_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sex_gender_category__biology_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sex_gender_category__biology_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sex_gender_category__biology_reading, TR),
    TR >= 0.70.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): This reading extracts significantly through boundary enforcement (who counts as woman, trans woman, intersex person), exclusion of trans women from the category, forced categorization of intersex individuals into binary, and suppression of rival readings. The extraction is not total (snare-level) because the constraint does coordinate sex-based harm recognition — cis women genuinely benefit from shared category space for advocacy. The intermediate value reflects the tangled character: real coordination function + asymmetric extraction. Suppression (0.72): High and rising. The constraint requires active enforcement: legal gatekeeping (sex classification change is difficult), medical protocols (intersex 'correction'), social policing (delegitimization of trans recognition and intersex visibility), and institutional investment in defending the reading against rivals. The rise over the interval reflects intensifying suppression as alternative readings gain traction — institutional power must be devoted to containing the contest itself. Theater ratio (0.68): Rising. The appeal to 'objective reproductive biology' is increasingly performative. Modern biology shows chromosomal, hormonal, anatomical, and developmental variation that does not constrain to a clean binary. Intersex individuals exist. Institutional actors maintain the binary simplification because it serves administrative and political functions, not because the biological claim is robust. Medical/scientific authority certifies a simplified model, performing 'objectivity' while suppressing biological complexity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival differentiation. Trans women and intersex individuals perceive Snare (pure extraction with no exit). Cis women perceive Tangled Rope (genuine coordination of sex-based harm + extraction of boundary enforcement labor). Institutional systems perceive Rope (pure coordination without extraction — the constraint works perfectly from their perspective). Medical authority perceives Piton (degraded role maintained through inertia — science certifies a simplification it knows is incomplete). The analytical observer perceives Tangled Rope (the reading coordinates sex-based harm prevention but extracts through suppression of rival readings and forced categorization of those who don't fit). The perspectival gap reveals that the constraint's classification is NOT about objective facts but about structural position — the same reproductive biology grounds different constraint types for different agents because the constraint's extraction mechanism targets some and benefits others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position relative to this specific constraint. Trans women and intersex individuals are full targets (d ≈ 0.95): structurally designed to be excluded or suppressed, no exit capacity within the framework. Cis women as a collective occupy a mixed position (d ≈ 0.50): they benefit from the category's stability and coordination function but pay costs of boundary enforcement labor and alliance-blocking, creating genuine asymmetry. Institutional sex classification systems are full beneficiaries (d ≈ 0.05): administrative simplicity, legal defensibility, low coordination costs. Medical/scientific authority (d ≈ 0.15): derives authority from certifying the biological claim, but derives suppression cost from mounting scientific challenges to the binary. These directionality values feed into the sigmoid f(d) to produce experienced effective extractiveness chi for each perspective. Beneficiaries with arbitrage options (institutional systems, medical authority) experience low or negative chi; victims with no exit (trans women, intersex) experience maximum chi; cis women experience moderate chi reflecting mixed position.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the biology reading is one reading of a contested kernel, not a fact about biology itself. The mandate ('sex category membership should be determined by reproductive biology') coexists with rival mandates ('determined by identity' and 'determined by hybrid process'). The tangled rope classification emerges from the biology reading's dual character: it genuinely coordinates sex-based harm prevention (coordination function is real), but does so while extracting through boundary enforcement and suppression of alternatives (extraction mechanism is real). The classification does not depend on resolving which reading is 'correct' — all three readings are live institutional/epistemic positions. The constraint's extractiveness rises over the interval (0.42 → 0.58) as suppression intensity increases to contain rival readings. The rising theater ratio reflects that the biological claim becomes more performative as biological complexity is suppressed. The tangled rope classification is stable across the interval because both coordination and extraction mechanisms are structural to the reading, not contingent on external factors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_determinism_scope,
    'Does the claim ''reproductive biology determines category membership'' refer to a pure biological fact or a claim about what social institutions should recognize?',
    'Distinguish between (a) descriptive claim: reproductive biology is one dimension of human variation; (b) normative claim: social institutions SHOULD organize around reproductive categories exclusively; (c) essentialist claim: reproductive biology is the sole legitimate basis for sex category. Historical analysis of when and why institutions adopted biology-based categories reveals that the choice was institutional and political, not biologically mandated.',
    'If purely descriptive: biology reading is about one observable dimension among many, not THE axis of categorization. If normative/essentialist: reading makes a political commitment that biology determines social meaning. The constraint''s classification changes depending on whether biology is treated as determinative (tangled_rope with asymmetric extraction) or merely informative (rope with low extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_determinism_scope, conceptual, 'Whether ''reproductive biology determines category'' is descriptive fact or normative/essentialist commitment').

omega_variable(
    intersex_ontological_status,
    'Are intersex variations (chromosomal, hormonal, anatomical) errors/disorders to be resolved into binarity, or are they legitimate category members requiring multi-valued classification?',
    'Medical/ethical analysis: comparison of outcomes under forced-assignment vs. self-determination paradigms; documentation of harms from ''corrective'' medical interventions; examination of whether binary categories are actually necessary for sex-based harm prevention (could disaggregated data or multiply-valued categories serve the same epistemic function?).',
    'If intersex are legitimate members: binary is inadequate, the biology reading forecloses itself (cannot claim both that biology determines category AND that biology is always binary). If intersex are disorders: high enforcement costs to maintain the binary (medical intervention on infants/children is required). Either way, the reading''s claim to ''immutable biology'' becomes contested — either biology doesn''t constrain to binary, or maintaining the binary requires active modification of biology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intersex_ontological_status, empirical, 'Ontological status of intersex individuals under biology-based categorization').

omega_variable(
    sex_based_harm_prevention_coverage,
    'Does the biology-based category boundary actually correspond to the set of people who experience sex-based harms, or does it produce both false negatives (cis men who need reproductive health services) and false positives (trans women who do not)?',
    'Empirical analysis of sex-based harm distributions (reproductive coercion, domestic violence, sexual assault, labor exploitation): who experiences these harms, and does the biology-based category accurately predict vulnerability? Comparison to alternative category boundaries (self-identification, social position, historical marginalization).',
    'If biology-based category poorly tracks actual harm distributions: the constraint''s coordination function is weaker than it appears (theater rises, extractiveness may change). If biology tracks harm well: the coordination function is genuine, but the reading still faces the intersex problem (boundary enforcement costs remain high). The tang between coordination and extraction may shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_based_harm_prevention_coverage, empirical, 'Whether biology-based categories accurately predict sex-based harm vulnerability').

omega_variable(
    enforcement_mechanism_required,
    'What level of institutional enforcement is actually required to maintain the biology-based boundary against drift or challenge?',
    'Historical analysis of boundary enforcement costs over time: legal change (how hard is it to change sex classification on documents?), medical gatekeeping (do institutions require proof of biology?), social policing (is the category self-enforcing through culture, or does it require active suppression of alternatives?), resistance movements (how much suppression is devoted to containing trans recognition, intersex visibility, etc.?).',
    'If boundary enforcement is low and self-maintaining: the constraint approaches Rope (coordination with minimal coercion). If high and requires active institutional investment: the constraint is Snare or Tangled Rope depending on coordination function. The suppression metric (0.72) assumes high enforcement; empirical evidence of the actual enforcement intensity could revise this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_required, empirical, 'Institutional enforcement intensity required to maintain biology-based category boundary').

omega_variable(
    kernel_reading_contest_framing,
    'This constraint is one reading of the contested kernel ''sex/gender category membership.'' What distinguishes this biology reading from its sibling readings (identity reading, hybrid reading), and where does the contest originate?',
    'Structural analysis: The readings differ in where they locate the source of truth about category membership. Biology reading: external, immutable, discoverable at birth. Identity reading: internal, subjective, self-determined. Hybrid reading: combination (biology + social transition process). The contest originates in competing commitments about legitimacy: should categories be grounded in observable facts (biology), subjective experience (identity), or institutional process (gatekeeping)? These are not empirically resolvable disagreements — they are commitments to different framings of what counts as legitimate evidence.',
    'Recognizing the kernel contest reveals that all three readings coexist as live positions in contemporary discourse. No single reading is ''correct'' across all contexts. The biology reading is the institutional/historical baseline, but the contest itself is what produces the constraint''s high suppression (0.72) — institutional power is devoted to defending biology reading against rival readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_framing, conceptual, 'Nature and origins of the kernel contest between three readings of sex/gender category').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__biology_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__biology_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__biology_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__biology_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__biology_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__biology_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_essentialist_epistemic_commons).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, trans_institutional_access_barriers).

% DUAL FORMULATION NOTE:
% The sex/gender category kernel decomposes into three constraint stories, one per reading. All three stories share base structure (how categories are constituted) but differ in ε values, beneficiary/victim sets, and classifications. The biology reading (this story) has ε=0.58, Tangled Rope. The identity reading has higher extractiveness (ε≈0.65+) due to greater boundary enforcement required to suppress the dominant biology-reading alternative. The hybrid reading has intermediate extractiveness reflecting its gatekeeping mechanism. All three affect downstream constraints (trans access, essentialist epistemics) and form a network of mutual influence. The contest between readings is itself a constraint with its own extractiveness (enforcing the dominance of any single reading requires suppression of alternatives).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
