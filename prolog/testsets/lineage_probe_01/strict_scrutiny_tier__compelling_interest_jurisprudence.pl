% ============================================================================
% CONSTRAINT STORY: strict_scrutiny_tier__compelling_interest_jurisprudence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strict_scrutiny_tier__compelling_interest_jurisprudence, []).

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
 *   constraint_id: strict_scrutiny_tier__compelling_interest_jurisprudence
 *   human_readable: Strict Scrutiny Tier: Compelling Interest Jurisprudence
 *   domain: constitutional_law/equal_protection
 *
 * SUMMARY:
 *   The strict scrutiny tier's compelling interest doctrine operates as a
 *   gating mechanism for race-conscious government action: once a
 *   classification is identified as racial, the government must justify it by
 *   reference to a 'compelling governmental interest' and demonstrate that
 *   the means are narrowly tailored to achieve that interest. The tier's
 *   functional architecture depends critically on what counts as compelling.
 *   The jurisprudence has established a short approved list: remedying
 *   identified past discrimination (the primary enumerated interest) and, in
 *   educational contexts, achieving educational diversity (a more contested
 *   enumeration that recent doctrine has begun to retract). Purposes outside
 *   this list — administrative convenience, political representation,
 *   distributional fairness — face near-certain invalidation. The constraint
 *   exhibits the structure of a tangled rope: it provides a legitimate
 *   framework for race-conscious allocation (coordination function) while
 *   simultaneously constraining which purposes can justify that allocation
 *   (extraction function). Institutions pursuing enumerated purposes gain
 *   doctrinal authorization; institutions pursuing anything else face
 *   suppression. The tier's extractiveness has risen over four decades as the
 *   Supreme Court has tightened the enumeration, retracted the diversity
 *   interest, and shifted toward the 'fatal in fact' trajectory where strict
 *   scrutiny increasingly functions as a near-absolute prohibition on
 *   race-consciousness. The theater ratio is moderate: strict scrutiny
 *   appears to be genuine doctrinal scrutiny (lower theater than rational
 *   basis), but the work is substantially done by the enumerated interests
 *   list rather than genuine case-by-case evaluation.
 *
 * KEY AGENTS:
 *   - Supreme Court: Institutional authority (institutional/arbitrage) — defines what counts as compelling, benefits from doctrinal clarity and authority, net beneficiary of the tier
 *   - Educational Institution Pursuing Enumerated Purpose: Organized institutional actor (organized/constrained) — gains authorization to use race-consciousness for enumerated purposes but constrained by narrow tailoring and shifting doctrinal boundaries
 *   - Institution Pursuing Unenumerated Purpose: Institutional actor (powerless/trapped) — faces near-certain invalidation regardless of how carefully tailored the program; no exit within the doctrine
 *   - Individual Applicant (Disfavored by Race-Conscious Program): Moderate power actor (moderate/mobile) — bears a cost from race-conscious remediation but has some structural mobility through legal challenge; constraint is mixed coordination/extraction
 *   - Lower Court System: Institutional actor (institutional/constrained) — must apply the tier's doctrine but operates within a pre-set list of permissible interests; high theater, low functional discretion
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating the doctrine as a constitutional necessity rather than a constructed institutional framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strict_scrutiny_tier__compelling_interest_jurisprudence, 0.58).
domain_priors:suppression_score(strict_scrutiny_tier__compelling_interest_jurisprudence, 0.65).
domain_priors:theater_ratio(strict_scrutiny_tier__compelling_interest_jurisprudence, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strict_scrutiny_tier__compelling_interest_jurisprudence, extractiveness, 0.58).
narrative_ontology:constraint_metric(strict_scrutiny_tier__compelling_interest_jurisprudence, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(strict_scrutiny_tier__compelling_interest_jurisprudence, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strict_scrutiny_tier__compelling_interest_jurisprudence, tangled_rope).
narrative_ontology:human_readable(strict_scrutiny_tier__compelling_interest_jurisprudence, "Strict Scrutiny Tier: Compelling Interest Jurisprudence").
narrative_ontology:topic_domain(strict_scrutiny_tier__compelling_interest_jurisprudence, "constitutional_law/equal_protection").

domain_priors:requires_active_enforcement(strict_scrutiny_tier__compelling_interest_jurisprudence).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(strict_scrutiny_tier__compelling_interest_jurisprudence, 'a7a92097-cc1a-48bf-82e3-ebf2023e11ec').
narrative_ontology:cs_kernel_codification('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', formalized).
narrative_ontology:cs_authority_grounding('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', extraction).
narrative_ontology:cs_interpretation_layer_present('a7a92097-cc1a-48bf-82e3-ebf2023e11ec').
narrative_ontology:cs_reading_relation('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', strict_scrutiny_tier__fatal_in_fact_trajectory, influences).
narrative_ontology:cs_reading_relation('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', strict_scrutiny_tier__narrow_tailoring_mechanics, coexists_with).
narrative_ontology:cs_axiom('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', foundational, enumerated_interests_doctrine).
narrative_ontology:cs_axiom_status(enumerated_interests_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', enumerated_interests_doctrine, deontological).
narrative_ontology:cs_axiom('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', foundational, remedial_discrimination_is_compelling).
narrative_ontology:cs_axiom_status(remedial_discrimination_is_compelling, holdable).
narrative_ontology:cs_axiom_grounding('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', remedial_discrimination_is_compelling, empirically_contingent).
narrative_ontology:cs_reference_frame('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', enumerated_compelling_interests_canon).
narrative_ontology:cs_drift_state('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', contemporary_post_sffa, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a7a92097-cc1a-48bf-82e3-ebf2023e11ec', '').
narrative_ontology:cs_kernel_id(strict_scrutiny_tier__compelling_interest_jurisprudence, strict_scrutiny_tier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strict_scrutiny_tier__compelling_interest_jurisprudence, remedial_discrimination_interests).
narrative_ontology:constraint_beneficiary(strict_scrutiny_tier__compelling_interest_jurisprudence, supreme_court_doctrine_authority).
narrative_ontology:constraint_victim(strict_scrutiny_tier__compelling_interest_jurisprudence, non_enumerated_classification_purposes).
narrative_ontology:constraint_victim(strict_scrutiny_tier__compelling_interest_jurisprudence, institutional_autonomy_in_racial_allocation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTION PURSUING UNENUMERATED PURPOSE (SNARE) — An institution attempting race-conscious allocation for any purpose outside the Supreme Court's approved list faces near-certain invalidation. No exit: the tier permits no alternatives once classification hits strict scrutiny. Maximum extraction: the institution's own judgment about compelling interest is irrelevant; only the Court's list counts. The suppression is total — alternatives are foreclosed not by resource constraints but by doctrinal decree.
constraint_indexing:constraint_classification(strict_scrutiny_tier__compelling_interest_jurisprudence, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTION WITH ENUMERATED PURPOSE (TANGLED ROPE) — An institution pursuing remedial discrimination or educational diversity (the two enumerated compelling interests) gains legitimate authorization to use race-conscious allocation. But the authorization is hedged: narrow tailoring still applies, and the tier's doctrine creates extraction asymmetry. The institution benefits from being on the approved list (coordination function: doctrine provides a path for race-conscious action) but bears suppression costs (must prove narrow tailoring, faces hostile judicial scrutiny, constrained by the Court's shifting definitions of enumerated purposes). Mixed coordination and extraction.
constraint_indexing:constraint_classification(strict_scrutiny_tier__compelling_interest_jurisprudence, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SUPREME COURT DOCTRINE AUTHORITY (ROPE) — The Court experiences the tier as pure coordination: it provides a stable framework for adjudicating equal protection claims. The tier's hidden ledger — the enumerated list of compelling interests — serves the Court's institutional need to maintain consistent doctrine while preserving predictability. The extraction runs toward the Court: it gains authority, doctrinal coherence, and the ability to distinguish permissible from impermissible race-consciousness. Net beneficiary; experiences the constraint as legitimate doctrinal architecture.
constraint_indexing:constraint_classification(strict_scrutiny_tier__compelling_interest_jurisprudence, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INDIVIDUAL APPLICANT SUBJECT TO RACE-CONSCIOUS ALLOCATION (TANGLED ROPE) — An applicant from a disfavored group bears a cost (potential disadvantage in allocation) from race-conscious remediation. But the tier's justification is that this cost serves a compelling interest (remedying past discrimination or achieving educational diversity). The applicant has some structural mobility: they can challenge the allocation in court, appeal, or pursue alternatives. But they are also constrained: the tier permits their disadvantage if the institution meets the compelling interest test. Mixed extraction (bearing a disadvantage) and coordination (the tier's doctrine permits institutions to pursue legitimate remedial ends). The applicant's experience depends heavily on whether the enumerated interest is seen as legitimately compelling.
constraint_indexing:constraint_classification(strict_scrutiny_tier__compelling_interest_jurisprudence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LOWER COURTS AND ADMINISTRATIVE BODIES (PITON) — Lower courts and administrative actors apply the tier's doctrine with diminishing functional authority. The theater is high: they must perform strict scrutiny while operating within a pre-set list of permissible interests. The functional constraint has eroded: courts increasingly treat the tier as a constraint that will ultimately invalidate most race-conscious programs (a trend toward the 'fatal in fact' trajectory). The performance persists (courts must apply strict scrutiny) but the outcome is increasingly predictable and constraining. Theater ratio reflects the ritualistic application of doctrine whose direction is substantively determined by the enumerated interests list rather than genuine scrutiny.
constraint_indexing:constraint_classification(strict_scrutiny_tier__compelling_interest_jurisprudence, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the tier embodies an immutable principle: equal protection law requires that any racial classification, even for remedial purposes, must serve a truly compelling governmental interest. This reads the doctrine as a fixed constitutional requirement, not a contingent institutional arrangement. However, this reading masks the constructed nature of the 'compelling interest' list itself — which interests count as compelling is not derived from constitutional text but from judicial pronouncements. The engine's false summit detector will flag this as naturalization of a doctrine that is actually a contestable framework for adjudicating competing values.
constraint_indexing:constraint_classification(strict_scrutiny_tier__compelling_interest_jurisprudence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strict_scrutiny_tier__compelling_interest_jurisprudence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strict_scrutiny_tier__compelling_interest_jurisprudence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strict_scrutiny_tier__compelling_interest_jurisprudence, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strict_scrutiny_tier__compelling_interest_jurisprudence, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strict_scrutiny_tier__compelling_interest_jurisprudence, TR),
    TR >= 0.70.

:- end_tests(strict_scrutiny_tier__compelling_interest_jurisprudence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over time. The tier's extractiveness is driven by the asymmetry between enumerated and unenumerated purposes. An institution can use race-consciousness for remedial discrimination or educational diversity; anything else is essentially prohibited. The enumerated list is short and has been contracting (diversity interest is being retracted). The measurement trajectory shows rising extractiveness from 0.35 to 0.58 over four decades as the Court has tightened the enumeration and begun to question whether even diversity is truly compelling. This trajectory mirrors the 'fatal in fact' phenomenon — the tier increasingly functions as a categorical prohibition rather than a genuine balancing test. Suppression (0.65): High. Institutions pursuing unenumerated purposes face near-total suppression — no alternatives exist within the strict scrutiny framework. Even institutions pursuing enumerated purposes face significant suppression: they must prove their interest is truly compelling (not just plausible), demonstrate that race-consciousness is necessary (race-neutral alternatives must be exhausted), and show that the program is narrowly tailored to the specific interest. The suppression is reinforced by hostile judicial scrutiny and the predictability that most programs will be invalidated. Theater ratio (0.48): Moderate. The tier involves genuine doctrinal scrutiny (higher than rational basis, where almost everything passes), but the outcome is substantially predetermined by the enumerated interests list. The scrutiny of narrow tailoring is real, but the enumerated interests list does much of the work. As the tier has evolved, the theater has increased slightly: courts perform strict scrutiny while the underlying direction (prohibition of race-consciousness except in narrow cases) is increasingly predictable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows a stark perspectival gap between beneficiaries and victims of the enumerated interests list. An institution on the approved list (remedial discrimination) sees the tier as providing legitimate authorization and stable doctrine (Tangled Rope) — coordination function is real, but extraction is present through narrow tailoring and shifting boundaries. An institution off the approved list sees near-certain invalidation (Snare) — no legitimate path for race-consciousness exists, and suppression is total. The Supreme Court experiences the tier as pure doctrine (Rope) — it provides a stable framework for adjudication. Lower courts apply it as increasingly ritualistic (Piton) — the outcome is predictable even though the form is rigorous. Individual applicants bear costs that the doctrine justifies as serving a compelling interest (Tangled Rope) — mixed coordination/extraction from their perspective. The analytical observer risks treating the tier as a constitutional necessity (Mountain) — a 'natural law' of equal protection — but the constraint's structure reveals this as a false summit. The enumerated interests list is not derived from constitutional text but from judicial pronouncements, and it serves the Supreme Court's institutional authority. The perspectival gap between the beneficiary's (Court's) Rope experience and the off-list institution's Snare experience reveals the tier's extractive asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from the agent's structural relationship to the enumerated interests list. An institution on the approved list for remedial discrimination is a beneficiary (low d, negative/low χ) — it gains authorization to use race-consciousness. An institution pursuing an unenumerated purpose is a victim (high d, high χ) — it faces suppression regardless of the merit of its purpose. The Supreme Court is an institutional beneficiary (low d) — the tier serves its authority. Individual applicants subject to race-conscious allocation are complex: they benefit from the coordination function (a legitimate framework exists) but bear costs from the program's effect on them. The analytical observer at the civilizational level is at risk of identity lock with the natural law framing, treating the doctrine as immutable when it is actually contestable. The engine will compute directionality from beneficiary/victim declarations; the key structural insight is that the enumerated interests list creates asymmetry — what counts as compelling is not neutral but benefits some purposes (remedial discrimination) and constrains others (administrative convenience, political representation). This asymmetry is the source of the extractiveness.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compelling_interest_enumeration_authority,
    'What makes an interest ''compelling''? Is the list of enumerated purposes (remedying past discrimination, educational diversity, etc.) derived from constitutional text, historical practice, or judicial discretion?',
    'Textual analysis of Equal Protection Clause; historical tracing of which interests the Court has recognized as compelling across decades; comparison with other doctrinal tiers (rational basis, intermediate scrutiny) to identify whether the enumeration is unique to strict scrutiny or reflects a general pattern.',
    'If derived from text or historical practice: the enumeration is relatively stable and the tier functions as doctrine. If derived from judicial discretion: the enumeration is revisable and the tier is more extractive (the Court''s power to define what counts as compelling is a form of extraction). If unique to strict scrutiny: the tier is genuinely distinctive; if mirrors other tiers: the tier is less of a meaningful gating mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compelling_interest_enumeration_authority, conceptual, 'Whether compelling interest enumeration derives from constitutional text, practice, or judicial discretion').

omega_variable(
    diversity_interest_doctrinal_status,
    'Is ''educational diversity'' genuinely one of the tier''s enumerated compelling interests, or is it a softening exception that the Court has begun to retract?',
    'Doctrinal analysis of diversity cases (Gratz, Grutter, SFFA); comparison of how the Court treats diversity interest in strict scrutiny vs. the Court''s contemporary language about diversity as potentially non-compelling or insufficiently concrete.',
    'If diversity is genuinely enumerated: institutions have a stable category for race-conscious allocation. If diversity is being retracted: the enumerated list is shrinking and the tier is becoming more restrictive. This affects whether the tier''s extractiveness is stable or rising.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diversity_interest_doctrinal_status, empirical, 'Whether educational diversity remains enumerated as compelling interest').

omega_variable(
    remedial_discrimination_temporal_scope,
    'How distant in time can discrimination be and still count as ''identified'' for purposes of the remedial compelling interest? Does the remedy need to target the specific victims of identified discrimination, or can it serve broader remedial goals?',
    'Doctrinal analysis of remedial discrimination cases; analysis of how courts have defined ''identified discrimination'' (institutional discrimination vs. societal discrimination); examination of whether narrow tailoring to identified victims is a secondary constraint or a primary gating mechanism.',
    'If temporal scope is broad and targeting can be diffuse: the remedial interest is a flexible category and more programs qualify. If temporal scope is narrow and targeting must be precise: the remedial interest is a tight constraint and fewer programs qualify. This significantly affects the tier''s functioning as a gating mechanism vs. a legitimating authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_discrimination_temporal_scope, empirical, 'Temporal and targeting scope of remedial discrimination interest').

omega_variable(
    reading_vs_sibling_kernels,
    'Is this reading (compelling interest enumeration) the primary gating mechanism for strict scrutiny, or is the functional work done by the narrow tailoring doctrine (sibling reading) regardless of compelling interest?',
    'Empirical analysis of Supreme Court decisions: count how many programs are invalidated at the compelling interest stage vs. the narrow tailoring stage. Compare invalidation rates and decision rationales across decades.',
    'If compelling interest is the primary gate: this reading''s extractiveness is high and the tier''s functional work is done here. If narrow tailoring is primary gate: this reading''s extractiveness is moderate and it serves as a legitimating authority that permits the narrow tailoring analysis to do the real work. This affects the interpretation of the constraint''s structure and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_kernels, empirical, 'Whether compelling interest or narrow tailoring is the primary gating mechanism in strict scrutiny').

omega_variable(
    false_summit_candidate,
    'Is the tier''s enumeration of compelling interests a genuine constitutional requirement (''natural law'') or a constructed doctrine that benefits the Supreme Court''s institutional authority and neutralizes democratic experiments with race-consciousness?',
    'Jurisprudential analysis: does the enumeration follow from textual, historical, or principled constitutional reasoning, or is it an institutional assertion? Compare with other jurisdictions'' approaches to race-consciousness and equal protection. Examine whether the tier constrains institutions symmetrically or asymmetrically.',
    'If natural law: the mountain classification is correct and the tier is immutable. If constructed: the mountain classification is a false summit and the tier is a snare or tangled rope, depending on how narrowly the enumeration constrains race-conscious allocation. This is the primary omega for FSM detection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_candidate, conceptual, 'Whether strict scrutiny compelling interest enumeration is constitutional requirement or constructed doctrine').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strict_scrutiny_tier__compelling_interest_jurisprudence, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stri_tr_t0, strict_scrutiny_tier__compelling_interest_jurisprudence, theater_ratio, 0, 0.42).
narrative_ontology:measurement(stri_tr_t20, strict_scrutiny_tier__compelling_interest_jurisprudence, theater_ratio, 20, 0.45).
narrative_ontology:measurement(stri_tr_t40, strict_scrutiny_tier__compelling_interest_jurisprudence, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(stri_be_t0, strict_scrutiny_tier__compelling_interest_jurisprudence, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stri_be_t20, strict_scrutiny_tier__compelling_interest_jurisprudence, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(stri_be_t40, strict_scrutiny_tier__compelling_interest_jurisprudence, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(stri_su_t0, strict_scrutiny_tier__compelling_interest_jurisprudence, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(stri_su_t20, strict_scrutiny_tier__compelling_interest_jurisprudence, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(stri_su_t40, strict_scrutiny_tier__compelling_interest_jurisprudence, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strict_scrutiny_tier__compelling_interest_jurisprudence, enforcement_mechanism).
narrative_ontology:affects_constraint(strict_scrutiny_tier__compelling_interest_jurisprudence, strict_scrutiny_tier__fatal_in_fact_trajectory).
narrative_ontology:affects_constraint(strict_scrutiny_tier__compelling_interest_jurisprudence, strict_scrutiny_tier__narrow_tailoring_mechanics).
narrative_ontology:affects_constraint(strict_scrutiny_tier__compelling_interest_jurisprudence, equal_protection_rational_basis_tier).
narrative_ontology:affects_constraint(strict_scrutiny_tier__compelling_interest_jurisprudence, equal_protection_intermediate_scrutiny_tier).

% DUAL FORMULATION NOTE:
% The strict scrutiny tier decomposes into three structurally distinct constraints: (1) Compelling interest enumeration (this reading) — what purposes count as compelling; (2) Fatal in fact trajectory — whether strict scrutiny permits race-consciousness at all; (3) Narrow tailoring mechanics — whether the fit between means and ends is sufficiently tight. Each has its own extractiveness value, its own mechanism, and its own perspectival structure. The enumeration (this reading, ε=0.58) establishes a gating mechanism. The trajectory (ε=0.72) shows how that mechanism has tightened over time. The tailoring mechanics (ε=0.55) show how the tier functions operationally. The three readings are linked: the enumeration determines which interests can be pursued; the trajectory shows whether that permission is real or illusory; the tailoring mechanics show where programs actually fail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
