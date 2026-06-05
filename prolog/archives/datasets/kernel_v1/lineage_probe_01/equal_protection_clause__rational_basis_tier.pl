% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__rational_basis_tier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__rational_basis_tier, []).

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
 *   constraint_id: equal_protection_clause__rational_basis_tier
 *   human_readable: Equal Protection Clause — Rational Basis Tier (Economic/Social Classifications)
 *   domain: constitutional_law/equal_protection
 *
 * SUMMARY:
 *   The rational basis tier of equal protection doctrine represents a
 *   specific judicial approach to constitutional review of ordinary economic
 *   and social classifications. Under rational basis review, a classification
 *   survives constitutional challenge if there is any conceivable rational
 *   relation between the classification and a legitimate state interest —
 *   even if the legislative record does not articulate such a relation and
 *   even if the hypothesized relation is speculative. This constraint
 *   exhibits a classic tangled-rope structure: it coordinates the division of
 *   authority between legislatures and courts (both share authority over
 *   classification validity) while simultaneously extracting from claimants
 *   subject to classifications that pass this deferential review. The
 *   constraint's structure is fundamentally contested — alternative doctrinal
 *   tiers (intermediate and strict scrutiny) represent different readings of
 *   the same constitutional kernel (the equal protection clause). The
 *   rational basis tier benefits state legislatures by granting near-plenary
 *   authority to classify on economic and social grounds; it extracts from
 *   non-suspect-class claimants by narrowing the judicial review available to
 *   them. The theater ratio (0.65) reflects that the rational basis standard
 *   is substantively thin: courts announce that they will review
 *   classifications for rational basis but in practice uphold nearly all
 *   challenged laws, rendering the actual judicial scrutiny largely
 *   performative.
 *
 * KEY AGENTS:
 *   - State Legislatures: Primary beneficiary (institutional/arbitrage) — receive near-plenary authority to classify on economic/social grounds with assurance of judicial upholding
 *   - Non-Suspect Class Claimants: Primary victim (powerless/trapped) — subject to classifications with minimal meaningful judicial review pathway; no alternative forum
 *   - Regulatory Agencies: Secondary beneficiary (institutional/arbitrage) — receive deference in applying rational basis classifications to regulated populations
 *   - Business Firms: Moderate victim (moderate/constrained) — subject to regulatory classifications with some ability to relocate or lobby but constrained by rational basis deference
 *   - Activist Organizations: Organized reformers (organized/constrained) — push for doctrinal expansion (reclassifying additional groups to heightened scrutiny) as alternative to pure rational basis
 *   - Federal Courts: Institutional interpreter (institutional/arbitrage) — maintain the rational basis tier as the default review standard; benefit from reduced caseload vs. heightened scrutiny
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent doctrinal choice as constitutional necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__rational_basis_tier, 0.52).
domain_priors:suppression_score(equal_protection_clause__rational_basis_tier, 0.28).
domain_priors:theater_ratio(equal_protection_clause__rational_basis_tier, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__rational_basis_tier, extractiveness, 0.52).
narrative_ontology:constraint_metric(equal_protection_clause__rational_basis_tier, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(equal_protection_clause__rational_basis_tier, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__rational_basis_tier, tangled_rope).
narrative_ontology:human_readable(equal_protection_clause__rational_basis_tier, "Equal Protection Clause — Rational Basis Tier (Economic/Social Classifications)").
narrative_ontology:topic_domain(equal_protection_clause__rational_basis_tier, "constitutional_law/equal_protection").

domain_priors:requires_active_enforcement(equal_protection_clause__rational_basis_tier).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__rational_basis_tier, 'd360f5ed-ff02-4932-81ed-75b85ee0a702').
narrative_ontology:cs_kernel_codification('d360f5ed-ff02-4932-81ed-75b85ee0a702', formalized).
narrative_ontology:cs_authority_grounding('d360f5ed-ff02-4932-81ed-75b85ee0a702', lineage).
narrative_ontology:cs_interpretation_layer_present('d360f5ed-ff02-4932-81ed-75b85ee0a702').
narrative_ontology:cs_reading_relation('d360f5ed-ff02-4932-81ed-75b85ee0a702', equal_protection_clause__intermediate_scrutiny_tier, coexists_with).
narrative_ontology:cs_reading_relation('d360f5ed-ff02-4932-81ed-75b85ee0a702', equal_protection_clause__strict_scrutiny_tier, coexists_with).
narrative_ontology:cs_axiom('d360f5ed-ff02-4932-81ed-75b85ee0a702', foundational, legislative_deference_ordinary_classifications).
narrative_ontology:cs_axiom_status(legislative_deference_ordinary_classifications, holdable).
narrative_ontology:cs_axiom_grounding('d360f5ed-ff02-4932-81ed-75b85ee0a702', legislative_deference_ordinary_classifications, deontological).
narrative_ontology:cs_axiom('d360f5ed-ff02-4932-81ed-75b85ee0a702', foundational, minimal_review_sufficient_for_ordinary_rights).
narrative_ontology:cs_axiom_status(minimal_review_sufficient_for_ordinary_rights, holdable).
narrative_ontology:cs_axiom_grounding('d360f5ed-ff02-4932-81ed-75b85ee0a702', minimal_review_sufficient_for_ordinary_rights, conventional).
narrative_ontology:cs_reference_frame('d360f5ed-ff02-4932-81ed-75b85ee0a702', legislative_primacy_on_ordinary_classifications).
narrative_ontology:cs_drift_state('d360f5ed-ff02-4932-81ed-75b85ee0a702', contemporary_post_obergefell, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d360f5ed-ff02-4932-81ed-75b85ee0a702', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(equal_protection_clause__rational_basis_tier, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__rational_basis_tier, state_legislatures).
narrative_ontology:constraint_beneficiary(equal_protection_clause__rational_basis_tier, regulatory_agencies).
narrative_ontology:constraint_victim(equal_protection_clause__rational_basis_tier, non_suspect_class_claimants).
narrative_ontology:constraint_victim(equal_protection_clause__rational_basis_tier, ordinary_economic_rights_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-SUSPECT CLASS CLAIMANT (SNARE) — Subject to legislative classification with nearly no judicial review. The rational basis tier offers no exit: any conceivable rational relation to a legitimate state interest permits the classification, even if speculative or post-hoc. The claimant bears extraction (regulatory burden, differential treatment) with suppression of alternatives (no meaningful judicial remedy). Maximum experienced extraction.
constraint_indexing:constraint_classification(equal_protection_clause__rational_basis_tier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE LEGISLATURE (ROPE) — Primary beneficiary. The rational basis tier grants nearly plenary authority to classify and regulate on economic and social grounds. The legislature experiences the constraint as coordination: it can announce its regulatory intent with near-certainty that courts will uphold the line-drawing. Minimal judicial interference. The constraint solves the collective action problem of 'what classification authority do legislatures have?' and the legislature experiences minimal extraction.
constraint_indexing:constraint_classification(equal_protection_clause__rational_basis_tier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: BUSINESS FIRM IN REGULATED MARKET (TANGLED ROPE) — Moderately constrained. Subject to rational-basis classifications (licensing rules, differential tax treatment, occupational restrictions) but with some ability to lobby for inclusion in preferred regulatory categories or to relocate to more favorable jurisdictions. Benefits from regulatory predictability and uniform application of rules; bears cost of classification differential. Mixed extraction and coordination.
constraint_indexing:constraint_classification(equal_protection_clause__rational_basis_tier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: REGULATORY AGENCIES (ROPE) — Secondary beneficiary. The rational basis tier grants broad deference to agency classifications and line-drawing. Agencies can classify people into regulatory categories with confidence that rational basis review will uphold the scheme. Experiences the constraint as pure coordination: the tier enables predictable rulemaking.
constraint_indexing:constraint_classification(equal_protection_clause__rational_basis_tier, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RATIONAL BASIS TIER RITUAL (PITON) — The judicial review process itself is largely performative. Courts apply rational basis with 'one rational basis only' language (the reviewing court may hypothesize any rational relation that could rationally have prompted the legislature to enact the statute), meaning courts will almost always find constitutional validity. The actual judicial work — scrutinizing means-relation to ends — is theater; the outcome (upholding the classification) is predetermined. Theater ratio reflects the gap between the professed standard (meaningful review) and the practiced standard (nearly none).
constraint_indexing:constraint_classification(equal_protection_clause__rational_basis_tier, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGISLATIVE OVERRIDE MOVEMENT (SCAFFOLD) — Organized reform advocates see the rational basis tier as a temporary constitutional arrangement, not immutable. Alternative doctrines (heightened scrutiny tiers applied to ordinary classifications affecting discrete vulnerable groups) or explicit statutory reclassification of certain categories (e.g., disability, sexual orientation) as suspect classes are creating pathways that bypass pure rational basis deference. These movements have a sunset logic: if sufficient state legislatures and the Supreme Court adopt heightened protection for additional categories, rational basis tier's scope contracts. Low chi because the movement has explicit agency and sees an exit path.
constraint_indexing:constraint_classification(equal_protection_clause__rational_basis_tier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INHERENT JUDICIAL SEPARATION (MOUNTAIN) — From the civilizational view, some institutional separation of legislative and judicial power is a fundamental feature of constitutional design. The rational basis tier appears as a structural necessity: if judges reviewed all legislative line-drawing with the same scrutiny applied to race-based classifications, the judiciary would occupy the legislative function. This perspective sees rational basis as a natural law of constitutional architecture. However, this classification is a false summit: the debate is not about whether judicial review exists but about HOW MUCH review is proportionate to the constitutional interest at stake. The 'inherent separation' framing naturalizes a specific doctrinal choice.
constraint_indexing:constraint_classification(equal_protection_clause__rational_basis_tier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__rational_basis_tier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equal_protection_clause__rational_basis_tier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equal_protection_clause__rational_basis_tier, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__rational_basis_tier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(equal_protection_clause__rational_basis_tier, TR),
    TR >= 0.70.

:- end_tests(equal_protection_clause__rational_basis_tier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The rational basis tier permits legislatures to extract significant regulatory burdens on non-suspect classes with minimal judicial check. The measurement trajectory (0.35 → 0.52 over 60 years) reflects accumulating extraction as legislatures have learned to phrase classifications in ways that satisfy the standard and as courts have narrowed the suspect-class category. The value 0.52 reflects that extraction is real and substantial but not total — states must still provide some rational relation (formal constraint), and heightened scrutiny tiers exist for protected classes (a partial escape valve). Suppression (0.28): Moderate-low. The tier does not physically prevent challenges or ban speech; it narrows the grounds on which challenges succeed. Alternative doctrinal pathways exist (strict and intermediate scrutiny, though narrowly applied). The suppression is real but not maximal — an activist lawyer can still file suit and obtain a formal hearing, though the outcome is nearly predetermined. Theater ratio (0.65): The rational basis standard is explicitly advertised as meaningful review ('rationally related to a legitimate state interest') but practiced as near-automatic upholding. The gap between the professed and practiced standards is the theater. Courts do occasionally invalidate under rational basis (rare cases with no real relation, purely irrational classifications), maintaining the appearance of scrutiny, but the effective rate of invalidation is near zero for ordinary economic classifications.
 *
 * PERSPECTIVAL GAP:
 *   The rational basis tier creates a systematic perspectival gap: the same constitutional principle (equal protection) produces radically different classifications depending on the observer's structural position. The legislature sees protection of democratic authority; the powerless claimant sees a blocked escape route. The judicial system sees a manageable default; the activist sees a ceiling on what courts will do. The two sibling readings (intermediate and strict scrutiny tiers) are not alternative measurements of the same constraint; they are separate constraints with different beneficiary/victim structures. This story captures the rational basis tier specifically — the doctrinal choice that benefits legislatures most and constrains courts most.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) is computed from the base extractiveness (0.52) scaled by directionality f(d) and scope σ. The powerless/trapped claimant has high d (close to 1.0, maximum target status) and experiences chi amplified upward; the institutional beneficiary has low d (close to 0.0, maximum beneficiary status) and experiences chi dampened toward zero or negative. The moderate business firm has mid-range d and experiences moderate chi. The organizational reform movement, though 'organized,' has constrained exit (limited ability to change doctrine unilaterally) and experiences moderate chi. The directionality overrides are empty — the structural derivation from beneficiary/victim declarations and exit options produces accurate d values without override.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rational_relation_circularity,
    'Does the rational basis standard''s ''hypothesize any rational relation'' instruction render the doctrine circular (a tautology where everything satisfies ''rationally related'' to some conceivable purpose)?',
    'Empirical analysis: catalog Supreme Court cases applying rational basis from 1970 onward; identify the rate at which challenged classifications are invalidated; compare to pre-rational basis jurisprudence (post-Civil Rights Act 1964 but pre-rational basis crystallization circa 1965–1970)',
    'If circularity confirmed (near 0% invalidation rate): extractiveness rises to 0.65+, closer to snare. If meaningful review exists (even rare): tangled_rope classification holds. If some classifications are invalidated on rational basis grounds (outside suspect class context): rope interpretation emerges.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rational_relation_circularity, empirical, 'Whether rational basis standard is circular or permits meaningful review').

omega_variable(
    alternative_tier_necessity,
    'Do intermediate and strict scrutiny tiers constitute necessary judicial checks on legislative power, or do they represent judicial usurpation of line-drawing authority that the Constitution delegates to legislatures?',
    'Originalist vs. living-constitutionalist textual analysis of ''equal protection''; comparative constitutional law examining how other democracies distribute classification authority between legislatures and courts; empirical examination of whether heightened scrutiny tiers have prevented democratic majorities from enacting classifications they deemed important',
    'If heightened tiers are necessary: rational basis tier alone provides insufficient protection, and extractiveness is underestimated (the true extraction is the cumulative effect of rational basis PLUS absence of meaningful heightened scrutiny). If heightened tiers are judicial overreach: rational basis tier is the proper default, and extractiveness should be lower (0.35–0.45 range). If both views hold within different frameworks: the omega documents the commitment-system contest itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_tier_necessity, conceptual, 'Whether heightened scrutiny tiers are necessary or constitute judicial overreach').

omega_variable(
    category_expansion_dynamics,
    'Are non-suspect classes (e.g., sexual orientation, disability) moving toward recognized intermediate or suspect status, and does the rational basis tier''s scope therefore contract over time?',
    'Longitudinal analysis of Supreme Court doctrine 1970–2026: track which classifications were subject to rational basis in 1970; track which were reclassified to heightened scrutiny; identify the pattern of doctrinal evolution; compare to predictions made in each decade',
    'If expansion is accelerating: rational basis tier is a degrading constraint (piton), and its scope will contract. If expansion has plateaued: rational basis tier is stable, and the scaffold perspective is aspirational rather than structural. If expansion is reversing: the rational basis tier is expanding, extractiveness rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_expansion_dynamics, empirical, 'Trajectory of category expansion from rational basis to heightened scrutiny').

omega_variable(
    kernel_contest_natural_law,
    'Is the rational basis tier (this reading of the equal protection kernel) a natural consequence of constitutional structure, or is it a specific doctrinal choice among alternatives (strict scrutiny tier, intermediate tier, ad-hoc balancing)?',
    'This omega documents the kernel contest itself. See commentary.kernel_context and cs_structure.reading_relations. The analytical observer''s mountain classification instantiates one answer (structural necessity); the scaffold and legislative override perspectives instantiate the alternative answer (contingent doctrinal arrangement). The engine''s false summit detector identifies this omega as the signal.',
    'If natural law: rational basis tier is immutable law of constitutional architecture. If contingent: rational basis tier is a doctrinal choice and a site of ongoing political contestation. This omega does not resolve the question — it documents it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_natural_law, conceptual, 'Whether rational basis tier is structurally necessary or a contingent doctrinal choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__rational_basis_tier, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epc_rational_tr_t0, equal_protection_clause__rational_basis_tier, theater_ratio, 0, 0.48).
narrative_ontology:measurement(epc_rational_tr_t25, equal_protection_clause__rational_basis_tier, theater_ratio, 25, 0.62).
narrative_ontology:measurement(epc_rational_tr_t60, equal_protection_clause__rational_basis_tier, theater_ratio, 60, 0.65).

% Extraction over time
narrative_ontology:measurement(epc_rational_be_t0, equal_protection_clause__rational_basis_tier, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(epc_rational_be_t25, equal_protection_clause__rational_basis_tier, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(epc_rational_be_t60, equal_protection_clause__rational_basis_tier, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__rational_basis_tier, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__rational_basis_tier, intermediate_scrutiny_tier).
narrative_ontology:affects_constraint(equal_protection_clause__rational_basis_tier, strict_scrutiny_tier).

% DUAL FORMULATION NOTE:
% The three tiers of equal protection review (rational basis, intermediate, strict scrutiny) form a constraint family decomposed by the level of scrutiny applied. Each tier is a separate constraint story with its own ε (extractiveness), beneficiary/victim structure, and doctrinal implications. The rational basis tier represents the minimal-review endpoint; strict scrutiny represents the maximal-review endpoint; intermediate sits between. They are linked by network.affects_constraints because changes in doctrinal doctrine (e.g., reclassification of sexual orientation to intermediate scrutiny) shift the scope and application of the rational basis tier itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
