% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Fragmented Marriage Authority as Anti-Majoritarian Safeguard
 *   domain: legal_pluralism/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the federalist-millet reading of the
 *   marriage authority kernel: marriage and family law authority is
 *   deliberately fragmented across multiple legal systems (religious personal
 *   law codes + secular state law) as a constitutional mechanism to prevent
 *   majoritarian tyranny over religious minorities. The reading does NOT
 *   claim that all readings of marriage authority are compatible in a single
 *   framework — it asserts ONE coherent reading grounded in consociational
 *   anti-majoritarian theory. The structure benefits minority religious
 *   communities (who retain autonomy) and vindicates a federalist
 *   constitutional principle, while imposing coordination costs on the
 *   secular majority (legal fragmentation, inability to enact uniform civil
 *   law). Extraction is low because no single party is trapped; the
 *   arrangement is sustained by the political cost of changing it, not by
 *   coercion.
 *
 * KEY AGENTS:
 *   - Minority religious communities: retain personal law authority; beneficiaries of autonomy protection
 *   - Federalist constitutional order: the institutional structure itself; benefits from prevention of majoritarian dominance
 *   - Secular majority population: pays the cost of legal fragmentation; constrained by constitutional grid-lock
 *   - Women in minority communities: excluded from authority structures; absorb intra-community hierarchy without voice
 *   - Judiciary: maintains the consociational balance through constitutional interpretation
 *   - Legislative majority: holds electoral power but structurally unable to enact UCC without constitutional amendment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.28).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.15).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Fragmented Marriage Authority as Anti-Majoritarian Safeguard").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal_pluralism/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, '9c6ca073-1803-4b42-9372-dbbda81a3033').
narrative_ontology:cs_kernel_codification('9c6ca073-1803-4b42-9372-dbbda81a3033', formalized).
narrative_ontology:cs_authority_grounding('9c6ca073-1803-4b42-9372-dbbda81a3033', lineage).
narrative_ontology:cs_interpretation_layer_present('9c6ca073-1803-4b42-9372-dbbda81a3033').
narrative_ontology:cs_reading_relation('9c6ca073-1803-4b42-9372-dbbda81a3033', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c6ca073-1803-4b42-9372-dbbda81a3033', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c6ca073-1803-4b42-9372-dbbda81a3033', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('9c6ca073-1803-4b42-9372-dbbda81a3033', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('9c6ca073-1803-4b42-9372-dbbda81a3033', foundational, fragmentation_as_tyranny_prevention).
narrative_ontology:cs_axiom_status(fragmentation_as_tyranny_prevention, holdable).
narrative_ontology:cs_axiom_grounding('9c6ca073-1803-4b42-9372-dbbda81a3033', fragmentation_as_tyranny_prevention, deontological).
narrative_ontology:cs_axiom('9c6ca073-1803-4b42-9372-dbbda81a3033', secondary, minority_autonomy_precedes_equality).
narrative_ontology:cs_axiom_status(minority_autonomy_precedes_equality, holdable).
narrative_ontology:cs_axiom_grounding('9c6ca073-1803-4b42-9372-dbbda81a3033', minority_autonomy_precedes_equality, deontological).
narrative_ontology:cs_reference_frame('9c6ca073-1803-4b42-9372-dbbda81a3033', constitutional_federalism_as_anti_majoritarian_protection).
narrative_ontology:cs_drift_state('9c6ca073-1803-4b42-9372-dbbda81a3033', contemporary_pressure_for_ucc, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9c6ca073-1803-4b42-9372-dbbda81a3033', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_religious_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, federalist_constitutional_order).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.28) because the constraint is not primarily extractive — it is coordinative, solving a genuine collective-action problem (preventing tyranny) by fragmenting authority rather than centralizing it. The secular majority bears a real cost (legal fragmentation) but is not trapped; they retain the option of constitutional amendment (slow, difficult, but available). Suppression is minimal (0.15) because the arrangement persists through gridlock and constitutional design, not through active coercive suppression. Theater ratio is moderate (0.22) because the consociational framing is partly performative — the 'protection of minorities' language masks the real mechanism (making unification legislatively impossible) and grows more theatrical as demographic and political contexts shift. Accessibility_collapse is moderate (0.45) because alternatives exist theoretically (secular unification, individual secularization) but are blocked constitutionally and politically, not by economic or structural barriers. Resistance is present (0.38) because secular majorities periodically attempt UCC reforms, women within communities challenge personal law rules, and judicial expansion creeps toward equality floors — all meeting institutional resistance. Measurement series shows slight upward drift in extractiveness and theater from t=0 to t=55, then slight decline at t=75, tracking increased political pressure for unification (early period) and modest judicial harmonization success (late period) that slightly reduces the configuration's pure-rope character. The drift is authored at a single shared time grid so all metrics align.
 *
 * PERSPECTIVAL GAP:
 *   The federalist reading computes as rope from all seats, but the reasoning differs sharply: from the minority-community seat it is genuine coordination solving a real threat (majoritarian imposition). From the secular-majority seat it is a coordination problem solved by being prevented from solving it — a paradox the reading embraces (gridlock AS solution). From the women-excluded-seat it is a snare masquerading as rope: the coordination solves community autonomy but the internal extraction of women's equality is unaddressed. The engine computes per-seat types from structural data; the authored single-reading claim (rope) captures the reading's own logical structure while the metrics permit the divergence to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Minority communities sit near the full-beneficiary end (d ≈ 0.15): the constraint subsidizes their autonomy, they have mobile exit (can accept secular courts if they choose), and they actively benefit from the fragmentation. The secular majority sits near symmetric-to-target (d ≈ 0.55): they bear the cost of fragmentation, are constrained from unifying, but retain electoral and legislative power and theoretical constitutional-amendment exit. The federalist constitutional order sits at beneficiary (d ≈ 0.05): the arrangement IS the structure; it collects rents in the form of continued constitutional stability and prevention of majoritarian collapse. Women in minority communities sit near target (d ≈ 0.75): they bear the cost of intra-community hierarchy with no structural voice; their exit is constrained both by community pressure and by state law diversity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing majoritarian tyranny over minorities) remains live and contested. The federalist reading maintains that fragmented authority solves it; competing readings (secularist, gender-rights, judicial-harmonization) argue the problem has been superseded or that the solution is inadequate or harmful. The constraint avoids mandatrophy trap by remaining institutionally active: the judiciary continually interprets and maintains the consociational balance; legislative attempts at UCC continually test the constitutional boundaries; minority communities continually reassert autonomy claims. Mandatrophy would threaten if the founding problem were abandoned (all parties agreed secular unification was desirable) and the fragmented system persisted by inertia alone. The measurement evidence (slight oscillation, no monotonic drift toward theater dominance) supports the reading that the arrangement remains genuinely coordinative rather than degrading toward pure performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consociational_vs_communal_boundary,
    'Is the fragmentation of marriage authority grounded in federalist anti-majoritarian theory (consociational politics), or is it more fundamentally grounded in community autonomy rights independent of majority-protection rationale?',
    'Examine the constitutional history and textual grounding: Does the constitution explicitly invoke protection-of-minorities as the rationale, or does it invoke community self-determination? Compare the reading''s axioms with those of communal_autonomy_reading via the cs_structure.axioms field.',
    'If the foundation is purely consociational, the reading survives challenges to minority status (if minorities became majorities, the logic might flip). If the foundation is community autonomy, the arrangement is more stable but less clearly anti-majoritarian in principle. The two readings have substantial structural overlap and may collapse into one reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consociational_vs_communal_boundary, conceptual, 'Whether fragmentation is consociational (anti-majoritarian theory) or communal (autonomy theory).').

omega_variable(
    intra_community_extraction_visibility,
    'Does the consociational anti-majoritarian framing systematically obscure intra-community extraction (gender hierarchy, caste-based rules, minority-within-minority oppression) by treating community autonomy as a unitary good?',
    'Compare metrics and stakeholder analysis across this reading and the gender_rights_reading: assess whether women and intra-community minorities are identifiable victims in the personal law system despite this reading''s low-extraction frame.',
    'If the framing obscures substantial intra-community extraction, the constraint may function as a snare for internal minorities while appearing as rope to external majorities. The low base_extractiveness score assumes distribution of autonomy benefits within communities; if those benefits concentrate in community leadership and extract from women/minorities, true ε is higher. Suppression of intra-community voice (women absent from qadi/rabbinical councils) may be understated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intra_community_extraction_visibility, empirical, 'Whether fragmented authority masks intra-community gender and minority hierarchy as part of autonomy costs.').

omega_variable(
    gridlock_as_feature_vs_bug,
    'Is the legislative paralysis that prevents UCC unification a genuine feature of federalist design (majority protection), or is it a side effect of veto-player proliferation that happens to align with minority autonomy by accident?',
    'Historical analysis: was the fragmented authority deliberately adopted to prevent majoritarian legislation, or was it inherited from colonial-era legal pluralism and then constitutionalized post-hoc as a federalist principle?',
    'If design-intentional, the reading is correct and the arrangement is stable. If post-hoc rationalization, the consociational framing is partly theater and the arrangement is more vulnerable to reframing. The measurement series could track this: if the reading''s legitimacy erodes over time as players understand the fragmentation as historical accident rather than design, theater_ratio should trend upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gridlock_as_feature_vs_bug, empirical, 'Whether legislative paralysis is federalist design-feature or post-hoc rationalization of colonial inheritance.').

omega_variable(
    reading_contest_foreclosure_question,
    'Does the secularist_reading''s claim that personal law pluralism is a transitional anomaly logically foreclose the federalist-millet reading, or do the two readings coexist as competing constitutional frameworks held by different parties?',
    'Examine the cs_structure axioms of secularist_reading: if its foundational axiom asserts that authority belongs exclusively to democratic legislatures and pluralism must be eliminated, it forecloses the federalist axiom (authority fragmented as structural principle). If the secularist reading treats pluralism as transitional-but-tolerable, it coexists. The reading_relations in cs_structure should declare this relationship.',
    'If foreclosure holds, the two readings cannot be held within a single constitutional framework — one party would need to abandon its core claim. If coexistence holds, the readings are live competing positions whose relative strength depends on political power, not logical necessity. This affects the stability assessment of the constraint itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_foreclosure_question, conceptual, 'Whether secularist and federalist readings logically foreclose or coexist as competing constitutional frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__federalist_millet_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement(marr_tr_t25, marriage_authority__federalist_millet_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__federalist_millet_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(marr_tr_t55, marriage_authority__federalist_millet_reading, theater_ratio, 55, 0.24).
narrative_ontology:measurement(marr_tr_t75, marriage_authority__federalist_millet_reading, theater_ratio, 75, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(marr_be_t10, marriage_authority__federalist_millet_reading, base_extractiveness, 10, 0.24).
narrative_ontology:measurement(marr_be_t25, marriage_authority__federalist_millet_reading, base_extractiveness, 25, 0.27).
narrative_ontology:measurement(marr_be_t40, marriage_authority__federalist_millet_reading, base_extractiveness, 40, 0.29).
narrative_ontology:measurement(marr_be_t55, marriage_authority__federalist_millet_reading, base_extractiveness, 55, 0.3).
narrative_ontology:measurement(marr_be_t75, marriage_authority__federalist_millet_reading, base_extractiveness, 75, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority__federalist_millet_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(marriage_authority__federalist_millet_reading, 0.12).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the marriage_authority kernel. Each reading instantiates a different structural answer to 'who has legitimate authority over family law?' The federalist-millet reading treats fragmentation as anti-majoritarian constitutional design; the communal_autonomy_reading treats it as grounded in religious tradition (structural overlap but different normative framing); the secularist_reading treats pluralism as transitional anomaly; the gender_rights_reading challenges all community autonomy on gender-equality grounds; the judicial_harmonization_reading describes the constraint evolving via case-law constitutional floors. The five readings form a constraint family linked by network.affects_constraints. Each story has its own ε, beneficiary structure, and interpretation of the same kernel — the decomposition follows ε-invariance principle: the readings measure the kernel differently (one as federalist coordination, another as communal autonomy, another as transitional anomaly). Their divergence is the signal the corpus exists to capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
