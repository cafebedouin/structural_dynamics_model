% ============================================================================
% CONSTRAINT STORY: future_dsm_integration_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_future_dsm_integration_2026, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: future_dsm_integration_2026
 *   human_readable: Future DSM Strategic Vision (SCE-DoH & Intersectionality Integration)
 *   domain: technological/political/psychiatric_nosology
 *
 * SUMMARY:
 *   The integration of socioeconomic and cultural determinants (SCE-DoH) and
 *   intersectionality into the DSM-5 revision process represents a structural
 *   tension between genuinely progressive intent (reducing psychiatric
 *   pathologization of social conditions) and institutional expansion logic
 *   (extending psychiatric authority into the social/economic domain,
 *   creating new billable diagnostic categories, and deepening pharmaceutical
 *   market segmentation). The constraint exhibits all six DR types from
 *   different structural positions. Marginalized individuals diagnosed under
 *   expanded DSM criteria experience maximum extraction (trapped with new
 *   pathology labels). The abstract epistemic requirement for diagnostic
 *   coherence bears the cost of category proliferation. Clinical
 *   practitioners face mixed coordination and extraction (shared diagnostic
 *   language but increased documentation burden and score inflation).
 *   Institutional psychiatry and the pharmaceutical industry experience pure
 *   coordination benefits (expanded markets, extended authority). The
 *   anti-psychiatry coalition sees a temporary institution degrading under
 *   its own contradictions (Scaffold with sunset). The categorical DSM itself
 *   persists as a Piton — maintained through institutional inertia and
 *   legal/insurance lock-in despite fundamental epistemic incoherence. The
 *   theater ratio (0.65) reflects that the SCE-DoH integration involves
 *   extensive committee work, stakeholder engagement, and performative
 *   commitment to cultural sensitivity while the underlying categorical
 *   system remains incoherent and extraction mechanisms are legitimized
 *   rather than reduced.
 *
 * KEY AGENTS:
 *   - Marginalized individuals and communities: Primary victims (powerless/trapped) — experience new diagnostic labels that pathologize their social/economic context without addressing structural barriers to care
 *   - Diagnostic validity and nosological coherence: Primary victim (epistemic commons, powerless/trapped) — bears cost of category proliferation and boundary incoherence
 *   - Clinical practitioners: Secondary victim (moderate/constrained) — benefit from coordination (shared language) but bear extraction (documentation burden, score inflation, obligation to assign intersectionality codes)
 *   - Institutional psychiatry (APA/DSM stewardship committee): Primary beneficiary (institutional/arbitrage) — captures benefits of expanded diagnostic authority and billable categories
 *   - Pharmaceutical industry: Secondary beneficiary (institutional/arbitrage) — captures market expansion through new medication-treatable categories
 *   - Anti-psychiatry and neurodiversity coalition: Organized actor (organized/constrained) — perceives DSM expansion as temporary arrangement with sunset trajectory; building alternative diagnostic frameworks
 *   - DSM as categorical nosological system: Degraded institution (institutional/arbitrage, Piton) — persists through institutional lock-in despite epistemic incoherence
 *   - Internal DSM reformers: Organized reformers (organized/mobile) — genuinely motivated to reduce categorical harm but captured by expansion logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(future_dsm_integration_2026, 0.52).
domain_priors:suppression_score(future_dsm_integration_2026, 0.58).
domain_priors:theater_ratio(future_dsm_integration_2026, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(future_dsm_integration_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(future_dsm_integration_2026, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(future_dsm_integration_2026, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(future_dsm_integration_2026, tangled_rope).
narrative_ontology:human_readable(future_dsm_integration_2026, "Future DSM Strategic Vision (SCE-DoH & Intersectionality Integration)").
narrative_ontology:topic_domain(future_dsm_integration_2026, "technological/political/psychiatric_nosology").

domain_priors:requires_active_enforcement(future_dsm_integration_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(future_dsm_integration_2026, institutional_psychiatry).
narrative_ontology:constraint_beneficiary(future_dsm_integration_2026, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(future_dsm_integration_2026, dsm_stewardship_committee).
narrative_ontology:constraint_victim(future_dsm_integration_2026, marginalized_communities).
narrative_ontology:constraint_victim(future_dsm_integration_2026, diagnostic_validity).
narrative_ontology:constraint_victim(future_dsm_integration_2026, nosological_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED COMMUNITY MEMBER (SNARE) — Trapped within the DSM framework with no exit. New intersectionality metrics may label them differently, creating additional diagnostic/social stigma layers without reducing underlying structural barriers to care. Experiences maximum extraction: their lived conditions become data points for institutional legitimacy while resources remain concentrated in psychiatric gatekeeping institutions.
constraint_indexing:constraint_classification(future_dsm_integration_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NOSOLOGICAL COHERENCE / DIAGNOSTIC VALIDITY (SNARE) — The abstract epistemic requirement for valid, coherent diagnostic categories cannot exit or organize. Faces extraction through category proliferation, metric inflation, and definitional scope creep driven by institutional and pharmaceutical interests. Bears the full cost of diagnostic fragmentation while coordination benefits (if any) accrue elsewhere.
constraint_indexing:constraint_classification(future_dsm_integration_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CLINICAL PRACTITIONER (TANGLED ROPE) — Constrained by mandated DSM compliance for billing, licensing, and institutional practice. Benefits from the DSM as a coordination tool (shared diagnostic language, research database) but bears extraction through increased documentation burden, score inflation, and the obligation to assign intersectionality codes that may pathologize social context rather than genuine disorder.
constraint_indexing:constraint_classification(future_dsm_integration_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INSTITUTIONAL PSYCHIATRY / APA STEWARDSHIP (ROPE) — Primary beneficiary. Experiences DSM revision as pure coordination: expanded diagnostic criteria increase billable encounters, extend pharmaceutical markets, and legitimize psychiatric authority over social/economic determinants. Can exit (ignore recommendations) but chooses not to — arbitrage position is stable. Net beneficiary from DSM expansion.
constraint_indexing:constraint_classification(future_dsm_integration_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PHARMACEUTICAL INDUSTRY (ROPE) — Secondary beneficiary. SCE-DoH and intersectionality integration expand the diagnostic envelope, creating new medication-treatable categories and justifying pharmacological intervention for conditions previously framed as social/economic. Can arbitrage between definitions (support expansionist definitions that increase disease prevalence). Experiences the constraint as coordinating mechanism: DSM expansion = market expansion.
constraint_indexing:constraint_classification(future_dsm_integration_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANTI-PSYCHIATRY & NEURODIVERSITY MOVEMENTS (SCAFFOLD) — Organized agents (disability advocates, peer support networks, critical psychiatry scholars) perceive the DSM integration effort as a temporary institutional arrangement with sunset logic. The expansion of social/environmental categories creates tactical openings: intersectionality metrics can be weaponized to reveal that 'disorders' are actually policy failures, and the expanded scope increases pressure on the framework until it becomes internally incoherent (Goodhart collapse). Coalition builds parallel diagnostic systems (neurodiversity-affirming frameworks, social model alternatives) that will supersede DSM pathology-framing within a generational timeframe.
constraint_indexing:constraint_classification(future_dsm_integration_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: CATEGORICAL NOSOLOGY ITSELF (PITON) — The DSM as an institution persists through inertia despite fundamental epistemic incoherence (continuous trait distributions cannot be carved into discrete disorders; social/economic conditions are not diseases). The SCE-DoH expansion adds performative legitimacy ('we are now capturing social context') without addressing the underlying incoherence. Theater ratio high: extensive committee meetings, intersectionality code development, and stakeholder engagement create the appearance of scientific rigor while the fundamental category system degrades. The framework is maintained by institutional lock-in (insurance coding, legal liability, research funding), not by actual diagnostic utility.
constraint_indexing:constraint_classification(future_dsm_integration_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: INTERNAL DSM REFORM ADVOCATES (TANGLED ROPE) — Organized reformers within academic psychiatry seek to improve the DSM by adding intersectionality and SCE-DoH. Experience mixed coordination and extraction: genuinely motivated to reduce categorical harm and improve cultural sensitivity, but their reform efforts are captured by the institutional expansion logic — each new intersectionality metric increases institutional control and pharmaceutical market segmentation. They benefit from the coordination (shared language, research databases) but also unwittingly enable extraction (expanding psychiatric authority over social determinants). Their exit option is 'mobile' but incomplete — they can leave psychiatry but find it difficult to abandon the categories they've invested in developing.
constraint_indexing:constraint_classification(future_dsm_integration_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL / CIVILIZATIONAL VIEW (MOUNTAIN — FALSE SUMMIT) — From a civilizational perspective, one might argue that the difficulty of integrating social/economic determinants into a categorical disease taxonomy is an immutable feature of how language-based classification systems work: categories always exclude, and boundary-drawing always naturalizes socially contingent groupings. However, this perspective risks a false summit — it naturalizes what is actually a policy choice. Dimensional, network, or social-model alternatives exist; the constraint to use categorical DSM nomenclature is enforced, not inherent.
constraint_indexing:constraint_classification(future_dsm_integration_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(future_dsm_integration_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(future_dsm_integration_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(future_dsm_integration_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(future_dsm_integration_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(future_dsm_integration_2026, TR),
    TR >= 0.70.

:- end_tests(future_dsm_integration_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The SCE-DoH and intersectionality integration genuinely expands psychiatric diagnostic scope and billable categories. The expansion creates new extraction opportunities through (a) psychiatric authority over social/economic determinants, (b) pharmaceutical market segmentation via new diagnoses, and (c) increased documentation/coding burden for clinical practitioners. However, the extraction is not maximal (0.70+) because the reformed categories may also enable genuine benefits (improved cultural sensitivity, reduced misdiagnosis in minority populations) — the constraint is hybrid, not pure. Suppression (0.58): Moderate-high. Significant barriers to exit from the DSM framework include legal requirement for diagnostic coding (insurance/liability), institutional dependence on DSM taxonomy (research databases, training), and the absence of mature alternatives until recently. But suppression is not total — alternatives are emerging (neurodiversity frameworks, social model, dimensional systems), and organized resistance exists. Theater ratio (0.65): Moderate-high. The integration process involves extensive committee meetings, stakeholder consultation, and performative engagement with intersectionality and cultural sensitivity — all of which create legitimacy and appearance of rigor while the underlying categorical incoherence remains. Theater has increased over the revision interval as the committee expands DSM scope while claiming to reduce categorical harm.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the DSM revision simultaneously serves as a Rope (coordination mechanism) for institutional psychiatry and pharmaceutical industry, a Snare (pure extraction) for marginalized individuals and diagnostic validity itself, a Tangled Rope (mixed) for practitioners and internal reformers, and a Scaffold (temporary with sunset) for the anti-psychiatry coalition. This is not ambiguity in the constraint — it is accurate perspectival diversity reflecting real structural asymmetries. The gap is widest between the institutional beneficiaries (who see DSM expansion as straightforward coordination) and the powerless victims (who experience new diagnostic labels that pathologize their social context). The gap reveals that the reform intent of internal advocates is structurally insufficient to prevent extraction, because institutional incentives align with expansion rather than reduction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position relative to the extraction flow. Marginalized individuals and communities: victimized, trapped, powerless → d ≈ 0.95 → maximum f(d) → maximum experienced extraction. Institutional psychiatry: beneficiary, arbitrage exit option, institutional power → d ≈ 0.05 → negative f(d) → they experience the constraint as coordination benefit flowing toward them. Practitioners: moderate power, constrained exit (must use DSM for billing), partly beneficiary (coordination) and partly victim (extraction) → d ≈ 0.50 → moderate f(d) → tangled rope experience. The pharmaceutical industry: secondary beneficiary, arbitrage (can choose to develop drugs for new categories or not) → d ≈ 0.10 → negative f(d) → pure benefit. The coalition: organized, mobile (can develop alternatives), partly victim (extraction from continued DSM dominance) and partly actor (can build exit pathways) → d ≈ 0.45 → moderate f(d) → scaffold experience. Directive overrides: None declared. The structural derivation captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED at extractiveness = 0.52. The constraint does NOT resolve mandatrophy by claiming to be a pure coordination mechanism (Rope). Institutional psychiatry and pharmaceutical industry experience the constraint as Rope (net benefit from diagnostic expansion), but marginalized individuals experience Snare (maximum extraction), practitioners experience Tangled Rope (mixed), the coalition experiences Scaffold (temporary), and the DSM itself appears as Piton (degraded). The mandatrophy is resolved by recognizing that the constraint IS a hybrid (Tangled Rope at base_properties.claimed_type), not by claiming pure coordination. The integration of SCE-DoH and intersectionality is simultaneously: (1) a genuine reform effort to reduce categorical harm and improve cultural competence (coordination function), AND (2) an institutional expansion mechanism that extends psychiatric authority over social/economic domains and enables pharmaceutical market segmentation (extraction function). Both are structurally true. The mandate trap is that internal reformers cannot pursue (1) without enabling (2), because the institutional incentives are aligned. This is not a flaw in the reformers' intent but a structural feature of how institutional power operates. The classification resolves the mandatrophy by naming this hybrid accurately: Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dsm_expansion_vs_reform_intent,
    'Does the addition of SCE-DoH and intersectionality codes genuinely reduce psychiatric pathologization of social context, or does it systematize and expand it?',
    'Longitudinal analysis of diagnostic prevalence, medication prescription rates, and documented harms to intersectionality-coded groups over 10-year post-DSM-revision interval; comparison with pre-revision baseline and alternative diagnostic frameworks',
    'If reform intent realized: constraint shifts to Scaffold (sunset as alternative frameworks mature). If expansion intent realized: constraint remains Tangled Rope or Snare (extraction mechanism legitimized, not reduced).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dsm_expansion_vs_reform_intent, empirical, 'Whether SCE-DoH integration reduces or expands psychiatric pathologization').

omega_variable(
    categorical_vs_dimensional_fundamental,
    'Is the constraint fundamentally an institutional power structure (Snare/Tangled Rope) or a natural property of categorical classification systems (Mountain)?',
    'Comparative analysis of diagnostic systems using dimensional, network, or social-model alternatives (e.g., ICD-11 dimensional variants, NIMH RDoC framework, neurodiversity-affirming frameworks); assessment of whether these alternatives solve the extraction problem or merely relocate it',
    'If institutional structure: constraint can be reformed/replaced by moving to dimensional or social-model systems. If natural law: expansion of intersectionality codes is inevitable regardless of intent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(categorical_vs_dimensional_fundamental, conceptual, 'Whether the constraint is institutional power structure or fundamental property of categorical systems').

omega_variable(
    pharmaceutical_capture_degree,
    'To what extent do pharmaceutical industry incentives drive the expansion of DSM categories, particularly in the SCE-DoH integration?',
    'Financial analysis of pharmaceutical industry lobbying, funding of DSM committee research, and off-label marketing strategies targeted at new intersectionality codes; correlation with medication prescription rates in newly-defined diagnostic categories',
    'If capture is strong: extraction mechanism is pharmaceutical-driven (Snare with secondary beneficiary). If capture is weak: institutional psychiatry expansion is autonomous (Tangled Rope with primary benefit to institutional stewardship).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_capture_degree, empirical, 'Degree of pharmaceutical industry capture of DSM expansion').

omega_variable(
    coalition_exit_pathway_maturity,
    'How mature and viable are the alternative diagnostic frameworks (neurodiversity-affirming, social model, dimensional systems) as genuine replacements for DSM pathology-framing?',
    'Assessment of adoption rates in clinical practice, insurance coverage, research citation impact, and training program integration; timeline analysis for each alternative framework''s reach critical mass',
    'If mature: Scaffold sunset is realistic, constraint will degrade to Piton as DSM loses functional utility. If immature: coalition pathway is aspirational, DSM extraction remains entrenched for decades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_exit_pathway_maturity, empirical, 'Maturity and viability of alternative diagnostic frameworks as DSM replacements').

omega_variable(
    intersectionality_metric_validity,
    'Can intersectionality codes be operationalized as valid diagnostic modifiers, or do they function as social labeling that pathologizes structural conditions?',
    'Psychometric validation studies of proposed intersectionality codes; analysis of whether codes predict treatment outcomes, prognosis, or biological markers (true diagnostic validity) or merely correlate with demographic categories (social classification)',
    'If valid: codes improve diagnostic precision and clinical utility (moves toward Rope). If invalid: codes are performative window-dressing that increases institutional control (confirms Tangled Rope / Snare extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersectionality_metric_validity, empirical, 'Whether intersectionality codes constitute valid diagnostic modifiers or social labeling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(future_dsm_integration_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsm_int_tr_t0, future_dsm_integration_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(dsm_int_tr_t5, future_dsm_integration_2026, theater_ratio, 5, 0.58).
narrative_ontology:measurement(dsm_int_tr_t10, future_dsm_integration_2026, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(dsm_int_be_t0, future_dsm_integration_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dsm_int_be_t5, future_dsm_integration_2026, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(dsm_int_be_t10, future_dsm_integration_2026, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(future_dsm_integration_2026, information_standard).
narrative_ontology:affects_constraint(future_dsm_integration_2026, psychiatric_nosology_institutional_lock_in).
narrative_ontology:affects_constraint(future_dsm_integration_2026, pharmaceutical_market_segmentation_by_diagnosis).
narrative_ontology:affects_constraint(future_dsm_integration_2026, pathologization_of_poverty_and_marginalization).

% DUAL FORMULATION NOTE:
% The DSM integration effort decomposes into three distinct structural constraints: (1) the institutional lock-in of categorical psychiatry (ε~0.08, Mountain — immutable by institutional design), (2) the pharmaceutical incentive structure that rewards diagnostic expansion (ε~0.45, Tangled Rope — mixed coordination and extraction), and (3) the specific pathologization of social/economic conditions as psychiatric disorders (ε~0.52, this story). These are not the same constraint viewed from different angles; they have different causal mechanisms and different resolution pathways. The DSM integration story is the interface where institutional psychiatry, pharmaceutical incentives, and social pathologization mechanisms interact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
