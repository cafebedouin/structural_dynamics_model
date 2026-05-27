% ============================================================================
% CONSTRAINT STORY: originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_originalist_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: originalist_reading
 *   human_readable: Originalist Constitutional Authority: Fixed Meaning at Ratification
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   Originalism is a reading of constitutional authority that fixes meaning
 *   at the moment of ratification, grounding legitimacy in historical public
 *   understanding of the text. This constraint exhibits Tangled Rope
 *   structure: it provides a genuine coordination function (standardized
 *   methodology for interpretation, predictable doctrine, reduction of
 *   arbitrary judicial discretion) while simultaneously enabling extraction
 *   from agents who cannot access or benefit from the methodological
 *   framework (unenumerated rights claimants, adaptive interpreters
 *   constrained by originalist gatekeeping, disenfranchised populations whose
 *   interests were absent from the ratifiers' historical understanding). The
 *   constraint has intensified over the measurement interval (1985–2025):
 *   originalism evolved from a methodological proposal to an institutional
 *   movement (Federalist Society, originalist appointments, law school
 *   curriculum), theater ratio increased as methodological rigor became
 *   decoupled from outcome constraint, and base extractiveness stabilized as
 *   the constraint's enforcement mechanisms matured. Theater elevation
 *   reflects growing scholarly observation that originalist methodology often
 *   produces outcomes consistent with originalist jurists' broader policy
 *   preferences, suggesting the constraint functions partly as a legitimacy
 *   cover for ideological interpretation rather than purely as a
 *   discretion-limiting discipline. The constraint presents as a natural law
 *   to originalist practitioners (Mountain perspective) but reveals through
 *   cross-position analysis as a contingent institutional reading that
 *   concentrates interpretive authority.
 *
 * KEY AGENTS:
 *   - Originalist Jurists: Primary beneficiary (institutional/arbitrage) — capture methodological clarity, doctrinal stability, institutional network support; can arbitrage between originalist and non-originalist contexts
 *   - Unenumerated Rights Claimants: Primary victim (powerless/trapped) — historical public understanding at ratification forecloses recognition; exit requires Article V amendment (prohibitively costly)
 *   - Adaptive Constitutional Interpreters: Secondary victim (moderate/constrained) — constrained by originalist gatekeeping but benefit from methodological predictability; career costs to departing framework
 *   - Non-Originalist Judicial Institutions: Institutional actor (institutional/constrained) — constrained by originalist doctrinal entrenchment but benefit from stable constraint for strategic doctrine-building
 *   - Law School Apparatus: Institutional actor (institutional/arbitrage) — maintains originalist institutional networks; theater ratio high (rigor maintained but outcomes increasingly driven by underlying preferences)
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing originalism as inherent to constitutional law rather than contingent reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(originalist_reading, 0.38).
domain_priors:suppression_score(originalist_reading, 0.52).
domain_priors:theater_ratio(originalist_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(originalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(originalist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(originalist_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(originalist_reading, tangled_rope).
narrative_ontology:human_readable(originalist_reading, "Originalist Constitutional Authority: Fixed Meaning at Ratification").
narrative_ontology:topic_domain(originalist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(originalist_reading, 'ad9a7fba-c7bb-48b2-a728-09fc29108927').
narrative_ontology:cs_created_at('ad9a7fba-c7bb-48b2-a728-09fc29108927', '').
narrative_ontology:cs_kernel_codification('ad9a7fba-c7bb-48b2-a728-09fc29108927', fixed_text).
narrative_ontology:cs_authority_grounding('ad9a7fba-c7bb-48b2-a728-09fc29108927', lineage).
narrative_ontology:cs_interpretation_layer_present('ad9a7fba-c7bb-48b2-a728-09fc29108927').
narrative_ontology:cs_kernel_id(originalist_reading, constitutional_text_authority).
narrative_ontology:cs_reading_relation('ad9a7fba-c7bb-48b2-a728-09fc29108927', living_constitutionalist_reading, forecloses).
narrative_ontology:cs_reading_relation('ad9a7fba-c7bb-48b2-a728-09fc29108927', positivist_reading, coexists_with).
narrative_ontology:cs_axiom('ad9a7fba-c7bb-48b2-a728-09fc29108927', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom('ad9a7fba-c7bb-48b2-a728-09fc29108927', foundational, historical_public_understanding_objective).
narrative_ontology:cs_axiom_status(historical_public_understanding_objective, holdable).
narrative_ontology:cs_reference_frame('ad9a7fba-c7bb-48b2-a728-09fc29108927', founding_era_constitutional_meaning).
narrative_ontology:cs_drift_state('ad9a7fba-c7bb-48b2-a728-09fc29108927', contemporary_judicial_implementation, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(originalist_reading, originalist_jurists).
narrative_ontology:constraint_beneficiary(originalist_reading, executive_constraint_seekers).
narrative_ontology:constraint_beneficiary(originalist_reading, historical_methodology_practitioners).
narrative_ontology:constraint_victim(originalist_reading, disenfranchised_populations).
narrative_ontology:constraint_victim(originalist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(originalist_reading, adaptive_constitutional_interpretation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNENUMERATED RIGHTS CLAIMANT (SNARE) — Structurally trapped by the originalist framework's restrictive gate on recognizing new rights. Historical public understanding at ratification forecloses recognition of contemporary claims (privacy, autonomy, dignity claims not explicitly enumerated). Compressed exit options: either abandon the claim or pursue Article V amendment (prohibitively costly). Bears maximum extraction from the constraint — the constraint's primary mechanism is suppression of alternative interpretive pathways.
constraint_indexing:constraint_classification(originalist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ADAPTIVE CONSTITUTIONALIST INTERPRETER (TANGLED ROPE) — Constrained by originalism's epistemic methodology (historical evidence requirements, public understanding documentation) but also benefits from the constraint's rigidity as a coordination function: originalism provides predictable, documentable interpretation standards that enable legal planning and reduce judicial arbitrariness. Mixed extraction: suppressed in scope of permissible interpretation, enabled in procedural clarity. Moderate power and constrained exit reflect career cost of departing originalist framework within contemporary law schools and judicial networks.
constraint_indexing:constraint_classification(originalist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORIGINALIST JURIST (ROPE) — Net beneficiary of the constraint. Originalism's fixed-meaning framework provides doctrinal clarity, interpretive methodology, and professional legitimacy within originalist networks. Low experienced extraction because the jurist can arbitrage between originalist and non-originalist venues and select cases/opinions strategically. The constraint solves a coordination problem for originalist practitioners: standardizing interpretation methodology enables communication and doctrine-building.
constraint_indexing:constraint_classification(originalist_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NON-ORIGINALIST JUDICIAL INSTITUTION (TANGLED ROPE) — Constrained by originalism's doctrinal entrenchment (originalist judges and justices have institutional power; Originalist Society networks have epistemic influence) but benefits from originalism's provision of stable constraint: predictable methodology enables strategic doctrinal development around originalist precedent rather than requiring constant reinterpretation. Mixed experience: suppressed in scope of permissible new doctrine, enabled in strategic coordination against originalist overreach.
constraint_indexing:constraint_classification(originalist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational analytical perspective, the constraint appears as an immutable feature of constitutional law: the text is fixed in time; historical context is epistemically objective; interpretation cannot change what the ratifiers meant. This perspective risks naturalizing the originalist methodology as inherent to constitutional authority rather than contingent institutional choice. The engine will flag this as a false summit: originalism is one reading of constitutional authority, not a law of nature. Historical understanding itself is interpretively constructed.
constraint_indexing:constraint_classification(originalist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: LAW SCHOOL INSTITUTIONAL APPARATUS (PITON) — Originalism has become institutionalized in law schools, federal judiciary appointments, and Federalist Society networks, but the primary function it once served (constraining judicial discretion through textual methodology) is now increasingly theatrical: originalist judges show methodological rigor but achieve preferred outcomes through originalist reasoning (telos fishing, framers' intent selection, historical contingency weighting). Theater ratio elevated because the constraint persists through institutional inertia (funding, appointment networks, prestige association) rather than because originalist methodology uniquely constrains judicial outcomes. The machinery of originalism runs but its primary function (discretion constraint) has degraded.
constraint_indexing:constraint_classification(originalist_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(originalist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(originalist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(originalist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(originalist_reading, TR),
    TR >= 0.70.

:- end_tests(originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Originalism extracts through methodological gatekeeping (restricting permissible interpretation pathways) and historical evidence requirements that favor certain interests (those represented in available historical sources, typically propertied classes). However, the extraction is not as severe as pure exclusion — originalist methodology does provide genuine coordination benefits (predictable doctrine, methodological standards, reduced arbitrariness). The modest extractiveness reflects this hybrid character: real coordination function coexists with real asymmetric extraction. Suppression (0.52): Moderate-high. The constraint operates through high barriers to recognition of unenumerated rights, suppression of adaptive interpretation pathways, and gatekeeping through historical evidence documentation. However, suppression is not total — non-originalist interpreters can articulate alternative methodologies and argue for historical reinterpretation. The level reflects that suppression operates through methodological authority rather than formal prohibition. Theater ratio (0.48): Moderate. Over the measurement interval (1985–2025), theater ratio increased from ~0.25 to ~0.48 as originalism became institutionalized. Early originalism (1985) functioned more as pure methodology; contemporary originalism (2025) exhibits higher theater as the constraint's enforcement becomes increasingly institutional (appointment networks, Federalist Society influence, law school gatekeeping) rather than purely methodological. The theater reflects growing scholarly debate about whether originalist reasoning often produces outcomes consistent with originalist jurists' underlying preferences.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates pronounced perspectival divergence. The originalist jurist sees coordination (Rope): clear methodology, predictable doctrine, professional legitimacy. The adaptive interpreter sees mixed coordination and extraction (Tangled Rope): genuine clarity benefits offset by suppression of interpretive pathways. The unenumerated rights claimant sees pure extraction (Snare): trapped by methodological gatekeeping with no exit. The law school apparatus sees degraded function (Piton): methodology persists through institutional inertia while increasingly theater-like. The analytical observer risks seeing natural law (Mountain): constitutional meaning is fixed at ratification and cannot change without Article V — this appears inevitable, immutable, structural. However, the engine's false summit detector flags this: originalism's appearance of natural law dissolves when exposed to cross-position analysis. Historical understanding itself is interpretively constructed; methodological gatekeeping serves identifiable beneficiaries; the constraint's naturalness is rhetorical rather than structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist jurists experience low d (0.15–0.20): they are net beneficiaries of the constraint with high exit optionality (arbitrage — can move between originalist and non-originalist venues). Unenumerated rights claimants experience high d (0.92–0.95): they are trapped by the constraint with no effective exit (cannot abandon fundamental claims; Article V amendment is prohibitively costly). Adaptive interpreters experience moderate d (0.60–0.65): constrained but not trapped; they benefit partially from methodological clarity while bearing significant costs. The institutional perspectives show d variation: originalist institutional actors at d ~0.10–0.15 (beneficiaries with arbitrage), non-originalist institutions at d ~0.55–0.65 (constrained by entrenchment but benefiting from stability). The analytical observer at canonical d ~0.73 (moderate power, analytical exit). The directionality structure produces the perspectival gap: beneficiary perspectives see Rope or Scaffold; victim perspectives see Snare or Tangled Rope; analytical perspective sees Mountain (false summit candidate). The piton perspective derives from theater ratio elevation, not from high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   Originalism exemplifies mandatrophy by appearing to resolve the problem of judicial discretion (preventing arbitrary interpretation through methodological discipline) while actually enabling a different form of discretion (outcome-driven methodology selection, favorable historical narrative construction, strategic application of historical evidence). The constraint's legitimacy claim—that it constrains discretion through historical objectivity—obscures its actual mechanism: authority concentration through methodological gatekeeping. This is the classic mandatrophy pattern: solving one problem (arbitrary interpretation) while enabling another (arbitrary gatekeeping). The measurement interval shows increasing theater ratio as originalism becomes institutionalized — the machinery becomes more complex and harder to evaluate from within. The mandatrophy resolution requires recognizing that the constraint provides real coordination benefits (predictable doctrine) alongside real extraction (methodological gatekeeping of unenumerated rights). Both are structural; neither fully describes the constraint. Classification as Tangled Rope (not Rope) captures this hybrid: genuine coordination plus asymmetric extraction, both enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_understanding_construction,
    'Is historical public understanding at ratification an objective historical fact or an interpretively constructed narrative?',
    'Meta-historical analysis: historiographical comparison of competing accounts of what ratifiers understood, evidence of selective source-picking, documentary archive coverage and bias, counterfactual alternative narratives with equal historical support',
    'If objective fact: originalism provides knowable constraint on interpretation (Rope/Mountain features stable). If interpretively constructed: originalism''s constraint derives from rhetorical authority rather than objective grounding (Snare/Tangled Rope features highlight interpretive discretion hidden within methodology). Classification could shift: from claimed Mountain to Tangled Rope or Snare depending on transparency of construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_understanding_construction, conceptual, 'Whether historical public understanding is objective or constructed').

omega_variable(
    methodology_versus_outcome_correlation,
    'Does originalist methodology constrain judicial outcomes more effectively than living constitutionalism or other interpretive approaches?',
    'Empirical analysis: comparison of outcome distributions across originalist vs non-originalist judges on identical legal questions; measurement of outcome-driven reasoning in opinions labeled ''originalist'' vs methodology-driven reasoning; correlation between originalist methodology adoption and ideological consistency of outcomes',
    'If originalism constrains outcomes: validates rope/mountain coordination function (methodological discipline reduces arbitrariness). If outcomes drive methodology: originalism is theater masking ideological preference (Piton validation, Snare features more salient). Theater ratio calibration depends on this correlation strength.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodology_versus_outcome_correlation, empirical, 'Whether originalist methodology constrains judicial outcomes').

omega_variable(
    unenumerated_rights_foreclosure_necessity,
    'Is the restriction on recognizing unenumerated rights a necessary logical consequence of originalism, or a contingent policy choice within originalist frameworks?',
    'Jurisprudential analysis: examination of originalist scholars who recognize unenumerated rights (libertarian originalists, Privileges or Immunities originalists); comparison of methodological commitments; identification of which originalist axioms actually require foreclosure of unenumerated rights vs which are consistent with recognition',
    'If necessary consequence: originalism''s suppression of unenumerated rights claims is structural (not contingent policy). If contingent choice: the constraint''s extraction mechanism can be partially decoupled from originalist methodology — some originalist frameworks could reduce suppression without abandoning the reading (Tangled Rope could be partially reformed). Affects classification robustness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unenumerated_rights_foreclosure_necessity, conceptual, 'Whether unenumerated rights foreclosure is necessary to originalism').

omega_variable(
    kernel_vs_reading_ambiguity,
    'Does the constitutional text (kernel) commit to originalist interpretation, or does originalism commit to a particular reading of the text?',
    'Textual analysis: whether the Constitution contains self-interpreting provisions about how it should be interpreted; comparison with other constitutional commitments about amendment (Article V) to assess whether the text presupposes fixed or evolving meaning',
    'If text commits to originalism: originalism is not a reading but the kernel''s self-specification (classification boundary blurs). If originalism is a reading: the false-summit diagnostic is active — the constraint appears as natural law (mountain) from originalist perspective but is revealed as contingent reading by cross-position analysis. Affects how the constraint structures authority legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_vs_reading_ambiguity, conceptual, 'Whether text commits to originalist interpretation or vice versa').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(originalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orig_tr_t0, originalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(orig_tr_t20, originalist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(orig_tr_t40, originalist_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(orig_be_t0, originalist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(orig_be_t20, originalist_reading, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(orig_be_t40, originalist_reading, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(originalist_reading, identity_coordination).
narrative_ontology:affects_constraint(originalist_reading, living_constitutionalist_reading).
narrative_ontology:affects_constraint(originalist_reading, positivist_reading).
narrative_ontology:affects_constraint(originalist_reading, article_v_amendment_gatekeeping).

% DUAL FORMULATION NOTE:
% Originalism is one reading of the kernel constitutional_text_authority. The sibling readings (living constitutionalism, positivism) are separate constraints with different ε values reflecting different structural extraction mechanisms. Originalism's constraint structure derives from historical gatekeeping; living constitutionalism's derives from discretionary adaptation; positivism's derives from institutional authority. These are not variations of a single constraint but structurally distinct constraints sharing a common kernel. Link via network.affects_constraints to enable cross-reading contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(originalist_reading, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
