% ============================================================================
% CONSTRAINT STORY: federal_construction__lander_cultural_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federal_construction__lander_cultural_sovereignty, []).

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
 *   constraint_id: federal_construction__lander_cultural_sovereignty
 *   human_readable: Länder Cultural Sovereignty in German Federalism
 *   domain: legal/constitutional/education_policy
 *
 * SUMMARY:
 *   The German federal constitution enshrines a principle of Kulturhoheit —
 *   the forbidding of federal formation of minds — that allocates education,
 *   culture, broadcasting, and police authority to the sixteen Länder. This
 *   is presented as a foundational feature of German federalism, protecting
 *   regional autonomy and cultural diversity from centralized homogenization.
 *   However, this principle instantiates a contested kernel: the federal
 *   construction itself. Three structurally distinct readings coexist: this
 *   reading (Länder cultural sovereignty as a protective barrier), the
 *   Bundesrat entanglement reading (Land executives inside federal lawmaking
 *   through the upper house), and the cooperative drift reading (the
 *   separation has eroded into entangled joint governance where no level acts
 *   independently). This constraint story models the first reading — the
 *   boundary-protection reading. The constraint exhibits the signature of a
 *   Tangled Rope: it coordinates genuine regional diversity and autonomous
 *   policy-making (coordination function) while simultaneously extracting
 *   mobility costs, credential non-portability, and standardization
 *   suppression (asymmetric extraction). The suppression is high (0.62)
 *   because the constitutional prohibition actively prevents federal cultural
 *   policy solutions; federal actors cannot act unilaterally. The
 *   extractiveness is moderate (0.38) because the constraint's primary
 *   function is coordination of regional autonomy, not extraction — the
 *   extraction is a secondary effect. The theater ratio (0.51) reflects the
 *   increasing gap between the formal constitutional prohibition and the
 *   practical coordination structures (EU directives, interstate agreements,
 *   joint curricula committees) that have hollowed the barrier while
 *   maintaining the rhetorical claim.
 *
 * KEY AGENTS:
 *   - Land Governments (especially larger Länder): Primary beneficiary (institutional/arbitrage) — exercise educational and cultural sovereignty; control curriculum, broadcasting, police organization; benefit from policy differentiation and regional identity formation
 *   - Federal Government: Victim-adjacent (institutional/constrained) — forbidden from cultural policy; can influence only through indirect mechanisms (funding, framework legislation, Bundesrat negotiation)
 *   - Inter-Land Migrants and Mobile Workers: Primary victim (moderate/constrained) — face credential non-recognition, curriculum incompatibility, career barriers when crossing Land boundaries; no federal recourse mechanism
 *   - National Standardization Interests: Victim (powerless/trapped) — efficiency-maximizing preferences for uniform standards have no institutional voice; cannot organize collective action at federal level
 *   - EU and Interstate Coordination Bodies: Organized actors (organized/constrained) — building harmonization pathways (Bologna Process, EU directives, teacher reciprocity) that gradually undermine constitutional rigidity
 *   - Constitutional Court (BVerfG): Institutional guardian (institutional/analytical) — enforces the boundary but has increasingly acknowledged practical entanglement; mediates between strict reading and cooperative reality
 *   - Analytical Observer: Civilizational level (analytical/analytical) — risks naturalizing a constitutional choice as inherent federalism principle
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federal_construction__lander_cultural_sovereignty, 0.38).
domain_priors:suppression_score(federal_construction__lander_cultural_sovereignty, 0.62).
domain_priors:theater_ratio(federal_construction__lander_cultural_sovereignty, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federal_construction__lander_cultural_sovereignty, extractiveness, 0.38).
narrative_ontology:constraint_metric(federal_construction__lander_cultural_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(federal_construction__lander_cultural_sovereignty, theater_ratio, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federal_construction__lander_cultural_sovereignty, tangled_rope).
narrative_ontology:human_readable(federal_construction__lander_cultural_sovereignty, "Länder Cultural Sovereignty in German Federalism").
narrative_ontology:topic_domain(federal_construction__lander_cultural_sovereignty, "legal/constitutional/education_policy").

domain_priors:requires_active_enforcement(federal_construction__lander_cultural_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federal_construction__lander_cultural_sovereignty, '4fac97d7-f45f-497c-975e-ef883c313f1f').
narrative_ontology:cs_kernel_codification('4fac97d7-f45f-497c-975e-ef883c313f1f', formalized).
narrative_ontology:cs_authority_grounding('4fac97d7-f45f-497c-975e-ef883c313f1f', lineage).
narrative_ontology:cs_interpretation_layer_present('4fac97d7-f45f-497c-975e-ef883c313f1f').
narrative_ontology:cs_reading_relation('4fac97d7-f45f-497c-975e-ef883c313f1f', federal_construction__bundesrat_entanglement, coexists_with).
narrative_ontology:cs_reading_relation('4fac97d7-f45f-497c-975e-ef883c313f1f', federal_construction__cooperative_drift_reading, influences).
narrative_ontology:cs_axiom('4fac97d7-f45f-497c-975e-ef883c313f1f', foundational, cultural_formation_cannot_be_centralized).
narrative_ontology:cs_axiom_status(cultural_formation_cannot_be_centralized, holdable).
narrative_ontology:cs_axiom_grounding('4fac97d7-f45f-497c-975e-ef883c313f1f', cultural_formation_cannot_be_centralized, deontological).
narrative_ontology:cs_axiom('4fac97d7-f45f-497c-975e-ef883c313f1f', secondary, regional_difference_is_intrinsic_good).
narrative_ontology:cs_axiom_status(regional_difference_is_intrinsic_good, holdable).
narrative_ontology:cs_axiom_grounding('4fac97d7-f45f-497c-975e-ef883c313f1f', regional_difference_is_intrinsic_good, deontological).
narrative_ontology:cs_reference_frame('4fac97d7-f45f-497c-975e-ef883c313f1f', constitutional_separation_of_cultural_authority).
narrative_ontology:cs_drift_state('4fac97d7-f45f-497c-975e-ef883c313f1f', contemporary_european_integration, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4fac97d7-f45f-497c-975e-ef883c313f1f', '').
narrative_ontology:cs_kernel_id(federal_construction__lander_cultural_sovereignty, federal_construction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federal_construction__lander_cultural_sovereignty, regional_governments).
narrative_ontology:constraint_beneficiary(federal_construction__lander_cultural_sovereignty, land_school_systems).
narrative_ontology:constraint_beneficiary(federal_construction__lander_cultural_sovereignty, cultural_diversity_interests).
narrative_ontology:constraint_victim(federal_construction__lander_cultural_sovereignty, national_standardization).
narrative_ontology:constraint_victim(federal_construction__lander_cultural_sovereignty, inter_land_mobility).
narrative_ontology:constraint_victim(federal_construction__lander_cultural_sovereignty, federal_cultural_policy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A student or family relocating between Länder faces sixteen separate curricula, incompatible certifications, and no federal recourse. Exit from the constraint means leaving the nation entirely. Maximum extraction: trapped agents bear full cost of fragmentation while the structural lock prevents federal coordination solutions.
constraint_indexing:constraint_classification(federal_construction__lander_cultural_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Teachers, broadcast professionals, and cultural workers face recognition barriers and credential non-portability across Land boundaries. Some agency through professional licensing and EU mobility directives, but significant career costs. The constraint simultaneously coordinates regional autonomy and extracts mobility costs.
constraint_indexing:constraint_classification(federal_construction__lander_cultural_sovereignty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Regional governments benefit from the educational and cultural sovereignty they exercise — control over curriculum, broadcasting policy, police organization, and cultural funding. The constraint enables their preferred coordination: regional identity formation and policy differentiation. No extraction experienced; pure benefit from the institutional autonomy the constraint provides.
constraint_indexing:constraint_classification(federal_construction__lander_cultural_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% EU directives, Bologna Process accords, and teacher-training reciprocity agreements represent organized actors building coordination pathways that bypass the constitutional rigidity. These agents see the Land sovereignty constraint as temporary — interstate agreements and supranational standards are gradually harmonizing certification and curriculum, creating effective sunset pressure on the constitutional barrier.
constraint_indexing:constraint_classification(federal_construction__lander_cultural_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The federal prohibition on 'formation of minds' (Bildungshoheit) has become largely ceremonial. Joint curricula committees, interstate agreements, and de facto harmonization have hollowed the constitutional barrier while the formal prohibition persists. Land governments maintain the sovereignty claim rhetorically while participating in coordinating structures that undermine it. Theater ratio reflects the gap between formal prohibition and practiced coordination.
constraint_indexing:constraint_classification(federal_construction__lander_cultural_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational perspective, the principle that culture and education cannot be centralized is presented as inherent to federalism itself — that the formation of minds must remain local or the federalism collapses into hierarchy. This perspective treats Land sovereignty as a natural law of political architecture. However, structural data reveals this as a false summit: the constraint benefits identifiable actors (Land governments) and its maintenance requires active suppression of mobility and harmonization (suppression ≥ 0.62). The 'natural law' framing naturalizes what is a contingent constitutional choice.
constraint_indexing:constraint_classification(federal_construction__lander_cultural_sovereignty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federal_construction__lander_cultural_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(federal_construction__lander_cultural_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(federal_construction__lander_cultural_sovereignty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(federal_construction__lander_cultural_sovereignty, TR),
    TR >= 0.70.

:- end_tests(federal_construction__lander_cultural_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate but rising. The constraint's primary function is coordination of regional diversity — Land governments genuinely need autonomy to implement distinct educational philosophies and cultural policies. This is legitimate coordination, not pure extraction. However, the constraint does extract costs: inter-Land mobility is suppressed, credential portability is blocked, and federal actors cannot respond to uniform problems. The rising trajectory (0.22→0.38 over 77 years) reflects that as Germany became more mobile and integrated, the coordination function became less salient while the extraction cost became more visible. The suppression mechanism is strengthening because Land governments are defending the boundary more actively as EU and interstate pressure increases. Suppression (0.62): High and stable. The constitutional prohibition on 'formation of minds' is a structural barrier — federal actors literally cannot legislate in these domains without constitutional amendment. This is not rhetorical suppression; it is doctrinal. The constraint actively suppresses federal cultural policy alternatives. Theater ratio (0.51): Moderate and rising. The constitutional prohibition has become partially performative. EU directives (Bologna Process, workplace safety standards), interstate agreements (teacher reciprocity, curricular harmonization), and joint committees have created de facto coordination that bypasses the formal prohibition. Yet the prohibition persists rhetorically — Land governments invoke it while participating in harmonizing structures. The gap between formal claim and actual practice is widening (0.38→0.51), indicating that the constraint is increasingly maintained through theater rather than through active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence across power levels and exit options. Land governments (institutional/arbitrage) see pure coordination — the constraint enables regional diversity. Inter-Land migrants (moderate/constrained) see extraction — the constraint suppresses their mobility without their consent. Federal government (institutional/constrained, different exit than Land governments) sees a negotiated barrier — they can influence through Bundesrat but cannot act unilaterally. Harmonization actors (organized/constrained) see a temporary problem with a sunset — interstate and EU agreements are building solutions. The constitutional court (institutional/analytical) sees a genuine principle under pressure — it acknowledges entanglement while maintaining the formal boundary. The analytical observer at civilizational scope risks seeing a natural law (federalism inherently requires local control), but the structural data reveals this as a false summit: identifiable beneficiaries exist, suppression is active and measurable, and the constraint's maintenance requires constant enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from each agent's structural relationship to the extraction flow: Land governments experience d ≈ 0.10 (beneficiary + arbitrage exit = minimal extraction toward them). Federal government experiences d ≈ 0.65 (constrained by constitutional barrier, unable to act unilaterally; victimized by the suppression but not powerless — can negotiate through Bundesrat, frame issues as infrastructure rather than education, use funding leverage). Inter-Land migrants experience d ≈ 0.85 (trapped in credential non-portability, no exit, forced to accept the cost). EU/interstate bodies experience d ≈ 0.50 (symmetric; they benefit from successful harmonization but also compete with Land sovereignty claims for authority). The perspectives produce differentiated chi values from these directionalities: Land governments see rope (low chi); federal actors see tangled rope (moderate chi from being partially constrained); trapped agents see snare (high chi from being unable to exit); harmonizing bodies see scaffold (low chi from seeing the structure as temporary and solvable).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that the legitimate coordination function (regional autonomy) and the extractive burden (mobility suppression) are genuinely coupled — they cannot be separated without redesigning the entire federal structure. The Land governments cannot have cultural sovereignty without suppressing federal-level standardization. The mobile workers cannot have credential portability without reducing Land autonomy. The question is not 'is this coordination or extraction?' but 'which value does federalism prioritize — regional autonomy or internal mobility?' Different readings of the contested kernel (cultural sovereignty vs. Bundesrat entanglement vs. cooperative drift) prioritize these differently. This reading (cultural sovereignty) privileges regional autonomy; the cooperative drift reading privileges entanglement and co-responsibility. Both are structural readings, not misclassifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is German federalism fundamentally structured by Land cultural sovereignty (this reading), by Bundesrat co-administration (bundesrat_entanglement reading), or by cooperative entanglement that has eroded the original separation (cooperative_drift reading)?',
    'Historical institutional analysis of constitutional amendments, legislative practice, and inter-Land disputes. Track which reading''s premises guide actual dispute resolution: Do courts protect Land sovereignty absolutely (supporting this reading)? Do courts enforce Bundesrat entanglement (supporting bundesrat_entanglement)? Do courts acknowledge entanglement and shift focus to accountability rather than separation (supporting cooperative_drift)?',
    'If this reading: federal cultural policy remains structurally forbidden; harmonization requires constitutional amendment. If bundesrat_entanglement: Land governments hold veto via Bundesrat; policy is negotiated, not separated. If cooperative_drift: separation is nominal; actual governance is entangled; focus shifts to co-responsibility and joint accountability mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which constitutional reading explains German federalism''s actual structure').

omega_variable(
    harmonization_pressure_trajectory,
    'Will EU standardization directives and interstate coordination agreements gradually erode the Land sovereignty constraint (scaffold sunset logic), or will the constitutional protection hold indefinitely?',
    'Longitudinal tracking of: (a) EU regulatory scope expansion into education and culture; (b) interstate reciprocity agreements bypassing constitutional barriers; (c) actual curricular divergence vs. convergence over 20-year horizon; (d) constitutional court rulings on federal authority in culture/education.',
    'If harmonization succeeds: scaffold perspective is correct; sunset is real; extractiveness declines as the constraint loses enforcement power. If constitutional protection holds: extractiveness remains stable or increases as enforcement tightens; scaffold perspective is aspirational, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harmonization_pressure_trajectory, empirical, 'Whether EU and interstate coordination will erode Land educational sovereignty').

omega_variable(
    suppression_mechanism_structural_vs_rhetorical,
    'Is the suppression (0.62) of federal cultural policy structural (constitutional barriers that genuinely prevent federal action) or rhetorical (federal government chooses not to act, defending inaction through the sovereignty claim)?',
    'Constitutional court rulings on federal authority boundaries; federal government budget submissions requesting cultural authority; Bundestag legislative attempts in education/culture domain; expert analysis of whether constitutional barriers are absolute or negotiable through creative statutory interpretation.',
    'If structural: the constraint''s suppression is a genuine feature of the constitutional order; federal action would require amendment. If rhetorical: the constraint''s suppression is performative; federal governments could act but invoke the constitutional claim to justify subsidiarity preference. This distinction determines whether the mountain perspective is genuinely foreclosed (structural) or is a false summit (rhetorical naturalization).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_rhetorical, empirical, 'Whether suppression of federal cultural policy is constitutional or rhetorical').

omega_variable(
    beneficiary_externality_costs,
    'Do the beneficiaries of Land sovereignty (Land governments, cultural diversity) internalize the costs imposed on mobility and uniformity interests, or do they externalize these costs?',
    'Cost-benefit analysis comparing: (a) gains to Land governments from educational autonomy; (b) documented costs to inter-Land migrants and workers; (c) efficiency losses from credential non-recognition; (d) whether Land governments compensate affected parties or coordinate harmonization to reduce externalities.',
    'If costs are externalized: the constraint is rent-seeking by Land governments, strengthening the snare classification for trapped agents. If costs are internalized through coordination: the tangled_rope classification holds; the constraint has a legitimate dual function (coordination + extraction) rather than pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_externality_costs, empirical, 'Whether Land sovereignty externalities are internalized or absorbed by other agents').

omega_variable(
    false_summit_axiom_grounding,
    'Is the claim that ''formation of minds cannot be federalized'' grounded in deontological principle (sovereignty as intrinsic right), empirical contingency (federalism requires local control), or instrumental reasoning (federal control would be inefficient)?',
    'Constitutional law doctrine analysis: Weimar constitutional texts, founding debates, BVerfG jurisprudence. Identify which grounding type the German legal tradition uses. Compare to other federations (US, Switzerland, Austria, Australia) to determine whether the axiom is universal (suggesting deontological or natural-law grounding) or contingent (suggesting instrumental or conventional grounding).',
    'If deontological: the axiom is resistant to empirical challenge or drift; the mountain perspective has genuine force. If empirical: the axiom is vulnerable to evidence that federal cultural policy works; drift state triggers reclassification. If instrumental: the axiom is subject to efficiency-based renegotiation when circumstances change.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_axiom_grounding, conceptual, 'What kind of authority grounds the Länder cultural sovereignty axiom').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federal_construction__lander_cultural_sovereignty, 1949, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lcs_tr_t0, federal_construction__lander_cultural_sovereignty, theater_ratio, 0, 0.38).
narrative_ontology:measurement(lcs_tr_t15, federal_construction__lander_cultural_sovereignty, theater_ratio, 15, 0.45).
narrative_ontology:measurement(lcs_tr_t30, federal_construction__lander_cultural_sovereignty, theater_ratio, 30, 0.51).

% Extraction over time
narrative_ontology:measurement(lcs_be_t0, federal_construction__lander_cultural_sovereignty, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(lcs_be_t15, federal_construction__lander_cultural_sovereignty, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(lcs_be_t30, federal_construction__lander_cultural_sovereignty, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(lcs_su_t0, federal_construction__lander_cultural_sovereignty, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(lcs_su_t15, federal_construction__lander_cultural_sovereignty, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(lcs_su_t30, federal_construction__lander_cultural_sovereignty, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federal_construction__lander_cultural_sovereignty, identity_coordination).
narrative_ontology:affects_constraint(federal_construction__lander_cultural_sovereignty, bundesrat_entanglement).
narrative_ontology:affects_constraint(federal_construction__lander_cultural_sovereignty, cooperative_drift_reading).
narrative_ontology:affects_constraint(federal_construction__lander_cultural_sovereignty, eu_standardization_pressure).
narrative_ontology:affects_constraint(federal_construction__lander_cultural_sovereignty, inter_land_credential_portability).

% DUAL FORMULATION NOTE:
% The Länder cultural sovereignty constraint is one reading of the federal construction kernel. The other readings (Bundesrat entanglement, cooperative drift) are structurally distinct constraints with different ε values and different victim/beneficiary sets. This family of constraints shares the same constitutional text but decomposes into three distinct structural stories. Affects_constraints links trace the network: this reading affects the cooperative_drift reading (by defining what the drift is drifting from) and the bundesrat_entanglement reading (by defining what is being co-administered). External constraints affected: EU standardization pressure represents organized actors circumventing the constitutional boundary; inter-land credential portability represents a downstream harm created by the sovereignty principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federal_construction__lander_cultural_sovereignty, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
