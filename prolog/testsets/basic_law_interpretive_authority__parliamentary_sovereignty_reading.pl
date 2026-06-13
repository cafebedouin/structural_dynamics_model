% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty: Legislative Final Authority Over Constitutional Meaning
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint instantiates the parliamentary sovereignty reading of the
 *   contested kernel 'basic_law_interpretive_authority': the claim that an
 *   elected legislature, through democratic mandate and representative
 *   accountability, retains final interpretive authority over constitutional
 *   meaning. The reading asserts that when legislatures and courts disagree,
 *   the legislature's will should prevail because it alone is directly
 *   accountable to voters. This reading coordinates around democratic
 *   legitimacy and majority will. The extracted costs fall on constitutional
 *   minorities (who lack electoral voice), judicial independence
 *   infrastructure (which faces pressure from override threats), and
 *   rights-bearing individuals whose protections depend on judicial
 *   enforcement. The claim/metric gap is deliberate: this reading is CLAIMED
 *   as tangled_rope (genuine coordination around democratic authority PLUS
 *   asymmetric extraction from minorities) while the measurements document
 *   rising theater and suppression—the engine measures the divergence. This
 *   story decompose from the single natural-language concept 'basic law
 *   interpretive authority' into three structurally distinct constraints
 *   (sibling readings), each with its own ε and beneficiary/victim structure.
 *
 * KEY AGENTS:
 *   - elected_legislature: institutional actor; holds final interpretive authority under this reading; benefits directly from override capacity
 *   - legislative_supermajority_coalitions: beneficiaries; gain ability to move constitutional baselines when they can assemble votes
 *   - constitutional_minorities: powerless victims; protections depend on legislative grace rather than enforceable limits
 *   - judicial_independence_infrastructure: institutional victim; faces pressure from override threats and jurisdiction-stripping
 *   - rights_bearers_without_electoral_voice: moderate-power victims; core security depends on judicial protection against legislative pressure
 *   - judicial_review_courts: identity-locked payers; perform review function but lack final authority
 *   - democratic_constituency: beneficiary with high exit (can change legislatures via elections)
 *   - analytical_observer: examines the authority distribution and its effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.72).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty: Legislative Final Authority Over Constitutional Meaning").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'f9e22de5-be6d-4601-9ed3-8c9648c95fc8').
narrative_ontology:cs_kernel_codification('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', fixed_text).
narrative_ontology:cs_authority_grounding('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', extraction).
narrative_ontology:cs_reading_relation('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', foundational, legislative_supremacy_principle).
narrative_ontology:cs_axiom_status(legislative_supremacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', legislative_supremacy_principle, deontological).
narrative_ontology:cs_axiom('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', foundational, electoral_accountability_binds_constitutional_authority).
narrative_ontology:cs_axiom_status(electoral_accountability_binds_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', electoral_accountability_binds_constitutional_authority, instrumental).
narrative_ontology:cs_reference_frame('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', electoral_accountability_as_legitimacy_source).
narrative_ontology:cs_drift_state('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f9e22de5-be6d-4601-9ed3-8c9648c95fc8', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, legislative_supermajority_coalitions).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, judicial_independence_infrastructure).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_bearers_without_electoral_voice).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.54→0.68) because legislatures, once granted override authority, accumulate confidence in exercising it; subsequent overrides of judicial protections become normalized. Theater rises (0.28→0.42) because judicial review persists but increasingly operates as a staging ground for legislative preferences—courts issue opinions knowing they can be overridden, which transforms review from authoritative adjudication into legislative signaling. Suppression rises (0.58→0.72) because the constraint requires active suppression of supranational legal forums, formal amendment procedures, and judicial independence claims. The measurement series share one time grid (every metric at every point) so temporal alignment is exact. The plateau at t=32-40 reflects saturation: legislatures have internalized their override authority and the institutional pressure on minorities and courts stabilizes at the new equilibrium.
 *
 * PERSPECTIVAL GAP:
 *   The legislature and its beneficiary coalition perceive this arrangement as democratic coordination—the solution to tyrannical judicial overreach. Constitutional minorities and judges perceive it as the legislatively structured extraction of their institutional capacity and rights. The engine computes directionality from these opposed structural relationships: the legislature enters at low d (beneficiary, high power, analytical exit = near 0.1), minorities at high d (victims, powerless, trapped exit = near 0.95), judges at moderate-high d (constrained exit, institutional pressure = near 0.65). The same rule structure produces different perceived types for different seats: coordination for those who benefit; extraction for those who pay.
 *
 * DIRECTIONALITY LOGIC:
 *   Legislature (elected_legislature, institutional power, analytical exit options): derives d near 0.05—benefits from override authority, high institutional power, can exit via constitutional design change if threatened. Legislative supermajority coalitions (institutional, analytical exit): d near 0.08—direct beneficiaries of coalitional override authority. Constitutional minorities (powerless, trapped): d near 0.95—victims without electoral voice, cannot exit the jurisdiction or veto override. Judicial_review_courts (institutional power but identity-locked): d near 0.70—institutional pressure and override threat are substantial costs; identity-locked because exit (ceasing to be judges) is impossible while performing their institutional role; their directionality reflects the identity fusion: their role definition (constitutional interpreters) is contradicted by their structural position (final authority vested elsewhere). Rights_bearers_without_electoral_voice (moderate power, constrained exit): d near 0.80—security depends on judicial protection, which is threatened by legislative override. Democratic_constituency (organized power, mobile exit): d near 0.15—beneficiaries through electoral control; can exit (leave jurisdiction, immigrate) or vote out legislatures they dislike.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing judicial oligarchy, preserving democratic will) is declared as 'contested' status and 'live' in the legislative reading. However, outside benefiting seats there is substantial attestation that the founding problem has been largely solved in modern democracies: judicial independence is protected by ethics codes, public accountability, and professional norms. The real function now identified by critics is extractive: parliamentary sovereignty protects legislative majorities from judicial constraints on their power. This is the mandatrophy signature: the constraint was built to solve a problem (judicial overreach risk) that has been substantially addressed, but the constraint persists because it now serves a different, extractive function (insulating majorities from rights review). The tangled_rope classification captures this: genuine coordination (democracy does rest on elected representation) PLUS extraction (suppression of minority voice and judicial independence) maintained through active enforcement (override threats, jurisdiction-stripping threats).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    democratic_legitimacy_vs_majority_tyranny,
    'Does legislative authority grounded in electoral accountability actually prevent tyranny of the majority, or does it enable it by removing enforceable constitutional checks?',
    'Comparative analysis of jurisdictions with strong legislative supremacy vs. strong judicial review: do the former show higher rates of minority rights violations? Do the latter show higher rates of legislative frustration? Historical case studies of override decisions and their consequences for affected minorities.',
    'If analysis shows strong judicial review prevents majority tyranny while legislative supremacy enables it, the constraint''s democratic legitimacy claim is undermined and the classification shifts toward snare. If legislative supremacy shows equivalent or better protection outcomes, the coordination function is vindicated and tangled_rope holds. If outcomes are genuinely mixed by context, the constraint remains contested and the omega remains unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_legitimacy_vs_majority_tyranny, empirical, 'Whether electoral accountability actually constrains majority tyranny or whether it enables it.').

omega_variable(
    judicial_independence_under_override_threat,
    'Is judicial review genuinely independent when courts know their decisions can be overridden by legislative supermajority, or does override authority systematically suppress judicial willingness to constrain the legislature?',
    'Institutional analysis of judicial behavior before and after override authority is granted or threatened. Survey data on judicial perceptions of institutional autonomy. Analysis of opinion-writing patterns: do judges who face override threats write narrower opinions, defer to legislatures more often, or abandon rights review?',
    'If override threat systematically suppresses judicial review (measured by narrower opinions, deference patterns, or stated judicial anxiety), then the constraint''s effect is to convert courts from constitutional guardians into legislative advisors—the theater_ratio interpretation is validated and suppression is understated. If judicial behavior is largely unaffected by override threat, the suppression mechanism is weaker than authored and the constraint may be less extractive than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_independence_under_override_threat, empirical, 'Whether override authority suppresses independent judicial review.').

omega_variable(
    electoral_representation_and_minority_voice,
    'Do electoral mechanisms actually give constitutional minorities a voice proportionate to their interests, or do they structurally exclude minorities from meaningful legislative representation?',
    'Analysis of electoral outcomes: representation rates for racial, religious, gender, and other minorities in legislatures vs. population shares. Geographic clustering effects. Supermajority requirements that exclude minorities from coalition-building. Historical data on legislatures overriding protections for groups with low electoral power.',
    'If minorities are structurally underrepresented in legislatures and legislatures have overridden minority protections at high rates, the reading''s claim that electoral accountability protects minorities is false—the constraint operates as pure extraction and should reclassify toward snare. If electoral representation is robust for minorities and overrides are rare, the coordination function is vindicated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(electoral_representation_and_minority_voice, empirical, 'Whether electoral mechanisms actually represent constitutional minorities or exclude them.').

omega_variable(
    alternative_readings_foreclosure,
    'Does this reading (parliamentary sovereignty) logically foreclose the judicial supremacy reading, or are they genuinely coexistent positions that different parties hold simultaneously?',
    'Logical analysis of the core premises: if Parliament retains final authority, can courts simultaneously hold final authority? Or is one necessarily excluded by the other? Empirical observation of jurisdictions where both claims are made (e.g., UK with parliamentary supremacy and growing human rights judicial review; US with judicial review but legislative supermajority override mechanisms).',
    'If the readings foreclose each other (logically incompatible in a single framework), then the kernel bifurcates into two fundamentally different constitutional orders and comparative analysis is valid but cross-system prediction is not. If they coexist as lived positions in the same jurisdiction (institutional tensions managing both claims), they do not foreclose and institutional evolution can move between them. Foreclosure status affects how different jurisdictional readings are classified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_readings_foreclosure, conceptual, 'Whether parliamentary sovereignty logically forecloses judicial supremacy or whether they coexist as live contested positions.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.72) structural (external barriers—jurisdiction-stripping laws, electoral exclusion rules, institutional design preventing minority voice) or internalized (judges and minorities have internalized the lesson that override authority exists and have abandoned challenging legislatures even where they could)?',
    'Post-override behavior: if legislatures remove override threats and protections are restored, do judges and minority advocates immediately resume challenging authority (structural suppression), or do they remain deferential due to habit and internalized powerlessness (internalized suppression)? Interviews with judges and minority advocates about their perception of their authority. Historical periods where override authority was formalized vs. periods where it was merely threatened.',
    'If suppression is primarily structural, the constraint''s persistence depends on continued legal barriers—remedying those barriers (removing override authority, expanding minority electoral representation) would release suppression. If suppression is primarily internalized, the constraint would persist even after formal barriers are removed because the affected parties have learned defeat—this indicates deeper institutional capture and higher effective suppression than the scalar reflects. Internalized suppression affects remediation design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression operates through external institutional barriers or through internalized learned deference.').

omega_variable(
    kernel_reading_and_observational_dependence,
    'Does the choice of reading (parliamentary sovereignty vs. judicial supremacy vs. popular constitutionalism) depend on which observables we measure—or are the readings genuinely structurally distinct constraints?',
    'ε-invariance test: apply each reading''s framings to the same empirical domain (e.g., a specific constitutional dispute) and measure whether ε and the beneficiary/victim structure differ across readings independent of the observables chosen. If ε varies only with observation choice (e.g., measured one way yields high extraction, measured another way yields low), the readings describe the same constraint. If ε and victim/beneficiary structure are stable across reasonable observation choices within each reading, the readings are distinct constraints.',
    'If readings are distinct constraints (stable ε per reading), the family decomposition is justified and sibling relationships describe true structural kinship. If readings are measurement artifacts (ε varies with observation choice), one reading should be declared the primary constraint and others marked as observational variants, not separate constraints. This omega ensures the family structure is real rather than a labeling artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_and_observational_dependence, conceptual, 'Whether the readings are structurally distinct constraints or observational variants of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 8, 0.59).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 16, 0.64).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel basic_law_interpretive_authority. The kernel comprises three structurally distinct claims about final constitutional authority: parliamentary_sovereignty_reading (this file) asserts legislatures hold final authority through electoral accountability; judicial_supremacy_reading asserts courts hold final authority through expertise and independence; popular_constitutionalism_reading asserts constitutional meaning emerges from distributed democratic contestation rather than terminal institutional adjudication. ε-invariance principle requires separate constraints: parliamentary sovereignty produces ε=0.68 with legislature as beneficiary and minorities as victims; judicial supremacy produces lower ε with different victim/beneficiary alignment (judges as beneficiary, majorities as victims); popular constitutionalism produces yet another ε with distributed costs. Each reading has its own measurements, directionality logic, and classification. The family is linked via affects_constraints to enable network analysis of institutional coupling and contamination propagation across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(basic_law_interpretive_authority__parliamentary_sovereignty_reading, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
