% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border Control Legitimacy Under Jurisdictional Sovereignty
 *   domain: political/legal/migration
 *
 * SUMMARY:
 *   State border control policy is justified simultaneously as protection of
 *   jurisdiction (sovereignty doctrine) and as fulfillment of humanitarian
 *   obligations (asylum law, human rights treaties). This reading of the
 *   kernel anchors legitimacy in the balance between these obligations rather
 *   than in either alone: sovereignty includes jurisdictional authority to
 *   regulate rights and obligations within territory, but does NOT include
 *   unlimited closure authority, and legitimacy requires proportionality
 *   tests, necessity justification, and public consent. The dual victim sets
 *   (excluded migrants AND displaced citizens) signal that the constraint
 *   creates extraction at both boundaries: it closes labor markets to
 *   outsiders (benefiting incumbents) while creating distribution pressure
 *   that can harm marginal citizens if admission is poorly calibrated.
 *   Enforcement is described as requiring legitimacy justification — this is
 *   the jurisdictional-sovereignty reading's distinguishing claim: closure is
 *   permitted but must be justified, not absolute.
 *
 * KEY AGENTS:
 *   - state_executive: administers border enforcement, balanced between humanitarian commitments and citizen-protection framing
 *   - incumbent_citizens: benefit from labor-market closure, bear enforcement costs, hold democratic veto over legitimacy
 *   - excluded_migrants: victims of closure, no political voice, trapped outside
 *   - displaced_citizens: victims of admission policy that may prioritize immigrants over domestic equity, drive populist backlash
 *   - international_human_rights_bodies: constrain enforcement through proportionality monitoring (weak enforcement)
 *   - asylum_claimants: straddling victim/beneficiary (nominal legal protection, actual discretion in admission)
 *   - labor_market_gatekeepers: selective beneficiaries (high-skill sectors benefit from mobility; low-skill incumbents threatened)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.68).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.72).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border Control Legitimacy Under Jurisdictional Sovereignty").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political/legal/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, '2374113a-3d30-407e-84ef-3b2c01ff521d').
narrative_ontology:cs_kernel_codification('2374113a-3d30-407e-84ef-3b2c01ff521d', distributed).
narrative_ontology:cs_authority_grounding('2374113a-3d30-407e-84ef-3b2c01ff521d', extraction).
narrative_ontology:cs_interpretation_layer_present('2374113a-3d30-407e-84ef-3b2c01ff521d').
narrative_ontology:cs_reading_relation('2374113a-3d30-407e-84ef-3b2c01ff521d', border_control_legitimacy__sovereignty_primary, coexists_with).
narrative_ontology:cs_reading_relation('2374113a-3d30-407e-84ef-3b2c01ff521d', border_control_legitimacy__freedom_of_movement_primary, coexists_with).
narrative_ontology:cs_axiom('2374113a-3d30-407e-84ef-3b2c01ff521d', foundational, closure_requires_justification).
narrative_ontology:cs_axiom_status(closure_requires_justification, holdable).
narrative_ontology:cs_axiom_grounding('2374113a-3d30-407e-84ef-3b2c01ff521d', closure_requires_justification, deontological).
narrative_ontology:cs_axiom('2374113a-3d30-407e-84ef-3b2c01ff521d', foundational, sovereignty_separable_from_closure_authority).
narrative_ontology:cs_axiom_status(sovereignty_separable_from_closure_authority, holdable).
narrative_ontology:cs_axiom_grounding('2374113a-3d30-407e-84ef-3b2c01ff521d', sovereignty_separable_from_closure_authority, deontological).
narrative_ontology:cs_reference_frame('2374113a-3d30-407e-84ef-3b2c01ff521d', balanced_jurisdictional_authority).
narrative_ontology:cs_drift_state('2374113a-3d30-407e-84ef-3b2c01ff521d', contemporary_securitization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2374113a-3d30-407e-84ef-3b2c01ff521d', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, incumbent_citizens).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, labor_market_gatekeepers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68) reflects the constraint's operation as asymmetric closure: substantial extraction from excluded migrants (denied access) and moderate extraction from marginal citizens (wage pressure, resource competition) channels to incumbent beneficiaries. Suppression is high (0.72) because enforcement machinery is necessary to maintain closure against migrant pressure and internal political opposition; asylum claimants cannot be simply ignored when they have legal claims, so coercive border enforcement, detention, deportation, and asylum-denial are routine. Theater (0.41) is moderate: proportionality and necessity language is deployed, but enforcement intensification over the interval (suppression_requirement rising from 0.58 to 0.72) suggests theater is serving as cover for stronger coercion. Accessibility collapse is moderate (0.62) because alternatives (open borders, closed borders) remain conceptually available even if politically constrained; the balancing reading does not claim alternatives are impossible, only that legitimacy requires justification. Resistance is moderate (0.58): humanitarian advocates, asylum-rights organizations, and labor advocates mounted real political resistance; incumbent-citizen resistance to high admission also constrained the other direction. The measurement series shows extractiveness rising steeply through t=25, then plateauing; theater rising continuously; and suppression rising with a similar trajectory — the pattern suggests policy-makers added coercive enforcement while dressing it in legitimacy language, the classic marker of theater rise in a tangled_rope system.
 *
 * PERSPECTIVAL GAP:
 *   State executives and nationalist constituencies perceive the constraint as legitimate coordination (democratic self-determination requires bounded membership; citizens consent to admission levels; sovereignty requires some closure). Excluded migrants and asylum-rights advocates perceive it as unjustified extraction (closure is defended by post-hoc proportionality language but enforcement is coercive and rights-violating). Displaced citizens perceive it as failure (their interests are not balanced; admission policy prioritizes immigrant narratives over domestic equity). The engine should compute these as different-seat divergences: the agenda-setter seat (state) perceives rope-like coordination; the payer seats (excluded migrants, displaced citizens) perceive snare-like extraction; the beneficiary seats (incumbents, gatekeepers) perceive beneficial rope. The claim/metric independence principle applies here: this reading is CLAIMED as justifiably balanced (tangled_rope: real coordination plus real asymmetry, both legitimate); the metrics describe substantially extractive enforcement (high suppression, rising theater), which the engine should read as potential mandate-drift or false legitimacy claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants: d = 1.0 (full target). They are trapped, powerless, and the constraint directly extracts their access to opportunity and safety. No exit options; no representation; no benefit. Displaced citizens: d = 0.65 (moderate-high target). They have some citizen power and voice, but are losing ground and not recognized by progressive elites; their resistance drives backlash but cannot block admission. Incumbent citizens: d = 0.35 (moderate beneficiary). They benefit from labor closure but bear enforcement costs and political disagreement; their consent is required but never full. Labor gatekeepers (powerful): d = 0.25 (beneficiary, with variation by sector). High-skill gatekeepers benefit from mobility; low-skill incumbents feel threatened. The state_executive: d = 0.50 (symmetric). They benefit from enforcement legitimacy and citizen support, but bear the burden of balancing impossible pressures (humanitarian + citizen + labor demands) and risk losing consent if they admit too much or too little. No stakeholder is purely symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (national self-determination and democratic control over membership) is contested. Some read it as still live (closure is needed to sustain democracy), others as partially dead (constitutional rights and property law solve the self-determination problem; closure persists for nationalist rent-seeking). The rising theater_ratio (28% to 41%) suggests that proportionality language is increasingly post-hoc: enforcement intensified (suppression requirement rose from 0.58 to 0.72) while legitimacy justifications were elaborated. This is the classic mandatrophy pattern: the constraint's operational function (defending incumbent labor-market position) diverged from its stated function (balancing protection and consent). If the founding problem (democratic self-determination) is read as substantially solved by constitutional law, the constraint should reclassify to snare (pure closure extraction with proportionality theater). If the founding problem is live, the constraint remains tangled_rope but with high mandatrophy risk. The measurement data (rising suppression despite rising proportionality language) supports the mandatrophy reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_threshold_ambiguity,
    'What level and mix of admission balances protection obligations, labor market needs, and public consent such that enforcement remains legitimate? Is there a stable equilibrium, or is the constraint inherently cyclical (admission rises, public backlash triggers restriction, humanitarian crisis drives admission again)?',
    'Comparative analysis of admission policy trajectories across democracies over 20+ years; structural modeling of feedback loops between public opinion, labor market conditions, and humanitarian pressure.',
    'If no stable equilibrium exists, the constraint is cyclical and the theater_ratio fluctuates with the cycle. If legitimacy can be stabilized at some admission level, the current high theater_ratio reflects temporary regime drift rather than structural inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_threshold_ambiguity, empirical, 'Whether the balance between obligations and consent is stable or cyclically unstable.').

omega_variable(
    citizenship_vs_residence_rights_boundary,
    'Are the dual victim sets (excluded migrants and displaced citizens) actually two faces of the same constraint, or two separate constraints? That is, does admitting more migrants necessarily displace citizen welfare access, or are these distributional failures independent of migration policy?',
    'Causal analysis isolating the effect of admission policy on citizen wage and welfare access, controlling for labor market structure, welfare funding, and fiscal policy. Compare jurisdictions with similar admission policies but different citizen-support levels.',
    'If the two victim sets are independent (admission does not necessarily displace citizens), the constraint should be split: one story about migrant exclusion, another about citizen displacement. Both victims would then reflect different constraint logics rather than dual effects of the same mechanism. If they are coupled, the dual-victim structure is structurally correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(citizenship_vs_residence_rights_boundary, empirical, 'Whether dual victimhood reflects a single constraint or two separate ones.').

omega_variable(
    kernel_reading_jurisdiction,
    'This constraint instantiates the ''jurisdictional_sovereignty'' reading of the border_control_legitimacy kernel: sovereignty is authority to regulate rights/obligations within territory, balanced against protection obligations and public consent — NOT absolute closure authority. Does this reading meaningfully distinguish itself from the ''freedom_of_movement_primary'' sibling (which emphasizes mobility rights) and the ''sovereignty_primary'' sibling (which emphasizes closure authority)? Or do the readings collapse into one another under scrutiny?',
    'Legal and political analysis: ask what each reading ALLOWS and FORBIDS that the others do not. This reading allows constrained admission policy but forbids unlimited closure or unlimited admission. Can sibling readings be distinguished by what they permit/prohibit, or do their real-world implications converge?',
    'If readings collapse, the kernel itself is under-specified (one claim, not three). If they remain distinct, this reading''s structural location (balancing rather than absolutizing) is the key differentiator — it is the middle reading between two poles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_jurisdiction, conceptual, 'Whether the three kernel readings are structurally distinct or rhetorical variants of one underlying claim.').

omega_variable(
    enforcement_proportionality_calibration,
    'The constraint is described as requiring ''proportionality and necessity tests'' on enforcement. But what counts as proportionate? Does pushback of irregular entrants at the border count? Detention of asylum seekers pending adjudication? Family separation to deter future claims? The suppression metric (0.72) implies substantial enforcement coercion, but does the described legitimacy regime actually constrain suppression, or does ''proportionality'' become a post-hoc justification for any enforcement the state chooses?',
    'Case law analysis and human rights documentation: do courts and monitoring bodies actually strike down enforcement as disproportionate, or do they consistently uphold state discretion? Are there instances where states have retreated on enforcement methods due to proportionality challenges, or does the legitimacy constraint function purely as theater?',
    'If proportionality is actually enforced, suppression should decrease over time as courts/bodies strengthen standards. If it is theater, suppression persists or rises despite proportionality language, and the theater_ratio remains high. Theater_ratio rising while formal proportionality language tightens would indicate the constraint is becoming more extractive (enforcement intensifying despite legitimacy constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_proportionality_calibration, empirical, 'Whether proportionality requirements actually constrain enforcement or function as post-hoc justification.').

omega_variable(
    consent_capture_mechanism,
    'Public consent is named as a legitimacy requirement, but is consent actually determining policy, or is it manufactured/manipulated through media and political framing? If consent is systematically inflamed against migrants through securitization rhetoric, is the constraint drawing its legitimacy from genuine consent or from manufactured backlash?',
    'Media analysis and public opinion research: track whether political rhetoric about migration security/danger precedes or follows public opinion shifts. Compare jurisdictions where political leadership emphasizes humanitarian obligations versus those emphasizing security threats; assess whether opinion differs due to rhetoric or pre-existing attitudes.',
    'If consent is manufactured, the legitimacy grounding of the constraint is unstable — real opposition could emerge if framing changed. If consent is genuine, the constraint''s legitimacy is more stable but requires constant attention to actual public preferences. High theater_ratio could reflect either scenario; temporal analysis of consent dynamics would clarify which.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_capture_mechanism, empirical, 'Whether public consent is determining policy or is being manufactured to justify predetermined policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.28).
narrative_ontology:measurement(bord_tr_t5, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 5, 0.31).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 10, 0.35).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 15, 0.38).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 20, 0.4).
narrative_ontology:measurement(bord_tr_t25, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 25, 0.41).
narrative_ontology:measurement(bord_tr_t30, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 30, 0.41).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(bord_be_t5, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(bord_be_t25, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(bord_be_t30, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bord_su_t5, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(bord_su_t25, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 25, 0.71).
narrative_ontology:measurement(bord_su_t30, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__jurisdictional_sovereignty, 0.12).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, labor_market_competition).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, asylum_law_enforcement).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, welfare_state_boundary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, national_identity_construction).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of three readings of the border_control_legitimacy kernel. The sibling readings are instantiated as separate constraint stories: sovereignty_primary (closure authority as constitutive of statehood) and freedom_of_movement_primary (mobility as overriding sovereign closure). Each reading has distinct beneficiary/victim structures and certification: this reading (jurisdictional_sovereignty) splits the victim set into excluded migrants and displaced citizens, acknowledging the constraint extracts at both boundaries. The three stories are linked by network.affects_constraints; comparison of their computed types should reveal how the three readings diverge structurally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__jurisdictional_sovereignty, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
