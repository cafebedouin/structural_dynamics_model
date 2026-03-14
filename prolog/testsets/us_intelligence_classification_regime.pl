% ============================================================================
% CONSTRAINT STORY: us_intelligence_classification_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_intelligence_classification_regime, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: us_intelligence_classification_regime
 *   human_readable: US Intelligence Classification Regime
 *   domain: governance/national_security/institutional_power
 *
 * SUMMARY:
 *   The US intelligence classification regime is a system of legal and
 *   institutional mechanisms for controlling information about government
 *   operations, allegedly designed to protect national security. The regime
 *   functions through original classification authority (granted to
 *   designated officials), derivative classification (cascading restriction
 *   from classified sources), and criminal penalties for unauthorized
 *   disclosure. Structurally, classification represents a transfer of power
 *   from democratic institutions (legislatures, courts, public) to
 *   executive-branch intelligence agencies. Citizens cannot know what
 *   government does in their name; Congress lacks full access to classified
 *   operations it ostensibly oversees; courts defer to executive
 *   classification judgments; journalists face criminal liability for
 *   reporting; historians cannot document recent history; and oversight is
 *   rendered meaningless because it operates in secret. The constraint has
 *   metastasized since its post-WWII origins: the volume of classified
 *   material has expanded exponentially; classification timelines have
 *   extended indefinitely; declassification authority has been concentrated
 *   in agencies with interest in keeping information hidden; and legal
 *   penalties for unauthorized disclosure have increased. The regime persists
 *   not primarily because of legitimate operational security needs (though
 *   some such needs exist) but because it serves the institutional interests
 *   of intelligence agencies and executive power consolidation.
 *
 * KEY AGENTS:
 *   - Intelligence Agencies: Primary beneficiary (institutional/arbitrage) — control classification authority, determine what public learns, concentrate power in executive branch; benefit from secrecy without corresponding accountability.
 *   - Public/Citizens: Primary victim (powerless/trapped) — trapped by citizenship; cannot access information about government actions; cannot exit or negotiate; bear costs of secret state action.
 *   - Investigative Journalists: Secondary victim (moderate/constrained) — face criminal liability for publishing classified information; constrained by organizational risk and source protection; cannot fully investigate government.
 *   - Congressional Oversight Committees: Mixed actor (moderate/constrained) — have limited legitimate security needs met by classification, but experience severe information gatekeeping and power asymmetry; cannot publicly discuss findings; dependent on agency cooperation.
 *   - Declassification Reform Movement: Organized agent (organized/constrained) — advocates for automatic declassification, shorter timelines, mandatory review; sees sunset logic as achievable but institutional resistance prevents implementation.
 *   - Classification Bureaucracy: Institutional actor (institutional/arbitrage) — manages classification system; perpetuates through process compliance and reflexive over-classification; acknowledges system is degraded but perpetuates through inertia.
 *   - Analytical Observer: Civilizational position (analytical/analytical) — sees classification regime as structural extraction mechanism that concentrates power, prevents accountability, and criminalizes transparency.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_intelligence_classification_regime, 0.68).
domain_priors:suppression_score(us_intelligence_classification_regime, 0.75).
domain_priors:theater_ratio(us_intelligence_classification_regime, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_intelligence_classification_regime, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_intelligence_classification_regime, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_intelligence_classification_regime, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_intelligence_classification_regime, snare).
narrative_ontology:human_readable(us_intelligence_classification_regime, "US Intelligence Classification Regime").
narrative_ontology:topic_domain(us_intelligence_classification_regime, "governance/national_security/institutional_power").

domain_priors:requires_active_enforcement(us_intelligence_classification_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_intelligence_classification_regime, intelligence_agencies).
narrative_ontology:constraint_beneficiary(us_intelligence_classification_regime, executive_branch).
narrative_ontology:constraint_victim(us_intelligence_classification_regime, public_epistemic_commons).
narrative_ontology:constraint_victim(us_intelligence_classification_regime, congressional_oversight).
narrative_ontology:constraint_victim(us_intelligence_classification_regime, press_freedom).
narrative_ontology:constraint_victim(us_intelligence_classification_regime, citizens_access_to_information).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUPPRESSED CITIZEN (SNARE) — Trapped by classification regime. Cannot access information about government actions affecting them. No exit: citizenship is not voluntary, and information access rights are restricted by law. Maximal extraction — bears costs of secret state action with zero transparency and zero negotiating power.
constraint_indexing:constraint_classification(us_intelligence_classification_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INVESTIGATIVE JOURNALIST (SNARE) — Constrained by criminal penalties for publishing classified information and organizational risk (employer liability, source exposure). Cannot voluntarily exit journalism without career cost. Extraction is severe: suppression of reporting, imprisonment risk for accessing or publishing information, forced choice between public interest and legal liability.
constraint_indexing:constraint_classification(us_intelligence_classification_regime, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESSIONAL OVERSIGHT COMMITTEE (TANGLED ROPE) — Genuine coordination function: classification prevents adversaries from learning capabilities, methods, sources. Legitimate need to protect operational security. But asymmetric extraction: committees are clearance-dependent and information-gated; committee members cannot publicly discuss what they learn; intelligence agencies control the information flow to committees; classification authority is executive-branch-dominated. Mixed experience: some coordination benefit (national security) alongside significant asymmetric extraction (information gatekeeping, power imbalance).
constraint_indexing:constraint_classification(us_intelligence_classification_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTELLIGENCE AGENCY (ROPE) — Primary beneficiary. Experiences classification as pure coordination mechanism: protecting sources, methods, operational security. No extraction experienced because the agency controls the classification authority and the benefits flow entirely toward it. Can selectively declassify to serve institutional interests (FOIA exceptions, authorized leaks, strategic releases). Exit is not relevant — the agency controls the regime entirely.
constraint_indexing:constraint_classification(us_intelligence_classification_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DECLASSIFICATION REFORM MOVEMENT (SCAFFOLD) — Organized actors (historical societies, FOIA advocates, progressive policymakers) see classification as a temporary coordination failure with sunset logic. Automatic declassification timelines (20-year rule, 25-year rule, lifetime caps), mandatory review on request, and categorical declassification demonstrate that the regime could be bounded. The reform movement sees this as a scaffold that should transition to narrower, more targeted classification with higher transparency. However, indefinite classification expansion and agency resistance show the sunset clause is not functioning — the regime has expanded beyond its original scope.
constraint_indexing:constraint_classification(us_intelligence_classification_regime, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CLASSIFICATION BUREAUCRACY (PITON) — The apparatus of classification (original classification authority, mandatory declassification review, classification guides, derivative classification) has largely become theater. Millions of documents are classified; vast majority will never be reviewed for actual security content; bureaucracy perpetuates itself through process compliance rather than security function. Classification guides accumulate rather than simplify. New documents are reflexively classified under predecessor-classification logic. The system sees its own procedures as degraded (everyone acknowledges over-classification) but persists through institutional inertia and career incentive structures (classifying is safer than declassifying).
constraint_indexing:constraint_classification(us_intelligence_classification_regime, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, the classification regime functions as a structural extraction mechanism that (1) concentrates power in executive branch, (2) prevents democratic accountability, (3) suppresses public knowledge of government actions affecting millions, and (4) is sustained by legal and institutional structures that make exit structurally impossible. The regime has metastasized beyond legitimate security needs into a tool of institutional power consolidation. Classification law criminalizes reporting, legislation has become hostage to security classification, and courts defer to executive judgment on classification status. The constraint persists not because it solves a coordination problem but because power holders benefit from opacity.
constraint_indexing:constraint_classification(us_intelligence_classification_regime, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_intelligence_classification_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_intelligence_classification_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_intelligence_classification_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_intelligence_classification_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_intelligence_classification_regime, TR),
    TR >= 0.70.

:- end_tests(us_intelligence_classification_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The regime systematically transfers knowledge from public/legislators/courts to executive intelligence agencies, preventing democratic accountability and enabling unilateral executive action. This is extraction in the classical sense: asymmetric benefit (agencies gain power) and asymmetric cost (public loses knowledge and oversight capacity). The extractiveness is not maximum (0.90+) because some legitimate security functions exist and because partial oversight mechanisms function (committees have access, though limited). The trajectory from 0.42 to 0.68 over 40 years reflects the metastasis of classification beyond legitimate security into a tool of institutional power consolidation. Suppression (0.75): Very high. Criminal penalties for unauthorized disclosure, institutional cultures of secrecy, organizational risk of source exposure, and internalized self-censorship create multiple layers of suppression. Citizens face legal barriers to information access; journalists face criminal liability; organizations face institutional risk; bureaucratic compartmentalization prevents knowledge-sharing even within government. Suppression is not at maximum because some classified information does enter the public domain (leaks, authorized releases, FOIA) and legal mechanisms for classification review exist (though rarely successful). Theater ratio (0.62): Moderate-high. The classification bureaucracy (original classification authority, derivative classification guides, mandatory review procedures) has become substantially performative. Over-classification is endemic and acknowledged; vast majority of classified material will never be reviewed; classification guides accumulate rather than clarify; reflexive classification is safer than declassification; procedures serve to maintain secrecy rather than to identify legitimately sensitive information. Theater has increased over time as the system has expanded and the disconnect between security rationale and actual practice has widened.
 *
 * PERSPECTIVAL GAP:
 *   The regime achieves maximum perspectival divergence because the same structural mechanism (controlling information about government operations) appears as legitimate security protection to beneficiaries and as institutional power consolidation to victims. Intelligence agencies and executive branch perceive genuine security coordination function — protecting sources, methods, operational security. Citizens and journalists perceive oppressive institutional control — inability to know what government does, criminal penalties for reporting, suppression of accountability mechanisms. Congressional oversight perceives a mixed system — some coordination value alongside structural information disadvantage. Reformers perceive a fixable temporary arrangement if political will exists. Bureaucracy perceives its own degraded ritual. The analytical observer perceives structural extraction masked by security rationale. These are not different interpretations of the same phenomenon — they are different phenomenological experiences reflecting different structural positions. The constraint produces genuinely incompatible realities depending on where one stands.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation for each perspective flows from structural position: power level, exit options, and beneficiary/victim status. Intelligence agencies as institutional beneficiaries with arbitrage options derive low d (full beneficiary position) → negative f(d) → they experience the constraint as beneficial coordination. Citizens/public as powerless victims with trapped exit derive high d (full target position) → high f(d) → they experience maximum extraction chi. Journalists as moderate-power victims with constrained exit (career risk, legal liability) derive d ≈ 0.75 → high f(d). Congressional committees as institutional but constrained agents derive intermediate d reflecting mixed benefit (some security function) and victim status (information gatekeeping). The directive flow shows systematic asymmetry: benefits concentrate at institutional level with power; costs distribute downward to powerless and moderate agents with no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The classification regime is not genuinely ambiguous between extraction and coordination — it is genuinely both. The coordination function (protecting sources, methods, operational security) is real but narrow in scope and duration; the extraction function (controlling public information, preventing accountability, concentrating executive power) is real and expanding in scope. The constraint qualifies as snare precisely because the extraction function dominates the coordination function structurally. A snare exhibits extraction that exceeds legitimate coordination cost — this regime shows extractiveness (0.68) substantially higher than would be necessary for genuine operational security. The theater ratio (0.62) and trajectory (increasing from 0.35 to 0.62) indicate the classification system has metastasized beyond security function into institutional power preservation. The piton perspective (bureaucracy sees its own degradation) combined with snare/tangled-rope perspectives from other agents confirms the mandatrophy: the regime is a snare (extractive) that maintains itself through performance (piton theater) and presents itself as coordination when it suits institutional interests. The saddle point is that all perspectives are analytically correct from their structural positions — there is no single 'true' type, only a presheaf of types over observation sites. The constraint is coherently classified as snare because the extractiveness dominates and the suppression confirms that agents lack exit and negotiation power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimate_security_scope,
    'What proportion of actual classified information represents legitimate operational security vs. institutional power preservation?',
    'Historical analysis of declassified information: correlation between original classification rationale and actual revealed content. Comparison of security outcomes under different classification regimes (allied nations with shorter timelines, lower classification volumes).',
    'If legitimate proportion > 70%: classification is primarily coordination mechanism; reclassify snares as tangled_rope. If < 40%: classification is primarily extraction mechanism; current snare classification confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimate_security_scope, empirical, 'Proportion of classification serving legitimate security vs institutional power').

omega_variable(
    declassification_counterfactual,
    'What would change if automatic declassification timelines were enforced uniformly and executive discretion were eliminated?',
    'Controlled comparison: analyze FOIA mandatory declassification review outcomes (items released under legal requirement vs. items withheld under discretion). Examine allied intelligence practices under more restrictive classification regimes.',
    'If major reforms change extraction patterns: classification regime is contingent institutional arrangement (scaffold/snare). If extraction persists through alternative gatekeeping: suppression is structural rather than regime-dependent (snare classification robust).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declassification_counterfactual, empirical, 'Outcomes under enforced declassification vs discretionary gatekeeping').

omega_variable(
    organizational_identity_lock,
    'Are intelligence agencies structurally dependent on classification authority for institutional identity and survival, creating identity-locked perpetuation independent of security needs?',
    'Organizational analysis: can agencies articulate mission/function without reference to classified operations? Historical analysis of agency advocacy for expanding classification authority. Comparative analysis of intelligence services in regimes with mandatory declassification.',
    'If identity-locked: agencies will resist declassification reflexively and expand classification to defend organizational scope. Explains piton persistence despite acknowledged over-classification. Affects whether sunset clause can ever function (agent is identity-fused with the constraint).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_identity_lock, conceptual, 'Whether intelligence agencies are identity-locked to classification authority').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of public information primarily structural (legal penalties, institutional barriers) or internalized (self-censorship, institutional cultures that treat classified=hidden)?',
    'Media analysis of self-censorship patterns; comparison of reporting before/after legal clarification of source protection. Analysis of organizational cultures in different intelligence/media settings.',
    'If primarily structural: legal reform could reduce suppression significantly. If internalized: suppression persists after legal barriers removed; requires cultural/identity-level intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is structural barriers or internalized cultural patterns').

omega_variable(
    counterintelligence_legitimacy_threshold,
    'Do actual counterintelligence benefits of classification (preventing adversary learning of methods/sources) justify the suppression of unrelated historical, policy, or operational information?',
    'Intelligence analysis of compartmentalization efficacy: what fraction of classified material requires classification for operational security vs. could be declassified without compromising active operations? Historical examples of over-broad classification preventing legitimate inquiry.',
    'If threshold is crossed (non-essential material classified): classification regime is using security rationale as pretext for broader institutional power. Confirms snare/piton classification from institutional perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterintelligence_legitimacy_threshold, empirical, 'Whether actual counterintelligence needs justify scope of classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_intelligence_classification_regime, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uscr_tr_t0, us_intelligence_classification_regime, theater_ratio, 0, 0.35).
narrative_ontology:measurement(uscr_tr_t20, us_intelligence_classification_regime, theater_ratio, 20, 0.5).
narrative_ontology:measurement(uscr_tr_t40, us_intelligence_classification_regime, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(uscr_be_t0, us_intelligence_classification_regime, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(uscr_be_t20, us_intelligence_classification_regime, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(uscr_be_t40, us_intelligence_classification_regime, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_intelligence_classification_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_intelligence_classification_regime, 0.15).
narrative_ontology:affects_constraint(us_intelligence_classification_regime, freedom_of_the_press).
narrative_ontology:affects_constraint(us_intelligence_classification_regime, democratic_accountability).
narrative_ontology:affects_constraint(us_intelligence_classification_regime, executive_power_concentration).
narrative_ontology:affects_constraint(us_intelligence_classification_regime, public_knowledge_access).

% DUAL FORMULATION NOTE:
% The classification regime can be decomposed into at least three structurally distinct constraints: (1) legitimate counterintelligence security (ε ≈ 0.15, mountain/rope) — protecting active sources and ongoing operations; (2) historical information gatekeeping (ε ≈ 0.55, tangled_rope) — controlling narrative about past government actions; (3) executive power concentration (ε ≈ 0.72, snare) — using classification authority to prevent legislative and judicial oversight. This story treats the regime as a unified system to show how different ε values coexist in institutional structures that nominally serve a single function but are actually composed of distinct extraction and coordination mechanisms operating at different timescales.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_intelligence_classification_regime, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
