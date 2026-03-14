% ============================================================================
% CONSTRAINT STORY: literacy_monopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_literacy_monopoly, []).

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
 *   constraint_id: literacy_monopoly
 *   human_readable: Literacy Monopoly: Institutional Control of Knowledge Transmission
 *   domain: social/political/educational
 *
 * SUMMARY:
 *   Literacy monopoly represents the institutional capture of knowledge
 *   transmission through standardized credentialing systems. This constraint
 *   operates at the intersection of genuine coordination (organizing literacy
 *   transmission at scale) and extractive gatekeeping (restricting access to
 *   institutional knowledge and economic participation). The constraint
 *   exhibits maximum perspectival diversity: non-literate populations
 *   experience it as a snare with no exit; institutions experience it as pure
 *   coordination; organized digital alternatives see it as a degraded system
 *   with a sunset; analytical observers risk naturalizing it as an immutable
 *   feature of complex societies. The theater ratio has risen over the
 *   measurement interval (0.35 → 0.68) as credential inflation has
 *   accelerated while actual literacy transmission function has become
 *   increasingly decoupled from formal schooling. The extractiveness
 *   trajectory (0.42 → 0.58) shows progressive accumulation of extraction:
 *   credential requirements inflate while alternative knowledge pathways
 *   remain suppressed. This is Goodhart drift in institutional form — when
 *   credentials become the goal rather than the signal, the system optimizes
 *   for credential production rather than literacy outcomes.
 *
 * KEY AGENTS:
 *   - Non-Literate Populations: Primary victim (powerless/trapped) — locked out of economic participation, legal access, and institutional resources; no structural exit options within the system
 *   - Literate Working-Class: Secondary victim (moderate/constrained) — caught in credential inflation spiral; genuine literacy is coordinated but educational costs are rising faster than skill requirements
 *   - Educational Institutions: Primary beneficiary (institutional/arbitrage) — capture economic rents through curriculum control and credential gatekeeping; have arbitrage options and organizational flexibility
 *   - Regulatory/Credentialing Bodies: Captured institutional actor (institutional/constrained) — maintain literacy standards (coordination function) but become dependent on educational incumbents; constrained exit from credential inflation logic
 *   - Elite Literate Classes: Beneficiary (powerful/mobile) — can access alternative knowledge systems and navigate institutional requirements without extraction; high bargaining power
 *   - Digital Literacy Coalition: Organized agents (organized/mobile) — open-source education platforms, Wikipedia, Khan Academy, peer-to-peer networks building exit pathways; see traditional monopoly as temporary
 *   - Traditional Schooling System: Institutional inertia (institutional/constrained) — maintains performative functions (grade cohorts, credentials, rituals) despite degraded core literacy function; persists through path dependence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(literacy_monopoly, 0.58).
domain_priors:suppression_score(literacy_monopoly, 0.72).
domain_priors:theater_ratio(literacy_monopoly, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(literacy_monopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(literacy_monopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(literacy_monopoly, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(literacy_monopoly, tangled_rope).
narrative_ontology:human_readable(literacy_monopoly, "Literacy Monopoly: Institutional Control of Knowledge Transmission").
narrative_ontology:topic_domain(literacy_monopoly, "social/political/educational").

domain_priors:requires_active_enforcement(literacy_monopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(literacy_monopoly, institutional_literacy_gatekeepers).
narrative_ontology:constraint_beneficiary(literacy_monopoly, credentialing_bodies).
narrative_ontology:constraint_victim(literacy_monopoly, non_literate_populations).
narrative_ontology:constraint_victim(literacy_monopoly, alternative_knowledge_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-LITERATE POPULATIONS (SNARE) — Structurally locked out of economic participation, political voice, and access to institutional resources. Literacy is a gate to survival: employment, legal documents, medical information, social services all require it. Exit is impossible within the system; trapped agents cannot leverage alternatives when the constraint defines access to everything. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(literacy_monopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LITERATE WORKING-CLASS POPULATIONS (TANGLED ROPE) — Gained literacy through schooling but face high costs to exit the formal credentialing system. Literacy coordination is genuine (written communication enables economic coordination). But extraction is embedded: curriculum standardization narrows alternative knowledge forms, and credential inflation creates perpetual dependency on institutional validation. Constrained exit (high cost to bypass formal education for alternative knowledge transmission) combined with real coordination benefits produces mixed experience.
constraint_indexing:constraint_classification(literacy_monopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTIONS (ROPE) — Institutions legitimately coordinate knowledge transmission through standardized literacy. From their perspective, the constraint is pure coordination: organizing collective literacy enables economic function and social cooperation. They experience low extraction because they have arbitrage options (institutions can shift curriculum, adopt new technologies) and see genuine coordination utility. Beneficiaries with structural mobility.
constraint_indexing:constraint_classification(literacy_monopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY/CREDENTIALING BODIES (TANGLED ROPE) — Institutions that set literacy standards become captured by educational incumbents. Real coordination function (standardizing literacy levels across regions enables labor mobility). But constrained exit (regulatory bodies become dependent on institutional feedback loops for legitimacy) and asymmetric extraction (credential inflation benefits credentialing bodies and educators more than students) produce tangled rope from a different institutional position than the educational institutions themselves.
constraint_indexing:constraint_classification(literacy_monopoly, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ELITE LITERATE CLASSES (ROPE) — See literacy as genuine coordination and cultural transmission, with genuine mobility. Can access alternative knowledge systems (private libraries, mentorship networks, cultural capital), adapt to new technologies, and benefit from literacy without experiencing extraction. Their perspective is genuinely cooperative because their alternatives are rich and their bargaining power is high.
constraint_indexing:constraint_classification(literacy_monopoly, rope,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DIGITAL LITERACY AND ALTERNATIVE KNOWLEDGE COALITIONS (SCAFFOLD) — Organized actors (open-source education, Wikipedia, Khan Academy, peer-to-peer learning networks, indigenous knowledge preservation movements) are creating alternative pathways that bypass formal institutional gatekeeping. See the traditional literacy monopoly as temporary: distributed digital literacy, open curricula, and non-institutional knowledge transmission are building exit pathways. Sunset horizon: institutional literacy monopoly weakens as alternative credentialing (portfolios, demonstrated skills, open certifications) mature. Low effective extraction because organized agents have agency and see exit routes.
constraint_indexing:constraint_classification(literacy_monopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: TRADITIONAL SCHOOLING APPARATUS AS INERTIAL INSTITUTION (PITON) — The formal school system persists largely through institutional inertia despite degraded function. Theater ratio is high: standardized curricula, age-based grade cohorts, credential rituals (diplomas, transcripts) are largely performative. The actual literacy function (teaching reading, writing, numeracy) could be delivered far more efficiently through digital or mentorship models. The apparatus remains because institutional actors (teachers, administrators, credentialing bodies) are dependent on it; its functions have atrophied into theater. Piton classification reflects degradation of the original coordination function into ritual maintenance.
constraint_indexing:constraint_classification(literacy_monopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE SUMMIT DETECTION) — From a universal/civilizational analytical position, one might argue literacy monopoly is a natural law: complex societies require standardized knowledge transmission, credential systems are inherent to division of labor, and gatekeeping is inevitable. This is a false summit. The structural data shows contingent institutional arrangements (formal schooling, standardized curricula, credentialing bodies), not immutable limits. The engine's false summit detector identifies this as naturalization of what is actually a tangled_rope with high extractiveness and suppression.
constraint_indexing:constraint_classification(literacy_monopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(literacy_monopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(literacy_monopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(literacy_monopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(literacy_monopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(literacy_monopoly, TR),
    TR >= 0.70.

:- end_tests(literacy_monopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The literacy monopoly extracts significant rents through credential gatekeeping, but extraction is not as severe as a pure snare because genuine coordination benefits exist (standardized literacy does enable economic function). The high point reflects credential inflation: institutional actors have progressively decoupled credentials from actual literacy requirements, increasing extraction above the coordination cost baseline. Theater ratio (0.55): Moderate-high. Traditional schooling retains genuine literacy teaching function (reading, writing, numeracy), but significant portions are theatrical: age-based grade cohorts, standardized curricula, credential rituals. Theater has increased over time as institutional responsiveness has declined and credential sorting has become the primary institutional function. Suppression (0.72): Very high. Multiple suppression mechanisms: (1) Structural — lack of alternative credentialing pathways makes formal literacy gatekeeping unavoidable; (2) Internalized — non-literate populations often internalize their exclusion as personal inadequacy; (3) Deliberate — institutional actors resist alternative credentialing systems that would reduce their monopoly power. The high suppression explains why trapped agents cannot exit: the constraint defines access to economic survival.
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests maximum perspectival gap across all three tiers. Institutional beneficiaries see rope (genuine coordination of literacy). Trapped victims see snare (pure extraction with no coordination benefit for them). Moderate constrained agents see tangled rope (real coordination with embedded extraction). Organized alternatives see scaffold (temporary system with real sunset). The piton perspective (institutional inertia) and false summit (natural law) complete the spectrum. The perspectival gap reveals that 'literacy monopoly' is actually multiple constraints layered: (1) coordination of literacy transmission (rope from institutional perspective), (2) credential gatekeeping and inflation (snare from trapped perspective), (3) institutional inertia replacing degraded function (piton), and (4) emerging alternative systems (scaffold with sunset). The same metric values produce different classifications depending on the observer's structural position because the coordination function is real but unequally distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for literacy monopoly: Institutional beneficiaries (educational institutions, credentialing bodies) derive low d (high beneficiary status + arbitrage exit → d ≈ 0.10-0.20) → low χ from their perspective → they perceive rope. Non-literate populations derive high d (victim status + trapped exit → d ≈ 0.95) → high χ → they perceive snare. Literate working-class derives moderate d (both beneficiary via access to economic coordination AND victim via credential inflation; constrained exit → d ≈ 0.55-0.65) → moderate χ → they perceive tangled rope. Organized digital alternatives derive low-moderate d (organizing agents with exit pathways; partly beneficiary from existing institutions, partly developing alternatives; mobile exit → d ≈ 0.35-0.45) → moderate χ → they perceive scaffold. The piton perspective combines institutional position (low base d) with high theater ratio (0.55+) to produce the classification gate. The false summit perspective (analytical/civilizational) risks d ≈ 0.72 (canonical analytical) → high χ, which the engine flags as inconsistent with mountain classification, revealing the natural law claim as unwarranted.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The literacy monopoly exemplifies how a single structural phenomenon produces legitimate readings across five constraint types without contradiction. Mandatrophy is resolved by recognizing that the six perspectives are not competing claims about 'the true nature' of the constraint but rather different structural readings from different positions. The beneficiary institution sees rope (coordination + arbitrage exit). The trapped non-literate sees snare (extraction + trapped exit). The constrained moderate sees tangled rope (coordination + extraction + constrained exit). The organized coalition sees scaffold (sunset + mobile exit). The inertial institution sees piton (degraded function + theater). The analytical observer sees either tangled_rope (correct) or mountain (false summit). No single perspective is 'wrong' — each is structurally accurate from its position. The mandatrophy is resolved by accepting that the constraint operates differently from different structural positions, and this difference is observable and measurable through the directionality chain. The false summit alert on the mountain perspective serves a diagnostic function: it flags naturalizing rhetoric that obscures the contingent institutional arrangements underneath.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_credential_literacy,
    'Is the suppression mechanism tied to functional literacy gaps or to credential inflation independent of actual literacy competence?',
    'Comparative analysis of literacy outcomes across formal-only vs alternative-plus-formal learners; correlation between credential possession and actual demonstrated literacy competence across contexts (employment, civic participation, knowledge application)',
    'If credential-driven: the constraint is partially performative; extractiveness should be reduced as actual literacy requirements are lower than institutional gatekeeping suggests. If functional gap dominates: extractiveness is appropriate; the monopoly reflects genuine coordination need.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(functional_vs_credential_literacy, empirical, 'Whether suppression reflects functional gaps or credential theater').

omega_variable(
    alternative_knowledge_system_viability,
    'Can non-institutionalized knowledge systems (mentorship, apprenticeship, community-based learning, indigenous knowledge transmission) deliver equivalent literacy and cognitive capabilities at scale?',
    'Historical data on pre-industrial literacy transmission; contemporary data on learning outcomes from alternative models (homeschooling, peer-to-peer networks, apprenticeship programs); longitudinal tracking of alternative-pathway completers',
    'If viable: scaffold perspective is confirmed and sunset is real; the monopoly''s suppression mechanism becomes increasingly indefensible. If not viable: coordination function is genuine and monopoly may be rational; extract higher to crane the constraint back toward rope rather than snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_knowledge_system_viability, empirical, 'Viability of non-institutional literacy transmission at scale').

omega_variable(
    credential_inflation_causality,
    'Does formal institutional literacy gatekeeping cause credential inflation, or does credential inflation create artificial demand for institutional gatekeeping?',
    'Cross-national comparison of credential inflation rates vs institutional literacy monopoly strength; temporal analysis of wage premiums for credentials vs actual literacy skill requirements; labor market analysis of credential substitution for skill testing',
    'If institutions cause inflation: the constraint is extractive; reducing institutional gatekeeping would naturally reduce credential requirements. If inflation is exogenous (labor market demand for signals): institutions are responsive rather than exploitative, reducing extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_causality, empirical, 'Directionality of credential inflation causality').

omega_variable(
    identity_lock_in_education,
    'To what degree do non-literate and under-literate populations internalize their exclusion as a personal identity limitation rather than a system barrier?',
    'Qualitative research on self-perception of literacy-locked agents; measurement of aspiration and agency shifts post-literacy acquisition; comparison of agent perception pre- and post-exit from the constraint',
    'If high identity lock: trapped exit option should perhaps shift toward identity_locked; the suppression mechanism is partially internalized. If low identity lock: trapped is appropriate; suppression is structural. This affects the classification ceiling — identity_locked creates rope potential where trapped might suggest pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_education, empirical, 'Degree of identity-based vs structural literacy lock').

omega_variable(
    digital_alternative_sufficiency,
    'As digital literacy infrastructure matures, does it provide sufficient alternative credentialing to constitute a real sunset for the institutional monopoly, or does institutional literacy retain irreplaceable gatekeeping power?',
    'Tracking of adoption rates for alternative credentials (digital certificates, portfolio-based hiring, skill verification platforms); labor market acceptance of alternative credentials vs formal diplomas; institutional resistance or adaptation to alternative credentialing systems',
    'If digital alternatives prove sufficient: scaffold sunset is real, and the constraint will measurably decay within generational timescale. If institutional literacy retains gatekeeping power: scaffold perspective is aspirational rather than structural, and the constraint persists at high extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(digital_alternative_sufficiency, empirical, 'Whether digital alternatives provide sufficient credentialing to replace institutional monopoly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(literacy_monopoly, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lit_mono_tr_t0, literacy_monopoly, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lit_mono_tr_t25, literacy_monopoly, theater_ratio, 25, 0.48).
narrative_ontology:measurement(lit_mono_tr_t50, literacy_monopoly, theater_ratio, 50, 0.55).
narrative_ontology:measurement(lit_mono_tr_t75, literacy_monopoly, theater_ratio, 75, 0.62).
narrative_ontology:measurement(lit_mono_tr_t100, literacy_monopoly, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(lit_mono_be_t0, literacy_monopoly, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lit_mono_be_t25, literacy_monopoly, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(lit_mono_be_t50, literacy_monopoly, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(lit_mono_be_t75, literacy_monopoly, base_extractiveness, 75, 0.59).
narrative_ontology:measurement(lit_mono_be_t100, literacy_monopoly, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(literacy_monopoly, information_standard).
narrative_ontology:boltzmann_floor_override(literacy_monopoly, 0.12).
narrative_ontology:affects_constraint(literacy_monopoly, credential_inflation_spiral).
narrative_ontology:affects_constraint(literacy_monopoly, alternative_knowledge_suppression).
narrative_ontology:affects_constraint(literacy_monopoly, educational_institutional_capture).

% DUAL FORMULATION NOTE:
% Literacy monopoly decomposes into three structurally distinct constraints: (1) credential_inflation_spiral (ε ≈ 0.65, snare) — pure extraction divorced from literacy function; (2) alternative_knowledge_suppression (ε ≈ 0.55, tangled_rope) — suppression of non-institutional knowledge systems alongside coordination function; (3) educational_institutional_capture (ε ≈ 0.48, tangled_rope) — regulatory bodies captured by educational incumbents. This story represents the unified constraint across all three; the decomposed stories track each mechanism separately and should be written with ε values reflecting their specific observables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(literacy_monopoly, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
