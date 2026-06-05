% ============================================================================
% CONSTRAINT STORY: sotu_2002_bush_terrorist_detention_and_prosecution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2002_bush_terrorist_detention_and_prosecution, []).

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
 *   constraint_id: sotu_2002_bush_terrorist_detention_and_prosecution
 *   human_readable: Detention and Prosecution of Captured Terrorists Outside Conventional POW Frameworks (SOTU 2002)
 *   domain: governance/security/international_law
 *
 * SUMMARY:
 *   The detention and prosecution framework established in the 2002 State of
 *   the Union address created a distinct legal regime for suspected
 *   terrorists captured globally. By designating detainees as outside
 *   conventional POW status (Geneva Conventions) and outside standard
 *   civilian criminal procedure (Fifth/Sixth Amendment protections), the
 *   framework generated flexibility for the executive branch in detention
 *   duration, interrogation procedures, and evidentiary standards in
 *   prosecutions. This constraint exhibits the structural signature of a
 *   snare: high extraction (suppression of due process protections), high
 *   suppression (limited alternatives for detainees, opaque designation
 *   criteria), and significant theater (military commissions appeared to
 *   offer judicial review while systematically deferring to executive
 *   designation). The extractiveness rose over the interval as the framework
 *   normalized and expanded beyond its initial scope, theater ratio increased
 *   as commissions became more elaborate without improving substantive
 *   review, and the contradiction between the humanitarian law regime's
 *   identity and its tolerance of the detention framework became visible. The
 *   constraint uniquely demonstrates the false summit risk: a regime that
 *   could reframe itself as defending universal humanitarian law instead
 *   adapted its identity to accommodate the detention innovation,
 *   illustrating how institutional self-concept can capture entire normative
 *   systems.
 *
 * KEY AGENTS:
 *   - Executive Branch Security Apparatus (Institutional/Arbitrage): Primary beneficiary — captures flexibility in detention, interrogation, prosecution; lowest cost position; manages threat designation classifications without external constraint
 *   - Detained Persons (Powerless/Trapped): Primary victims — indefinite detention without trial, limited access to legal representation, subjection to interrogation under ambiguous legal status; no exit options
 *   - International Humanitarian Law Regime (Powerless/Identity-Locked): Secondary victim (cognitive) — structurally mobile but identity-fused with post-war order; cannot reformulate protections without dissolving self-concept as U.S.-anchored foundation
 *   - Federal Judiciary (Institutional/Constrained): Institutional actor with degraded function — maintains appearance of oversight while deferring to executive designation through jurisdictional and substantive doctrines
 *   - Domestic Civil Liberties Constituency (Moderate/Constrained): Secondary victim-beneficiary — bears costs of degraded procedural protections but benefits from rule-of-law transparency advocacy; constrained by post-9/11 security consensus
 *   - International Human Rights Coalition (Organized/Constrained): Partially organized resistance — litigates detainee cases, applies diplomatic pressure, sets counter-norms; constrained by resource limitations and political isolation from security-prioritizing states
 *   - Analytical Observer (Analytical/Analytical): Risks naturalizing state security authority as immutable feature of governance, obscuring the institutional-legal innovation as contingent extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2002_bush_terrorist_detention_and_prosecution, 0.68).
domain_priors:suppression_score(sotu_2002_bush_terrorist_detention_and_prosecution, 0.78).
domain_priors:theater_ratio(sotu_2002_bush_terrorist_detention_and_prosecution, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2002_bush_terrorist_detention_and_prosecution, extractiveness, 0.68).
narrative_ontology:constraint_metric(sotu_2002_bush_terrorist_detention_and_prosecution, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(sotu_2002_bush_terrorist_detention_and_prosecution, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2002_bush_terrorist_detention_and_prosecution, snare).
narrative_ontology:human_readable(sotu_2002_bush_terrorist_detention_and_prosecution, "Detention and Prosecution of Captured Terrorists Outside Conventional POW Frameworks (SOTU 2002)").
narrative_ontology:topic_domain(sotu_2002_bush_terrorist_detention_and_prosecution, "governance/security/international_law").

domain_priors:requires_active_enforcement(sotu_2002_bush_terrorist_detention_and_prosecution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2002_bush_terrorist_detention_and_prosecution, executive_branch_security_apparatus).
narrative_ontology:constraint_victim(sotu_2002_bush_terrorist_detention_and_prosecution, detained_persons).
narrative_ontology:constraint_victim(sotu_2002_bush_terrorist_detention_and_prosecution, international_humanitarian_law_regime).
narrative_ontology:constraint_victim(sotu_2002_bush_terrorist_detention_and_prosecution, procedural_due_process_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DETAINED PERSON (SNARE) — Classified outside conventional POW status; no access to military tribunal protections or civilian criminal procedural guarantees; indefinite detention without trial. Maximum extraction: subject to state power with minimal legal recourse. Biological citizenship is the only exit option and state controls that too.
constraint_indexing:constraint_classification(sotu_2002_bush_terrorist_detention_and_prosecution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL HUMANITARIAN LAW REGIME (SNARE / IDENTITY-LOCKED) — The Geneva Conventions framework is structurally mobile (states could renegotiate; the regime could adapt) but cognitively captured by the U.S. institutional position: the regime's identity is constituted through the U.S. security apparatus as the primary guarantor of post-WWII order. Renegotiating protections would require the regime to perceive the U.S. as violating rather than defending the order. Identity-locked at generational horizon — the regime cannot reformulate protections without dissolving its self-concept as the foundation of post-war legality.
constraint_indexing:constraint_classification(sotu_2002_bush_terrorist_detention_and_prosecution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: EXECUTIVE BRANCH SECURITY APPARATUS (ROPE) — Sees the constraint as coordination: flexible detention and interrogation procedures enable rapid response to evolving terrorist networks. The apparatus experiences the framework as solving the problem of how to manage high-value detainees without triggering conventional POW status costs (transport, transparency, lawyer access). Net beneficiary with arbitrage options — can shift threat designations, reclassify detainees, or export detention to third-party regimes. Low or negative experienced extraction.
constraint_indexing:constraint_classification(sotu_2002_bush_terrorist_detention_and_prosecution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC CIVIL LIBERTIES CONSTITUENCY (TANGLED ROPE) — Constrained by post-9/11 security consensus and public fear; faces political cost of opposing detention framework but also benefits from rule-of-law transparency mechanisms (FOIA, Congressional oversight). Genuine coordination function (security with due process) coexists with asymmetric extraction (procedural protections systematically degraded). Mixed position: some agency through litigation and legislative advocacy, but underlying power asymmetry favors executive detention authority.
constraint_indexing:constraint_classification(sotu_2002_bush_terrorist_detention_and_prosecution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL JUDICIARY (PITON) — Early in the interval (2002-2006), courts largely declined jurisdiction over detainee challenges, citing military deference doctrines and the extraterritorial status of Guantanamo. The judicial review function is performative — courts maintain the appearance of oversight while systematically deferring to executive designation. Theater_ratio ≥ 0.65 reflects that judicial proceedings exist but substantive review is degraded. By interval end (Boumediene 2008), the piton begins to degrade toward functional review, but the institutional machinery persists through inertia even as its primary role (rubber-stamping detention) becomes visible.
constraint_indexing:constraint_classification(sotu_2002_bush_terrorist_detention_and_prosecution, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL HUMAN RIGHTS COALITION (SCAFFOLD) — Organized actors (NGOs, foreign governments, UN bodies) see the detention framework as a temporary security measure with an implicit sunset: as terrorist networks degraded and threat perception normalized, the extraordinary detention regime would expire. The coalition has agency through litigation, diplomatic pressure, and norm-setting. Constraints on exit are real (political isolation, resource limitations) but not insurmountable. The framework's extractiveness declines as threat narrative weakens, making this a contingent scaffold rather than permanent structure.
constraint_indexing:constraint_classification(sotu_2002_bush_terrorist_detention_and_prosecution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STATE NATURALIZATION (SNARE) — From a civilizational view, this perspective risks naturalizing state security authority as an immutable feature of governance: 'states must retain flexibility to detain threats; procedural protections necessarily reduce security options; the detainee classification is an unavoidable cost of protecting the majority.' This framing treats the institutional-legal innovation (creating a third legal category outside military/civilian frameworks) as a discovery of sovereign necessity rather than a contingent extraction mechanism. The engine's false summit detector will flag this as naturalization of institutional choice.
constraint_indexing:constraint_classification(sotu_2002_bush_terrorist_detention_and_prosecution, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2002_bush_terrorist_detention_and_prosecution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2002_bush_terrorist_detention_and_prosecution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2002_bush_terrorist_detention_and_prosecution, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2002_bush_terrorist_detention_and_prosecution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2002_bush_terrorist_detention_and_prosecution, TR),
    TR >= 0.70.

:- end_tests(sotu_2002_bush_terrorist_detention_and_prosecution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The framework extracts significant value from detained persons (indefinite detention, limited legal protections, restricted contact) and from the international humanitarian law regime (normative capture — the regime tolerates violations of its own principles). The extraction is not total (snare at ε ≥ 0.46 is confirmed) because there exists some procedural framework (military commissions, appeals processes), albeit degraded. Compared to pure chattel slavery (ε approaching 1.0), the detention regime permits limited legal argument, occasional judicial review, and possibility of eventual release. The extraction rises over the interval (0.55 → 0.72) as the framework normalizes, institutional resistance weakens, and the number of detainees grows — this trajectory supports the extraction accumulation hypothesis. Suppression (0.78): High. Multiple layers eliminate alternatives for detainees: (1) physical confinement at extraterritorial site; (2) designation as terrorist eliminates civilian court jurisdiction; (3) lack of attorney access prevents legal challenge to designation; (4) indefinite detention without trial deadline; (5) interrogation under ambiguous legal status prevents reliable self-advocacy. The suppression is structural (material barriers) rather than internalized, placing detainees in the 'trapped' exit category. Theater ratio (0.65): Moderate-high. Military commissions existed as procedural theater — they performed judicial function (rules of evidence, appellate review) while systematically deferring to executive threat designation. The commissions' actual effect was to rubber-stamp detention; their appearance was to legitimate it through legal process. The rise in theater_ratio (0.52 → 0.68) reflects elaboration of commission procedures without increase in substantive review capacity. Boumediene (2008) reduced theater somewhat by enabling habeas corpus review, but the underlying extraction mechanism persisted through alternative designations and continued detention.
 *
 * PERSPECTIVAL GAP:
 *   The detainee (powerless/trapped) sees Snare; the executive (institutional/arbitrage) sees Rope. This is the fundamental gap. One agent experiences the constraint as pure extraction with no alternatives; the other experiences it as efficient coordination with maximum flexibility. From the detainee's perspective, extractiveness is experienced at the full f(d) amplification (d ≈ 1.0 → f(d) ≈ 1.42). From the executive's perspective, extractiveness is dampened or inverted (d ≈ 0.05 → f(d) ≈ -0.12). The chi formula χ = ε × f(d) × σ(S) produces radically different values for the same base ε: at global scope (σ=1.2), detainee experiences χ ≈ 0.68 × 1.42 × 1.2 ≈ 1.16 (exceeds maximum, indicating asymmetric vulnerability); executive experiences χ ≈ 0.68 × (-0.12) × 1.2 ≈ -0.098 (negative, indicating net benefit). The perspectival gap is not observational ambiguity but structural asymmetry of extraction — the same constraint produces opposite directional flows for different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to extraction flow. Detainees: d ≈ 1.0 (full targets). Their exit options are trapped; they are classified as victims; they experience maximum suppression. The sigmoid f(d) at d=1.0 produces f(1.0) ≈ 1.42, amplifying experienced extractiveness. Executives with arbitrage options: d ≈ 0.05 (full beneficiaries). They can reclassify detainees, export detention to third parties, or adjust procedures — maximum exit flexibility. The sigmoid f(d) at d=0.05 produces f(0.05) ≈ -0.12, producing negative or minimal effective extraction (they experience coordination benefit). The judiciary at d ≈ 0.50 (symmetric) with constrained exit: experiences moderate extraction through institutional conflict — genuinely wants to review but institutionally constrained by deference doctrines. Identity-locked agents (humanitarian law regime): d ≈ 0.85 (high target status) because they bear cognitive cost of identity-dissonance. They perceive mobility (structural argument, they could protest) but cannot exercise it (identity fusion prevents action). At biographical time horizon, identity-locked produces Rope (perceive constraint as changeable in principle) rather than Mountain — the regime could reformulate but won't, making the binding mechanism cognitive rather than material.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint (ε=0.68 > 0.70 threshold) requires mandatrophy resolution per schema. The mandatrophy in the detention framework is the apparent contradiction between Snare classification (pure extraction) and the existence of procedural mechanisms (military commissions, appeals, eventual trials). The resolution: the procedures are not evidence of coordination; they are evidence of theater. The extraction is real; the coordination is performed. The six-perspective analysis demonstrates that all six types are legitimate depending on viewing position, but the system is fundamentally a snare for the primary victim (detainee). The mandatrophy is resolved by distinguishing structural extraction (detainee → executive apparatus) from institutional theater (commissions appearing to review while deferring). The framework persists not because it coordinates security with due process (that would be Tangled Rope) but because the executive apparatus benefits from flexibility (Rope for them) and the detainee has no exit (Snare for them). The mandatrophy confirms rather than contradicts the Snare classification: the existence of theater is the mechanism by which the snare persists — it legitimates extraction by performing oversight.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_classification_criteria,
    'What objective criteria distinguish a ''terrorist'' requiring extraordinary detention from a conventional combatant or criminal suspect?',
    'Comparative analysis of designation decisions; tracking of reclassifications and appeals; correlation between threat assessments and eventual trial outcomes or recidivism after release',
    'If criteria are clear and consistently applied: detention framework is coordinative classification system (Rope from more perspectives). If criteria are opaque and arbitrarily applied: framework is pure extraction mechanism (Snare confirmed). If criteria drift over time: supports extraction accumulation hypothesis (theater_ratio rising).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(threat_classification_criteria, empirical, 'Objectivity and consistency of terrorist designation criteria').

omega_variable(
    procedural_degradation_causality,
    'Do procedural protections degrade due to structural security requirements or due to institutional incentive capture (prosecutors benefiting from reduced evidentiary burden)?',
    'Comparison of conviction rates and evidence quality in military commissions vs conventional courts; tracking of prosecution success metrics and incentive structures; analysis of prosecutorial decision-making post-detention',
    'If structural: suppression value justified by genuine security-procedure tradeoff (suppression ≤ 0.65, framework legitimate if transparently debated). If capture: suppression value reflects extraction mechanism (suppression ≥ 0.75, framework is snare with cosmetic oversight). Directionality for prosecutors shifts from neutral arbiter to institutional beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(procedural_degradation_causality, empirical, 'Whether procedural degradation reflects structural security needs or institutional incentive capture').

omega_variable(
    extraterritorial_precedent_containment,
    'Does U.S. detention authority remain confined to Guantanamo Bay and explicit legal framework, or does the precedent enable expansion to other locations and implicit detention regimes?',
    'Mapping of CIA black sites and extraordinary rendition locations; comparison of detention populations across venues; analysis of legal justifications offered for expanded detention programs',
    'If contained: extractiveness value reflects bounded framework (ε ≈ 0.68 for Guantanamo specifically). If expanded: extractiveness is systematically underestimated — suppression rises as hidden detention networks multiply, framework becomes pure snare with no transparency. Scope shifts from national (Guantanamo visible) to global (network of secret facilities).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraterritorial_precedent_containment, empirical, 'Whether detention framework precedent remains bounded or expands to hidden detention networks').

omega_variable(
    identity_locked_binding_mechanism_ihrl,
    'Is the international humanitarian law regime''s failure to contest the detention framework a structural constraint (U.S. dominance in post-war order) or an identity-locked capture (regime''s self-concept depends on U.S. as guarantor)?',
    'Counterfactual analysis: how would the regime respond if a non-hegemon created equivalent detention framework? Discourse analysis of regime justifications for tolerance; tracking of regime position changes as U.S. power shifts',
    'If structural: regime needs formal renegotiation and major-power agreement to establish new protections. If identity-locked: regime''s perception could shift rapidly if self-concept reformulates (e.g., if hegemon image degrades) — sudden change in position despite unchanged material power. Identity-lock hypothesis predicts that regime will eventually move decisively once reframing occurs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_binding_mechanism_ihrl, conceptual, 'Whether IHRL regime failure is structural constraint or identity-locked capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2002_bush_terrorist_detention_and_prosecution, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_terror_theater_t0, sotu_2002_bush_terrorist_detention_and_prosecution, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sotu_terror_theater_t3, sotu_2002_bush_terrorist_detention_and_prosecution, theater_ratio, 3, 0.62).
narrative_ontology:measurement(sotu_terror_theater_t6, sotu_2002_bush_terrorist_detention_and_prosecution, theater_ratio, 6, 0.65).
narrative_ontology:measurement(sotu_terror_theater_t9, sotu_2002_bush_terrorist_detention_and_prosecution, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(sotu_terror_extract_t0, sotu_2002_bush_terrorist_detention_and_prosecution, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sotu_terror_extract_t3, sotu_2002_bush_terrorist_detention_and_prosecution, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(sotu_terror_extract_t6, sotu_2002_bush_terrorist_detention_and_prosecution, base_extractiveness, 6, 0.72).
narrative_ontology:measurement(sotu_terror_extract_t9, sotu_2002_bush_terrorist_detention_and_prosecution, base_extractiveness, 9, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2002_bush_terrorist_detention_and_prosecution, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(sotu_2002_bush_terrorist_detention_and_prosecution, 0.18).
narrative_ontology:affects_constraint(sotu_2002_bush_terrorist_detention_and_prosecution, extraordinary_rendition_cia_black_sites).
narrative_ontology:affects_constraint(sotu_2002_bush_terrorist_detention_and_prosecution, interrogation_enhanced_techniques_legalization).
narrative_ontology:affects_constraint(sotu_2002_bush_terrorist_detention_and_prosecution, post_911_surveillance_expansion).
narrative_ontology:affects_constraint(sotu_2002_bush_terrorist_detention_and_prosecution, war_on_terror_indefinite_detention_precedent).

% DUAL FORMULATION NOTE:
% The detention framework decomposes into distinct structural constraints with different ε values: (1) the legal classification innovation (creating third legal category) — ε ≈ 0.68, Snare, this story; (2) the interrogation procedures within detention — ε ≈ 0.72, Snare, separate story with higher suppression due to torture precedent; (3) the CIA extraordinary rendition network — ε ≈ 0.85, Snare, separate story with maximum suppression due to hidden detention sites. Each constraint has independent beneficiaries (executive security apparatus), victims (detained persons, international law regime), and measurement trajectories. The network affects_constraints links reflect causal dependency: the legal classification enables interrogation procedures which enable extraordinary rendition. The detention framework is upstream; rendition network is downstream and more extractive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2002_bush_terrorist_detention_and_prosecution, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
