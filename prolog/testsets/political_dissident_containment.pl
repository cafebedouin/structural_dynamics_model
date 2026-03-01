% ============================================================================
% CONSTRAINT STORY: political_dissident_containment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_dissident_containment, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: political_dissident_containment
 *   human_readable: The state carceral system for neutralizing political opposition
 *   domain: political/authoritarian_control
 *
 * SUMMARY:
 *   The death of Russian opposition leader Alexei Navalny in the Arctic penal
 *   colony at Kharp in February 2024 exemplifies a structural constraint in
 *   authoritarian systems: the legal and carceral framework used to eliminate
 *   political opposition while maintaining a facade of due process. This
 *   constraint operates through multiple layers: vague extremism statutes
 *   that criminalize opposition activity, a judiciary subordinate to the
 *   security apparatus, remote prison placement that isolates dissidents from
 *   public support and medical care, and harsh conditions that function as
 *   slow attrition. The constraint exhibits a critical feature of Snare
 *   classification: it combines legal theater (trials, appeals, formal
 *   procedures) with substantive incapacitation (imprisonment, isolation,
 *   deteriorating health). The classification demonstrates how mandatrophy is
 *   resolved: the constraint is not a coordination mechanism (no genuine
 *   public benefit from dissident elimination) and not a mountain (the
 *   carceral system is a choice, not an immutable law of political order). It
 *   is a pure extraction mechanism that neutralizes political threats through
 *   state violence disguised as legal process.
 *
 * KEY AGENTS:
 *   - Political Dissidents: Primary victim (powerless/trapped) — face predetermined conviction, remote imprisonment, and deteriorating conditions with no exit options or legal recourse
 *   - Ruling Party Regime and Security Apparatus: Primary beneficiary (institutional/arbitrage) — maintains political monopoly through dissident elimination; uses carceral system for opposition neutralization without triggering regime-collapse-level international sanctions
 *   - Opposition Parties and Civil Society: Secondary victim (moderate/constrained) — diverted resources toward legal defense and organizational survival; face preemptive suppression through extremism charges
 *   - State Judicial System: Institutional actor maintaining theater (institutional/arbitrage) — operates as subordinate apparatus delivering predetermined verdicts while maintaining appearance of legal legitimacy
 *   - International Human Rights Community: Organized observer (organized/mobile) — documents violations but has limited capacity to alter regime behavior; advocacy sometimes instrumentalized for geopolitical positioning
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing regime violence as inherent to state sovereignty rather than recognizing it as a contingent choice to eliminate opposition through extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_dissident_containment, 0.78).
domain_priors:suppression_score(political_dissident_containment, 0.88).
domain_priors:theater_ratio(political_dissident_containment, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_dissident_containment, extractiveness, 0.78).
narrative_ontology:constraint_metric(political_dissident_containment, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(political_dissident_containment, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_dissident_containment, snare).
narrative_ontology:human_readable(political_dissident_containment, "The state carceral system for neutralizing political opposition").
narrative_ontology:topic_domain(political_dissident_containment, "political/authoritarian_control").

domain_priors:requires_active_enforcement(political_dissident_containment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_dissident_containment, ruling_party_regime).
narrative_ontology:constraint_beneficiary(political_dissident_containment, state_security_apparatus).
narrative_ontology:constraint_victim(political_dissident_containment, political_dissidents).
narrative_ontology:constraint_victim(political_dissident_containment, civil_society_organizations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POLITICAL DISSIDENT (SNARE) — No meaningful exit options. The dissident is trapped within the state's jurisdiction with no protection from legal retaliation. Once charged under vague extremism statutes, the pathway is predetermined: conviction, imprisonment in remote facilities, deteriorating conditions. The constraint operates through legal machinery but functions as pure incapacitation. Suppression is maximal: no independent judiciary, no meaningful appeals, no international intervention capacity. The dissident experiences the full extractive force of the state apparatus with zero alternatives.
constraint_indexing:constraint_classification(political_dissident_containment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RULING PARTY REGIME (ROPE) — Benefits from the constraint as a coordination mechanism: the carceral system neutralizes opposition while maintaining a facade of legal process. The regime experiences the constraint as solving a coordination problem: how to eliminate political threats without triggering international sanctions that are severe enough to risk regime collapse. The legal apparatus provides deniability (prosecutions appear to follow legal procedures), and the remote prison system isolates consequences from public view. No extraction cost — pure benefit flows to the regime through political monopoly maintenance.
constraint_indexing:constraint_classification(political_dissident_containment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: OPPOSITION PARTIES & CIVIL SOCIETY (SNARE) — Severely constrained by fear of prosecution and organizational suppression. Exit options are limited: operate underground (high risk), exit the country (loss of base), or accept neutered status. The constraint functions as a preemptive extraction mechanism: resources, leadership, and organizational capacity are diverted toward legal defense and regime avoidance rather than political activity. Suppression through legal jeopardy (defamation suits, extremism charges) prevents any organized challenge. The mechanism is intentionally brutal — the visibility of dissident imprisonment serves as a deterrent to broader movement participation.
constraint_indexing:constraint_classification(political_dissident_containment, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL HUMAN RIGHTS COMMUNITY (TANGLED ROPE) — Organized actors (UN bodies, NGOs, Western governments) have genuine mobility and exit options (diplomatic pressure, sanctions, refugee asylum). They benefit from coordination (treaty frameworks, documented cases create norm-setting precedent) but experience asymmetric extraction: their advocacy is instrumentalized by Western governments for geopolitical positioning, their documentation is often ineffectual against determined authoritarian regimes, and their intervention sometimes endangers dissidents further. Suppression is moderate at this level (regimes cannot directly imprison international observers, but can restrict access and retaliate against domestic partners). The mixed nature reflects both coordination gains (establishing international norms) and extraction costs (limited actual leverage).
constraint_indexing:constraint_classification(political_dissident_containment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: STATE JUDICIAL SYSTEM (PITON) — The court system operates largely as theater: judges are subordinate to the security apparatus, verdicts are predetermined, legal procedures provide the appearance of legitimacy without substance. The theater ratio is high (0.65) because formal legal machinery — trials, appeals, legal representation — persists despite being non-functional as genuine dispute resolution. The judiciary maintains the performative framework (proper courtroom procedures, judges in robes, defense lawyers present) while delivering pre-determined outcomes. The system persists through institutional inertia: removing the judicial theater entirely would trigger international condemnation and destabilize regime legitimacy, so the regime maintains the shell of legal process.
constraint_indexing:constraint_classification(political_dissident_containment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, one might argue that state monopoly on violence is an inherent feature of sovereignty itself — that any state must suppress internal threats to maintain territorial integrity. This perspective risks naturalizing the constraint as an immutable law of political order. However, the structural data contradicts the mountain classification: the extractiveness (0.78), suppression (0.88), and theater ratio (0.65) reveal a contingent institutional arrangement, not a law of nature. The false summit exposes how authoritarian regimes use natural-law framing ('the state must maintain order') to legitimize what is actually a choice to eliminate opposition through carceral extraction rather than manage it through democratic competition.
constraint_indexing:constraint_classification(political_dissident_containment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_dissident_containment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_dissident_containment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_dissident_containment, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_dissident_containment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_dissident_containment, TR),
    TR >= 0.70.

:- end_tests(political_dissident_containment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High and increasing. The regime extracts political monopoly through dissident elimination. The constraint has intensified over the measured interval (0.55 → 0.78) as the regime faces increased opposition mobilization and responded with broader extremism prosecutions and harsher carceral conditions. The trajectory shows the constraint functioning as designed: each wave of opposition sparks broader criminalization and harsher imprisonment. Suppression (0.88): Extreme. No independent judiciary, no international intervention capacity, no meaningful appeals, vague criminal statutes that apply to any political organizing, remote prison placement that prevents solidarity or rescue, and documented medical neglect creating lethal conditions. Victims cannot organize alternatives because the constraint itself prevents organization. Theater ratio (0.65): Moderate-high and increasing (0.45 → 0.65). The regime maintains performative legal machinery (trials, judges, defense lawyers, appeal processes) despite all outcomes being predetermined by the security apparatus. The theater has increased as international attention has grown — the regime has responded by ensuring the appearance of legal process becomes more elaborate, not by abandoning it. This indicates the theater serves regime legitimacy, not genuine justice.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The dissident sees pure incapacitation with no exit (Snare). The regime sees a coordination solution to the problem of maintaining political monopoly (Rope). The international community sees a mixed constraint where coordination around human rights norms conflicts with limited leverage (Tangled Rope). The judiciary sees its degraded ritual role (Piton). The analytical observer risks seeing an inherent feature of sovereignty (Mountain), but the structural data reveals this as naturalization of a choice. The gap between the beneficiary (regime monopoly) and the victim (dissident incapacitation) is absolute — no shared interest or mutual benefit exists. This gap is the diagnostic signature of a Snare: the constraint persists because suppression is sufficient to prevent organized victim resistance, not because both parties benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality is determined by their structural position relative to the extraction flow. Dissidents have maximum d (0.95-1.0): victims with trapped exit options → high f(d) → high experienced extraction. The regime has minimum d (0.05): beneficiary with arbitrage exit (can maintain monopoly indefinitely) → low/negative f(d) → extraction flows toward them. Civil society has moderate d (0.55-0.65): victims with constrained exit options (cannot leave but can organize defensively) → moderate f(d) → moderate extraction. International observers have moderate d (0.70-0.75): analytical position with mobile options but limited leverage → high f(d) but not maximal. The judicial system has minimum d (0.05): subordinate beneficiary with arbitrage (maintains position by delivering verdicts) → low f(d). The directionality gradient tracks the extraction asymmetry: power and exit options determine who bears the cost and who reaps the benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_natural_law.
 *   RESOLVED BY STRUCTURAL CLARITY: The constraint is unambiguously a Snare, not a false coordination mechanism masquerading as extraction. Three tests confirm: (1) Beneficiary clarity: The regime experiences genuine benefit (political monopoly maintenance) with no corresponding cost. There is no coordination problem being solved for both parties. (2) Victim clarity: Dissidents experience pure incapacitation with no reciprocal benefit. The constraint offers them nothing in exchange for their imprisonment. (3) Suppression sufficiency: The regime can maintain the constraint indefinitely through suppression alone, without relying on victim buy-in or coordination incentives. The Snare classification is resistant to reframing. The mandate risk — 'maybe this is actually coordination around security?' — fails because the regime receives security through dissident elimination, not through cooperation with dissidents. The constraint is not an agreement (implicit or explicit) but an imposition. Mandatrophy is resolved: this is a Snare, not a Tangled Rope or Scaffold. The regime has chosen extraction over any genuine coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dissident_threat_threshold,
    'At what threshold of political threat does a regime transition from criminal prosecution to extrajudicial carceral elimination?',
    'Comparative analysis across regimes: identify correlation between regime fragility, opposition mobilization capacity, and severity of dissident imprisonment. Historical case studies of transitions from political detention to systematic elimination.',
    'If threshold is regime-fragility dependent: the constraint is a rational (though brutal) regime survival mechanism. If threshold is arbitrary: the constraint reflects ideological elimination rather than threat response, suggesting higher extractiveness (> 0.85) classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissident_threat_threshold, empirical, 'Threshold determining transition from prosecution to elimination').

omega_variable(
    international_sanction_elasticity,
    'Do international sanctions (economic, diplomatic, arms embargoes) actually increase or decrease the severity of dissident treatment within the regime?',
    'Time-series analysis of sanction severity vs prison conditions, dissident release rates, and mortality; comparative case studies of isolated vs partially-integrated regimes.',
    'If sanctions increase severity: they strengthen the regime''s narrative (external enemies), increasing suppression and extraction. If sanctions decrease severity: international pressure creates genuine constraints on the regime''s extraction capacity. This affects whether the constraint''s suppression level is endogenous or externally moderated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_sanction_elasticity, empirical, 'Impact of international sanctions on dissident treatment severity').

omega_variable(
    regime_legitimacy_feedback,
    'Does visible dissident persecution strengthen or weaken regime legitimacy among the general population and international community?',
    'Survey data on public opinion in regimes with visible dissident persecution; international polling on perception of regime legitimacy; correlation between dissident imprisonment visibility and protest frequency.',
    'If strengthens legitimacy (domestic fear-compliance): the constraint is self-reinforcing (Snare confirmed). If weakens legitimacy (domestic sympathy, international isolation): the constraint is unstable and depends on suppression intensity to persist, suggesting this is a doomed strategy rather than a stable equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_legitimacy_feedback, empirical, 'Effect of dissident persecution visibility on regime legitimacy').

omega_variable(
    biological_constraint_lethality,
    'Are deaths in Arctic penal colonies the result of deliberate state policy (homicide via carceral design) or foreseeable but unintended consequences of harsh conditions?',
    'Forensic analysis of death causes; investigation of medical care availability and deliberate denial; comparative analysis of mortality rates across Russian penal facilities (Arctic vs temperate); documentation of specific cases (e.g., Navalny).',
    'If deliberate policy: extractiveness should be rated > 0.85 (genocide-adjacent), mandatrophy becomes existential (is this even classifiable as a constraint or is it naked state violence?). If foreseeable consequences: extractiveness remains 0.78 (incapacitation + elimination through carceral conditions). This is the highest-stakes omega for this constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biological_constraint_lethality, empirical, 'Whether carceral deaths reflect deliberate policy or foreseeable consequences').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_dissident_containment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pdissident_tr_t0, political_dissident_containment, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pdissident_tr_t5, political_dissident_containment, theater_ratio, 5, 0.58).
narrative_ontology:measurement(pdissident_tr_t10, political_dissident_containment, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(pdissident_be_t0, political_dissident_containment, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(pdissident_be_t5, political_dissident_containment, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(pdissident_be_t10, political_dissident_containment, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_dissident_containment, enforcement_mechanism).
narrative_ontology:affects_constraint(political_dissident_containment, authoritarian_surveillance_state).
narrative_ontology:affects_constraint(political_dissident_containment, suppression_of_civil_society_assembly).

% DUAL FORMULATION NOTE:
% The political dissident containment constraint is structurally upstream of broader authoritarianism mechanisms. The carceral elimination of opposition leaders enables the regime to deploy surveillance and assembly restrictions against the general population without facing organized resistance. This story represents the apex extraction mechanism; the related constraints represent secondary enforcement layers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(political_dissident_containment, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
