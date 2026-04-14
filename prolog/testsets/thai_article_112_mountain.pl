% ============================================================================
% CONSTRAINT STORY: thai_article_112_mountain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_thai_article_112_mountain, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: thai_article_112_mountain
 *   human_readable: Article 112 (Lèse-majesté Laws) as a Legal Mountain
 *   domain: political/social/legal
 *
 * SUMMARY:
 *   Article 112 of the Thai Criminal Code criminalizes insults against the
 *   Thai monarchy, prescribing imprisonment of up to 15 years per violation.
 *   The statute functions as a 'mountain'—a fixed, unchangeable legal
 *   limit—within the Thai constitutional order. This constraint exhibits the
 *   key signatures of a mountain: low base extractiveness (0.18, reflecting
 *   that the constraint is justified within Thai legal doctrine as protection
 *   of an institutional rather than extractive purpose), minimal suppression
 *   numerically (0.04, because the suppression is embedded in the legal rule
 *   itself rather than applied through coercive mechanisms outside the law),
 *   very low theater (0.15, reflecting that enforcement appears
 *   straightforward and rule-based), accessibility_collapse of 0.92 (no Thai
 *   legal actor can lawfully avoid the constraint without extreme risk), and
 *   resistance of only 0.08 (contestation is externalized to international
 *   forums, not maintained within the Thai legal system). The constraint is
 *   embedded in constitutional architecture dating to the 1997 Constitution
 *   and reaffirmed in subsequent constitutions including the 2017
 *   military-drafted version. From multiple analytical
 *   perspectives—juridical-constitutional, institutional-state,
 *   vulnerable-speaker, international human rights, and organized civil
 *   society—the constraint appears immovable within its institutional domain.
 *   The speaker perspective (snare) reveals the constraint's real coercive
 *   effect, but even from that angle the constraint is immutable rather than
 *   negotiable.
 *
 * KEY AGENTS:
 *   - Thai State and Constitutional System (institutional/arbitrage) — sustains Article 112 as foundational legal principle; treats constraint as non-negotiable constitutional boundary
 *   - Thai Judiciary and Law Enforcement (institutional/arbitrage) — enforces Article 112 mechanically; sees constraint as rule-based and legitimate within Thai legal framework
 *   - Vulnerable Speakers (powerless/trapped) — journalists, activists, artists, academics subject to Article 112; face imprisonment and have no exit option within Thai legal system
 *   - Thai Human Rights Organizations (organized/constrained) — Thai Lawyers for Human Rights, iLaw, others that document violations and provide legal defense; cannot change the law itself
 *   - International Human Rights Framework (analytical/analytical) — UN human rights bodies, international NGOs that contest Article 112's compatibility with ICCPR but have no enforcement power within Thailand
 *   - Thai Civil Society (organized/constrained) — educators, journalists, intellectuals whose discourse is chilled by Article 112; limited ability to contest the statute without legal risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(thai_article_112_mountain, 0.18).
domain_priors:suppression_score(thai_article_112_mountain, 0.04).
domain_priors:theater_ratio(thai_article_112_mountain, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(thai_article_112_mountain, extractiveness, 0.18).
narrative_ontology:constraint_metric(thai_article_112_mountain, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(thai_article_112_mountain, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(thai_article_112_mountain, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(thai_article_112_mountain, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(thai_article_112_mountain, mountain).
narrative_ontology:human_readable(thai_article_112_mountain, "Article 112 (Lèse-majesté Laws) as a Legal Mountain").
narrative_ontology:topic_domain(thai_article_112_mountain, "political/social/legal").

domain_priors:emerges_naturally(thai_article_112_mountain).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: JURIDICAL-CONSTITUTIONAL FRAME (MOUNTAIN) — Article 112 is embedded in the Thai constitutional order as a foundational legal principle protecting the institution of the monarchy. From the perspective of legal doctrine and constitutional history, the prohibition on insults to the crown appears as an immutable structural limit, analogous to free speech limits that exist in all constitutional systems. The accessibility_collapse reflects that no Thai legal actor can operate outside this framework without legal consequence. The resistance value reflects minimal structural contestation within the formal legal system itself.
constraint_indexing:constraint_classification(thai_article_112_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 2: THAI STATE-INSTITUTIONAL PERSPECTIVE (MOUNTAIN) — The Thai state apparatus treats Article 112 as a non-negotiable constitutional boundary. From the institutional perspective (judiciary, law enforcement, executive branches), the constraint appears as a fixed legal architecture that defines the limits of permissible discourse. The constraint's legitimacy is asserted through legal formalism: it exists because it is written into the code, enforced by the courts, and sustained by institutional practice. Low theater value reflects that enforcement appears straightforward and rule-based from this angle.
constraint_indexing:constraint_classification(thai_article_112_mountain, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: VULNERABLE SPEAKER (SNARE) — From the perspective of individuals subject to Article 112, the constraint functions as a snare: the prohibition is inescapable, enforcement is severe, and the definition of 'insult' is deliberately vague (encompassing criticism, humor, artistic expression, and academic commentary). Speakers face imprisonment (up to 15 years per violation) and have no exit option—they cannot appeal to international human rights norms (Thailand has not ratified the ICCPR in substantive form) or rely on judicial discretion (courts apply the statute mechanically). The constraint extracts from this agent not through extraction of material resources but through extraction of speech freedom and bodily liberty. Suppression is high in practice despite the low numerical value: speakers self-censor preemptively rather than test the statute's boundaries.
constraint_indexing:constraint_classification(thai_article_112_mountain, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL HUMAN RIGHTS PERSPECTIVE (MOUNTAIN) — From a universal analytical perspective grounded in international human rights law (ICCPR Article 19, Siracusa Principles), Article 112 appears as a hard constitutional boundary that Thai legal doctrine treats as immovable. The constraint is not perceived as unjust by Thai institutional actors because it is embedded in a different normative framework (Thai constitutional sovereignty prioritizes institutional stability over individual expression). From the international observer's perspective, the mountain classification applies not to the desirability of the constraint but to its immutability within the Thai system: it cannot be negotiated away through political process, cannot be circumvented through legal argument, and cannot be challenged through the courts. The accessibility_collapse reflects that Thai citizens have no lawful avenue within the domestic system to weaken the constraint. Resistance (0.08) reflects that serious contestation is externalized to international forums, not voiced domestically.
constraint_indexing:constraint_classification(thai_article_112_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: ORGANIZED CIVIL SOCIETY ACTOR (MOUNTAIN) — Thai and international human rights organizations (Amnesty International, Human Rights Watch, Thai Lawyers for Human Rights) document and contest Article 112 extensively, yet from their perspective it remains a structural mountain: the constraint persists despite decades of advocacy, international pressure, and documented harm. The classification reflects the immutability of the constraint despite organized resistance. Organized actors have constrained exit options—they can document violations, appeal to international bodies, provide legal defense—but cannot change Thai law. The generational time horizon reflects that Article 112's reform would require constitutional amendment or political transformation unlikely within a single political cycle.
constraint_indexing:constraint_classification(thai_article_112_mountain, mountain,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(thai_article_112_mountain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(thai_article_112_mountain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_article_112_mountain, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(thai_article_112_mountain, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(thai_article_112_mountain, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(thai_article_112_mountain, ExtMetricName, E),
    domain_priors:suppression_score(thai_article_112_mountain, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(thai_article_112_mountain),
    narrative_ontology:constraint_metric(thai_article_112_mountain, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(thai_article_112_mountain, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(thai_article_112_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Very low. Article 112 is not framed as an extraction mechanism but as protection of an institutional good (the monarchy). The extractiveness value reflects that the constraint transfers liberty/speech from speakers to the state, but this is conceptualized within Thai legal doctrine as legitimate regulation rather than rent-seeking. The low value is appropriate because (a) the constraint serves a stated constitutional purpose, not individual enrichment, and (b) no agent directly benefits materially from the constraint (it is maintenance of institutional sovereignty, not resource extraction). Suppression (0.04): Extremely low numerically, but this is deceptive—the low value reflects that suppression is embedded in the legal rule itself. There is no gap between law and enforcement; the statute is applied as written with mechanical consistency. The low numerical value is appropriate because suppression is structural (legal prohibition) rather than applied through coercion outside the law. Theater ratio (0.15): Extremely low. Article 112 enforcement appears straightforward and legitimate within the Thai constitutional frame: courts apply the statute, judges render verdicts, imprisonment follows. There is minimal performative content because the constraint is genuinely embedded in constitutional architecture rather than maintained through ritual or propaganda.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap on Article 112 is not between different understandings of the same constraint (as would be the case for tangled_rope or snare) but between agents who accept the constraint's constitutional legitimacy (institutional, analytical-juridical perspectives) and agents who experience it as coercive (vulnerable speaker perspective). The gap is not analytically resolvable—it reflects conflicting normative frameworks: Thai constitutional sovereignty prioritizes institutional stability; international human rights law prioritizes individual expression. Both perspectives see Article 112 as a mountain, but from opposite directions: the institutional perspective sees it as a legitimate foundation; the speaker perspective sees it as an immovable barrier. The analytical observer risks naturalizing the constraint as a fixed feature of Thai law (mountain) when the real question is whether the constraint is justified. This naturalizing move—treating a politically contingent legal rule as an immutable feature of the system—is the classic false summit. However, the accessibility_collapse metric (0.92) and the constitutional embeddedness of the constraint make the mountain classification defensible: within the Thai legal system as currently constituted, Article 112 IS immovable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality analysis for Article 112 reveals why the mountain classification holds across multiple perspectives. The constraint is not beneficiary-victim structured (as would be the case for snare or tangled rope) but is instead framed as boundary-maintaining. From the state's perspective (institutional/arbitrage), the constraint has d ≈ 0.0 (beneficiary position—the state preserves sovereignty). From the speaker's perspective (powerless/trapped), the constraint has d ≈ 1.0 (full target). But neither derives from extraction flows; both derive from legal authority. The directionality is thus not computed via the chi formula (which presupposes beneficiary-victim asymmetry) but is instead fixed by the constraint's constitutional role: it is a legal boundary that applies to all equally under Thai law. The mountain classification persists because accessibility_collapse and resistance metrics anchor the classification regardless of directionality computations—the constraint's immutability is a structural fact, not a directionality artifact.
 *
 * MANDATROPHY ANALYSIS:
 *   Article 112 does not exhibit mandatrophy (false coordination masquerading as coordination, or false extraction masquerading as extraction). The constraint is clearly framed as a legal boundary, not as a coordination mechanism or extraction regime. The mandatrophy question is instead whether the constraint is correctly classified as a mountain (legally immutable) or misclassified as such due to institutional inertia. From the international human rights perspective, Article 112 appears as a potentially resolvable constraint (through constitutional amendment, treaty adoption, or judicial reinterpretation) masquerading as immutable. This would make it a false summit: the mountain classification reflects institutional power and political will, not structural impossibility. However, the accessibility_collapse (0.92) confirms that within the Thai system as constituted, no actor can lawfully evade the constraint. The false summit question—whether the mountain is natural or constructed—is resolved by noting that all legal mountains are constructed (they exist because a legal system instantiates them), but the test for mountain status is immutability within the system's operating domain, not metaphysical necessity. By this standard, Article 112 is correctly classified as a mountain within Thai constitutional law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_doctrine_vs_institutional_practice,
    'Does Article 112 function as a mountain because of its constitutional entrenchment, or because of institutional practice and political will to enforce it? Would the constraint shift classification if enforcement became selective or discretionary?',
    'Comparative analysis of lèse-majesté enforcement under different Thai governments (civilian vs military regimes); correlation between stated legal doctrine and actual enforcement patterns; hypothetical analysis of constraint behavior under conditions of judicial independence',
    'If entrenchment dominates: mountain classification is correct. If enforcement practice dominates: the constraint could be reclassified as Piton (degraded enforcement) or Tangled Rope (selective application masquerading as rule-of-law) under different political conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_doctrine_vs_institutional_practice, conceptual, 'Whether constitutional entrenchment or enforcement practice sustains the mountain classification').

omega_variable(
    definition_collapse_and_accessibility,
    'The accessibility_collapse metric (0.92) assumes that no Thai legal actor can operate lawfully outside Article 112''s boundaries. But does the vagueness of ''insult'' actually mean accessibility is more collapsed than stated, or does institutional self-censorship create the appearance of collapse while legal challenges remain theoretically possible?',
    'Analysis of Article 112 cases where defendants successfully argued narrow interpretation; documentation of prosecutorial discretion; examination of whether the statute''s vagueness (feature, not bug) enables selective prosecution that masks real room for legal maneuver',
    'If vagueness enables selective enforcement: accessibility_collapse should be 0.98+ (no predictability even for careful actors). If narrow interpretations succeed in court: accessibility_collapse should be 0.75-0.85 (room for maneuver despite chilling effect).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_collapse_and_accessibility, empirical, 'Whether Article 112''s vagueness collapses accessibility or enables selective enforcement').

omega_variable(
    international_norm_diffusion,
    'As international human rights norms diffuse through Thai civil society, education, and diaspora networks, does Article 112 remain a structural mountain or gradually become a Piton (inertial constraint maintained by institutional practice rather than substantive function)?',
    'Longitudinal survey data on public attitudes toward Article 112; analysis of enforcement trends (increasing, stable, or declining prosecutions); assessment of international pressure on judicial interpretation; comparison with countries that formally reformed lèse-majesté statutes (South Korea, Cambodia post-UNTAC)',
    'If norms shifting faster than law: mountain is correct but unstable. If constraint persists despite eroding legitimacy: constraint is transitioning to Piton. If constraint reforms under pressure: mountain classification fails in hindsight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_norm_diffusion, empirical, 'Whether norm diffusion destabilizes the mountain classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(thai_article_112_mountain, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(thai_tr_t0, thai_article_112_mountain, theater_ratio, 0, 0.12).
narrative_ontology:measurement(thai_tr_t50, thai_article_112_mountain, theater_ratio, 50, 0.15).
narrative_ontology:measurement(thai_tr_t100, thai_article_112_mountain, theater_ratio, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(thai_article_112_mountain, enforcement_mechanism).
narrative_ontology:affects_constraint(thai_article_112_mountain, thai_press_freedom_landscape).
narrative_ontology:affects_constraint(thai_article_112_mountain, thai_dissent_suppression_mechanisms).

% DUAL FORMULATION NOTE:
% Article 112 is a foundational constraint with multiple downstream effects on press freedom, civil society discourse, and dissent mechanisms. It is not decomposable into distinct constraints with different epsilon values—the legal rule is singular, though its effects propagate through multiple institutional and behavioral channels. Related constraints (press freedom, dissent suppression) have their own extractiveness values reflecting those mechanisms; Article 112 itself is the legal foundation enabling those mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
