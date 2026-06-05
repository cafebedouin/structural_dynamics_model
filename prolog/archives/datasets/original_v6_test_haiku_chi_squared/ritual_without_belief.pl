% ============================================================================
% CONSTRAINT STORY: ritual_without_belief
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ritual_without_belief, []).

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
 *   constraint_id: ritual_without_belief
 *   human_readable: The Hollow Orthopraxy
 *   domain: social/organizational/religious
 *
 * SUMMARY:
 *   The hollow orthopraxy represents a structural constraint where the
 *   external performance of a ritual or protocol is strictly enforced despite
 *   the vanishing or inversion of its underlying belief or functional
 *   utility. This occurs across religious traditions, organizational
 *   protocols, academic ceremonies, and cultural practices when institutional
 *   gatekeepers retain enforcement power even as the epistemological or
 *   functional foundation has eroded. The constraint exemplifies how
 *   suppression mechanisms can persist in the absence of coordination
 *   benefits: practitioners are compelled to perform not because the ritual
 *   produces collective goods, but because refusal triggers social,
 *   vocational, or identity costs. The increasing theater ratio over the
 *   measurement interval reflects the escalating performative content as
 *   institutional actors devote more energy to defending or reinterpreting
 *   the ritual than to its original function. The extractiveness rise (0.42 →
 *   0.68) indicates increasing asymmetry: gatekeepers extract compliance
 *   without providing coordination value.
 *
 * KEY AGENTS:
 *   - Practicing Members: Primary victims (powerless/trapped) — forced compliance with no functional benefit; exit costs exceed staying costs
 *   - Questioners/Believers in Transition: Secondary victims (moderate/constrained) — recognize hollowness but face social penalty for voicing doubt
 *   - Institutional Gatekeepers: Primary beneficiaries (institutional/arbitrage) — derive authority and organizational coherence from ritual enforcement; full exit option available
 *   - Religious Tradition Apparatus: Institutional actor (institutional/constrained) — maintains ritual through inertia despite internal theological incoherence (Piton perspective)
 *   - Reformist Coalition: Organized actors (organized/constrained) — recognize the constraint but lack power to change it; theater dominates activity (Piton with Scaffold potential)
 *   - Sociological Observer: Analytical perspective (analytical/analytical) — sees the constraint as pure extraction without coordination benefit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ritual_without_belief, 0.68).
domain_priors:suppression_score(ritual_without_belief, 0.72).
domain_priors:theater_ratio(ritual_without_belief, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ritual_without_belief, extractiveness, 0.68).
narrative_ontology:constraint_metric(ritual_without_belief, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ritual_without_belief, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ritual_without_belief, snare).
narrative_ontology:human_readable(ritual_without_belief, "The Hollow Orthopraxy").
narrative_ontology:topic_domain(ritual_without_belief, "social/organizational/religious").

domain_priors:requires_active_enforcement(ritual_without_belief).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ritual_without_belief, institutional_gatekeepers).
narrative_ontology:constraint_victim(ritual_without_belief, ritual_practitioners).
narrative_ontology:constraint_victim(ritual_without_belief, believer_pool).
narrative_ontology:constraint_victim(ritual_without_belief, organizational_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICING MEMBER (SNARE) — Compelled to perform the ritual under institutional penalty (social ostracism, vocational exclusion, family rupture). Cannot exit without bearing massive costs. Performs the ritual despite knowing its functional utility has vanished. Extracted time, attention, and credibility for zero coordination benefit. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.76.
constraint_indexing:constraint_classification(ritual_without_belief, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: QUESTIONER / BELIEVER IN TRANSITION (SNARE) — Begins to recognize that the ritual's stated function no longer obtains but cannot publicly voice doubt without social penalty. Forced choice: continue performing inauthentically (cognitive dissonance) or exit (relationship loss, identity rupture). Suppression prevents honest inquiry. d≈0.85, f(d)≈1.15, σ=0.9 → χ≈0.70.
constraint_indexing:constraint_classification(ritual_without_belief, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL GATEKEEPER (ROPE) — Enforces ritual compliance; derives authority, funding, and organizational coherence from the ritual's requirement. Experiences the constraint as coordination: ritual performance maintains institutional boundaries and member commitment. Possesses full exit option (arbitrage) — can shift emphasis or reinterpret ritual meaning without losing position. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.06. Negative extraction = net beneficiary.
constraint_indexing:constraint_classification(ritual_without_belief, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: RELIGIOUS TRADITION APPARATUS (PITON) — The broader institutional machinery (denominational authority, seminary training, liturgical commissions) sees the ritual as a central load-bearing element but recognizes (internally) that the theological justification has become incoherent. Maintains the ritual through inertia, habit, and fear of schism. theater_ratio=0.85 indicates performative preservation: the apparatus spends energy on defense and reinterpretation rather than function. d≈0.25, f(d)≈0.18, σ=1.1 → χ≈0.16.
constraint_indexing:constraint_classification(ritual_without_belief, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: REFORMIST COALITION (PITON WITH SUNSET POTENTIAL) — Organized agents within and adjacent to the tradition (theologians, younger clergy, interfaith scholars) recognize the constraint but possess limited power to change it. Theater dominates: much activity is spent on 'faithful reinterpretation' rather than actual reform. However, this perspective contains a Scaffold dimension: as belief defection accelerates and alternative community structures emerge (secular voluntarism, non-denominational spirituality), the ritual's enforcement capacity degrades naturally. d≈0.45, f(d)≈0.47, σ=1.1 → χ≈0.35.
constraint_indexing:constraint_classification(ritual_without_belief, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SOCIOLOGICAL OBSERVER (SNARE) — From a civilizational view, the hollow orthopraxy is pure extraction: the institutional apparatus extracts behavioral compliance and identity performance from believers despite the absence of coordination benefit. The ritual persists because suppression mechanisms (socialization, authority, shame) prevent collective exit. This is not an immutable law but a contingent equilibrium maintained by power asymmetry. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(ritual_without_belief, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ritual_without_belief_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ritual_without_belief, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ritual_without_belief, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ritual_without_belief, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ritual_without_belief, TR),
    TR >= 0.70.

:- end_tests(ritual_without_belief_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The ritual extracts time, cognitive effort, authenticity, and identity performance from practitioners while providing no measurable coordination benefit. The extraction is asymmetric: practitioners lose; gatekeepers and institutional apparatus gain. The value is higher than in pure coordination (0.05) because there is no genuine collective action problem being solved — the ritual persists for authority maintenance, not coordination. Suppression (0.72): High. Multiple barriers prevent honest critique: (1) theological framing makes doubt feel like moral failure, (2) social ostracism follows public disbelief, (3) vocational penalties apply to clergy who voice skepticism, (4) identity rupture follows exit (family, community, self-concept all tied to the ritual). Theater ratio (0.85): Very high. Institutional energy has shifted from performing the ritual's stated function to defending it against internal and external critique. Much activity consists of reinterpretation, theological gymnastics, and loyalty enforcement rather than the ritual's original purpose. The interval trajectory reflects degradation: early in the constraint's lifecycle, some participants believed; theater was lower (0.55). Over 40 years, belief collapsed among practitioners and leaders, but suppression mechanisms remained intact. The apparatus doubled down, increasing performative defense (theater → 0.85) and asymmetric extraction (ε → 0.68).
 *
 * PERSPECTIVAL GAP:
 *   The practicing member sees a Snare: they are trapped, extracting nothing. The questioner sees a Snare with increasing cognitive dissonance: they recognize the hollowness but cannot exit without massive cost. The institutional gatekeeper sees a Rope: they are solving a legitimate problem (maintaining organizational identity and boundary), and they possess full exit options (they could reframe the ritual or abandon it without losing position). The religious tradition apparatus sees a Piton: incoherent defense, maintenance through inertia, theater dominating function. The reformist coalition sees a Piton with Scaffold potential: they recognize the constraint and are building alternative pathways (secular community, theological reinterpretation, ecumenical bridges), but reform is slow due to vested interests. The sociological observer sees a Snare: this is not coordination but pure extraction of compliance from believers who have lost faith in the coordination function itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Practicing members: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction burden. Questioners: Victim + constrained → d≈0.85, f(d)≈1.15. Significant extraction; some constrained options (quiet defection, emigration) exist but are costly. Institutional gatekeepers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; they have full exit option (they can change institutional policy) but choose not to because the ritual benefits them. Religious tradition apparatus: Victim of institutional incoherence + constrained → d≈0.25, f(d)≈0.18. The apparatus is trapped by its own precedent and consistency requirement, unable to acknowledge the hollowness without fracture. Reformist coalition: Organized but constrained → d≈0.45, f(d)≈0.47. They can organize and voice critique but lack structural power to change enforcement. Sociological observer: Analytical → d≈0.88, f(d)≈1.32. The observer sees the constraint as extractive; this is not a false summit (Mountain), but a real Snare hidden behind theological language.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by identifying the extraction as real and distinct from coordination. The institutional gatekeeper's Rope perspective (they experience coordination) is legitimate for THEIR structural position. The practicing member's Snare perspective (they experience pure extraction) is equally legitimate for THEIR structural position. The constraint is NOT both Rope and Snare simultaneously; it is a Snare that benefits from a false Rope framing used by gatekeepers to justify enforcement. The sociological observer and the practicing member agree: this is extraction, not coordination. The institutional gatekeeper's Rope classification reflects their beneficiary position, not the constraint's true type. The mandatrophy is resolved by recognizing that 'coordination' from the gatekeeper's view is 'extraction' from the practitioner's view — they are not describing the same phenomenon. The constraint's true type is Snare because the primary functional outcome is not collective action but authority maintenance and compliance extraction. Ritual performance provides gatekeepers with (1) organizational coherence, (2) membership boundary maintenance, (3) a basis for authority claims, and (4) a mechanism for identity enforcement. None of these constitute coordination goods for practitioners; they are distribution goods (benefits to some, costs to others) with suppression preventing exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_residue_threshold,
    'What minimal level of functional utility must a ritual retain before suppression becomes pure extraction rather than legitimate coordination enforcement?',
    'Empirical analysis of ritual outcomes: community cohesion metrics, believer psychological health, ethical behavior, collective action capacity. Does the ritual produce measurable coordination goods?',
    'If functional utility > 0.2: constraint may be Tangled Rope (mixed coordination/extraction). If utility ≈ 0: constraint is pure Snare (extraction without coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_residue_threshold, empirical, 'Threshold for residual functional utility in hollow ritual').

omega_variable(
    belief_collapse_rate,
    'Does explicit disbelief among practitioners represent a majority tipping point, and if so, what catalyzes the transition from enforcement to reformation or collapse?',
    'Longitudinal belief surveys; analysis of defection cascades in comparative religious history; identification of threshold conditions for institutional schism or reformation.',
    'If tipping < 30% disbelief: institutional gatekeepers can sustain enforcement indefinitely. If tipping > 40%: suppression mechanisms fail, revealing the constraint''s extraction core.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(belief_collapse_rate, empirical, 'Critical mass of disbelief for enforcement collapse').

omega_variable(
    alternative_identity_sufficiency,
    'Do secular or post-denominational communities provide equivalent social belonging, meaning, and identity anchoring without ritual enforcement?',
    'Comparative analysis of member outcomes in traditional vs non-traditional communities; measurement of subjective well-being, social capital, ethical behavior, identity stability.',
    'If alternatives sufficient: Scaffold perspective confirmed, enforced ritual exits gradually. If alternatives insufficient: suppression mechanisms remain effective because no exit truly exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_identity_sufficiency, empirical, 'Whether alternative communities provide comparable identity and belonging').

omega_variable(
    leadership_complicity_range,
    'What proportion of institutional leaders consciously recognize the ritual''s hollowness, and does this knowledge change their enforcement incentive?',
    'Qualitative interviews with clergy, denominational officials, seminary faculty; analysis of internal theological critiques and administrative correspondence.',
    'High complicity (>60%): gatekeepers are knowing extractors (pure Snare). Low complicity (<20%): gatekeepers sincerely believe in the ritual (Tangled Rope). Mixed: Snare with variable enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leadership_complicity_range, empirical, 'Degree of conscious leadership recognition of ritual hollowness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ritual_without_belief, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hollow_tr_t0, ritual_without_belief, theater_ratio, 0, 0.55).
narrative_ontology:measurement(hollow_tr_t20, ritual_without_belief, theater_ratio, 20, 0.72).
narrative_ontology:measurement(hollow_tr_t40, ritual_without_belief, theater_ratio, 40, 0.85).

% Extraction over time
narrative_ontology:measurement(hollow_be_t0, ritual_without_belief, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hollow_be_t20, ritual_without_belief, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hollow_be_t40, ritual_without_belief, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ritual_without_belief, enforcement_mechanism).
narrative_ontology:affects_constraint(ritual_without_belief, belief_legitimacy_gap).
narrative_ontology:affects_constraint(ritual_without_belief, vocational_gatekeeping).

% DUAL FORMULATION NOTE:
% The hollow orthopraxy is a downstream constraint whose existence depends on two upstream conditions: (1) belief_legitimacy_gap — the epistemological erosion that makes the ritual's foundation incoherent, and (2) vocational_gatekeeping — the institutional mechanisms that allow gatekeepers to suppress dissent and maintain enforcement. Each upstream constraint has its own ε and classification; this constraint's ε=0.68 reflects the synergistic effect of both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ritual_without_belief, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
