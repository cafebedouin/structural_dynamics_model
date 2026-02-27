% ============================================================================
% CONSTRAINT STORY: insult_wisdom_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_insult_wisdom_training, []).

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
 *   constraint_id: insult_wisdom_training
 *   human_readable: The Odd Assignment (Paying for Insults)
 *   domain: religious/social/philosophical
 *
 * SUMMARY:
 *   A spiritual Master assigns a disciple the task of paying money to anyone
 *   who insults him for three years as a prerequisite for receiving wisdom
 *   teachings. The assignment creates a structural tension between its
 *   claimed pedagogical function (training in non-reactivity and
 *   ego-dissolution) and its actual mechanisms (financial extraction,
 *   systematic humiliation, and market-inverted social dynamics). This
 *   constraint exemplifies the distinction between coordination mechanisms
 *   and extractive mechanisms, and the risk of conflating harsh pedagogy with
 *   justified institutional pressure. The same assignment appears as pure
 *   coordination (wisdom tradition), temporary scaffolding (ego-dissolution
 *   tool with sunset), institutional theater (vestigial ritual maintained by
 *   authority), mixed extraction with coordination (insult market dynamics),
 *   pure extraction disguised as training (from the disciple's perspective),
 *   and a false natural law (civilizational view that risks naturalizing
 *   contingent suffering). The theater_ratio (0.58) reflects that much of the
 *   constraint's justification is narrative — the claim that paying for
 *   insults is the unique path to wisdom — rather than empirically
 *   demonstrated. The extractiveness (0.52) captures the real financial
 *   burden on the disciple and the perverse incentive structure created for
 *   potential insulters, moderated by the bounded 3-year duration and the
 *   genuine existence of alternative wisdom traditions that achieve similar
 *   outcomes without systematic payment-for-humiliation.
 *
 * KEY AGENTS:
 *   - Disciple: Primary victim (powerless/trapped) — bound by commitment; bears full financial and psychological cost; cannot exit without losing access to promised teachings
 *   - Master/Wisdom Tradition: Primary beneficiary (institutional/arbitrage) — gains authority, student commitment, and financial compensation; benefits from the credibility of a coherent (if brutal) system
 *   - Insult Generators (Society): Secondary beneficiary (moderate/constrained) — gain payment incentive and temporary power to affect the disciple; constrained by social norms against cruelty
 *   - Reformers/Alternative Traditions: Organized agent (organized/mobile) — propose sunset and alternative methods; see the constraint as temporary and replaceable
 *   - Institutional Religious Authority: Institutional actor (institutional/arbitrage) — maintains the constraint through narrative legitimacy and textual authority; sees it as vestigial but maintains it through inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent suffering as inherent to wisdom; must distinguish pedagogical necessity from extractive rationalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(insult_wisdom_training, 0.52).
domain_priors:suppression_score(insult_wisdom_training, 0.68).
domain_priors:theater_ratio(insult_wisdom_training, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(insult_wisdom_training, extractiveness, 0.52).
narrative_ontology:constraint_metric(insult_wisdom_training, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(insult_wisdom_training, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(insult_wisdom_training, tangled_rope).
narrative_ontology:human_readable(insult_wisdom_training, "The Odd Assignment (Paying for Insults)").
narrative_ontology:topic_domain(insult_wisdom_training, "religious/social/philosophical").

domain_priors:requires_active_enforcement(insult_wisdom_training).
narrative_ontology:has_sunset_clause(insult_wisdom_training).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(insult_wisdom_training, master_wisdom_tradition).
narrative_ontology:constraint_beneficiary(insult_wisdom_training, disciple_ego_dissolution).
narrative_ontology:constraint_beneficiary(insult_wisdom_training, insult_generators).
narrative_ontology:constraint_victim(insult_wisdom_training, disciple_financial_resources).
narrative_ontology:constraint_victim(insult_wisdom_training, disciple_dignity).
narrative_ontology:constraint_victim(insult_wisdom_training, social_coordination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCIPLE (SNARE) — Bound by oath/commitment to the Master; cannot exit without violating the entire relationship and renouncing access to promised wisdom. Trapped: must pay for every insult regardless of severity, frequency, or financial burden. No alternative path to the teaching. d≈0.92, f(d)≈1.40, σ=0.8 → χ≈0.60.
constraint_indexing:constraint_classification(insult_wisdom_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MASTER/WISDOM TRADITION (ROPE) — Genuine coordination function: the assignment creates a testable selection mechanism for sincere seekers and a training method for ego-dissolution (prerequisite for wisdom in many contemplative traditions). The master benefits from the credibility of a brutal-but-coherent system. d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.05. Net beneficiary through institutional authority.
constraint_indexing:constraint_classification(insult_wisdom_training, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INSULT GENERATORS (TANGLED ROPE) — Indirectly benefit from the constraint (it incentivizes paying attention to them; they gain power to extract payment); constrained by social norms against deliberately insulting others for profit. The system creates a perverse coordination: it inverts normal insult dynamics. d≈0.50, f(d)≈0.65, σ=0.8 → χ≈0.27. Mixed coordination (gets attention, payment) and extraction (targets the disciple).
constraint_indexing:constraint_classification(insult_wisdom_training, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: REFORMERS / ALTERNATIVE TRADITIONS (SCAFFOLD) — See the insult-payment mechanism as a temporary training tool, not a permanent constraint. Organized agents advocating for alternative ego-dissolution practices (meditation retreats, service, non-violent communication) propose sunset: as the disciple internalizes non-reactivity to insult, the financial payment becomes redundant and can be phased out. Has_sunset_clause_rationale: After 3 years, the disciple should be stable enough in wisdom that the external market incentive is no longer necessary. d≈0.35, f(d)≈0.32, σ=0.9 → χ≈0.16. Low extraction because the constraint is seen as temporary and targeted.
constraint_indexing:constraint_classification(insult_wisdom_training, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: INSTITUTIONAL RELIGIOUS AUTHORITY (PITON) — Views the insult-payment assignment as a vestigial ritual from ascetic traditions. The constraint persists through institutional inertia and textual authority (religious stories, teacher legitimacy) even though modern understanding of psychology and trauma suggests safer methods. theater_ratio≈0.58 indicates the authority maintains the practice partly through narrative performance (the 'wisdom' framing) rather than demonstrated efficacy. d≈0.10, f(d)≈-0.06, σ=1.0 → χ≈-0.01. Institutional beneficiary; the piton classification comes from high theater and attenuated function.
constraint_indexing:constraint_classification(insult_wisdom_training, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risk of naturalizing the assignment as an immutable law of wisdom acquisition: 'all genuine teaching requires suffering and humiliation.' This perspective treats ego-dissolution as requiring external market pressure on the disciple, as if this were a mathematical necessity. However, the base properties (ε=0.52, suppression=0.68, theater=0.58) contradict the mountain gate (ε must be ≤0.25). The engine's false summit detector catches this: the 'natural law of wisdom' is actually a contingent institutional arrangement with measurable extraction and suppression. d≈0.70, f(d)≈1.12, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(insult_wisdom_training, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(insult_wisdom_training_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(insult_wisdom_training, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(insult_wisdom_training, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(insult_wisdom_training, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(insult_wisdom_training, TR),
    TR >= 0.70.

:- end_tests(insult_wisdom_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The constraint extracts real financial resources from the disciple and creates a market incentive for others to insult him. This is not trivial. However, extractiveness is moderated by three factors: (1) the 3-year sunset (bounded duration), (2) the genuine existence of the wisdom tradition as a coherent system (not pure rent-seeking), and (3) the disciple's apparent voluntary commitment. If the assignment were indefinite, extractiveness would rise to 0.70+. Suppression (0.68): High. The disciple cannot easily exit — doing so violates the commitment and forfeits the teachings. The suppression is not total (he could walk away), but it is substantial. Social coordination is suppressed because the assignment inverts normal norms (typically we do NOT pay people who insult us). Theater ratio (0.58): Moderate. The assignment is justified through narrative legitimacy (the Master's authority, wisdom traditions, ego-dissolution philosophy) rather than through empirical demonstration of unique efficacy. The narrative component has increased over the 3-year interval as the constraint becomes more established in the disciple's life.
 *
 * PERSPECTIVAL GAP:
 *   The disciple sees a Snare: he is trapped, bears costs, and has no real exit. The Master sees a Rope: genuine coordination mechanism that filters for sincere seekers and trains non-reactivity. The society of insulters sees a Tangled Rope: they have gained temporary power (to extract payment) but are constrained by social norms. Reformers see a Scaffold: a harsh but temporary tool being replaced by better methods. The institutional authority sees a Piton: the constraint is maintained through narrative authority even as its pedagogical function has atrophied. The analytical observer risks seeing a Mountain (natural law of wisdom requiring suffering) but the structural data contradicts this false summit. The perspectival gap between the disciple (Snare) and the Master (Rope) is the central diagnostic tension: the same assignment is experienced as extractive coercion from one position and as coordinate-able training from another. This gap signals a Tangled Rope: the assignment has both genuine coordination function (selecting sincere students, training non-reactivity) AND asymmetric extraction (the Master gains authority and payment; the disciple bears costs).
 *
 * DIRECTIONALITY LOGIC:
 *   Disciple: Victim + trapped → d≈0.92, f(d)≈1.40. Maximal extraction coefficient. The disciple cannot exit; the constraint acts upon him with full force. Master: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.09. Net institutional beneficiary. The Master has full exit options (can revoke assignment, interpret it differently, rationalize it) and captures the primary benefits. Insult generators: Mixed (both beneficiary and victim) + constrained → d≈0.50, f(d)≈0.65. Moderate extraction. They gain payment incentive but are constrained by social norms against deliberate cruelty. Reformers: Organized + mobile → d≈0.35, f(d)≈0.32. Low extraction; they have agency and see a path forward (alternative methods). Institutional authority: Institutional + arbitrage → d≈0.10, f(d)≈-0.06. Piton classification driven by theater_ratio gate, not extraction. Analytical observer: Analytical → d≈0.70, f(d)≈1.12. Mountain classification is a false summit (observer naturalizes constraint).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the extraction-vs-coordination paradox by recognizing that it is a genuine Tangled Rope: it contains both real coordination function (selecting sincere students, training ego-dissolution through market inversion of the insult dynamic) AND real asymmetric extraction (financial and psychological costs borne by disciple, benefits captured by Master and tradition). The 3-year sunset clause is critical: the constraint is justified as temporary scaffolding for ego-dissolution, not as a permanent institution. If the assignment were extended indefinitely, it would collapse into a Snare. If there were empirical evidence that alternative methods (meditation retreats, service, Socratic dialogue) achieved equivalent ego-dissolution without extraction, the constraint would become unjustified Snare. The mandatrophy is resolved by anchoring the classification in the sunset clause and the existence of genuine alternative pedagogies. The false summit (analytical observer's natural law view) is caught by the engine's contradiction detection: ε=0.52 > 0.25 (mountain threshold), suppression=0.68 > 0.05 (mountain threshold), therefore the Mountain classification fails. The constraint is NOT a natural law; it is a contingent institutional arrangement with measurable extraction and suppression. This is the correct diagnosis: the assignment is justified as temporary pedagogy (Scaffold with Tangled Rope characteristics during the 3-year period) but risks degradation to a Piton (maintained through institutional inertia) if the tradition stops believing in its pedagogical function or if alternatives are not available.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    psychological_safety_threshold,
    'Does the insult-payment assignment improve genuine wisdom/non-reactivity, or does it create trauma and defensive personality structures?',
    'Longitudinal psychological assessment of graduates: measures of emotional regulation, self-other boundary health, genuine non-reactivity vs dissociative numbing, long-term relationship capacity',
    'If improves wisdom: constraint is justified Rope/Scaffold with genuine coordination function. If creates trauma: constraint is unjustified Snare with extraction disguised as training.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(psychological_safety_threshold, empirical, 'Whether insult-payment improves genuine wisdom or creates trauma').

omega_variable(
    alternative_method_equivalence,
    'Can equivalent ego-dissolution training be achieved through methods that do NOT require financial extraction or systematic humiliation?',
    'Comparative study of wisdom graduates from insult-payment traditions vs. meditation-only, service-based, or Socratic-dialogue traditions. Measures: non-reactivity, equanimity, integrated self-understanding, relational maturity.',
    'If alternatives work: the constraint is unnecessary and constitutes pure extraction (Snare). If alternatives fail: the constraint''s harshness has functional justification (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_method_equivalence, empirical, 'Whether alternative methods achieve equivalent ego-dissolution without extraction').

omega_variable(
    master_extractive_intent,
    'Is the Master''s enforcement of the assignment driven by genuine pedagogical belief in its necessity, or by financial incentive and power-maintenance?',
    'Textual analysis of Master''s teachings; institutional patterns (does the tradition reinvest payments into other disciples'' training, or concentrate them?); historical variant traditions (do parallel lineages use different mechanisms?); accountability structures for Master violations of stated principles.',
    'If genuine belief: constraint is Tangled Rope with authentic coordination function. If extractive intent: constraint is Snare with coordination function as cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(master_extractive_intent, conceptual, 'Whether Master''s intent is pedagogical or extractive').

omega_variable(
    voluntary_commitment_validity,
    'Does the disciple''s initial agreement to the assignment constitute genuine informed consent, or is consent compromised by power asymmetry and authority-dependence?',
    'Analysis of consent conditions: was the disciple aware of full financial burden before commitment? Can the disciple truly withdraw without severe consequences? Do exit options exist beyond the stated 3-year period?',
    'If consent is valid: constraint is Scaffold (temporary agreement). If consent is compromised: constraint is Snare (apparent agreement masks coercion).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_commitment_validity, conceptual, 'Whether initial consent is genuine or compromised by power asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(insult_wisdom_training, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(insult_tr_t0, insult_wisdom_training, theater_ratio, 0, 0.42).
narrative_ontology:measurement(insult_tr_t1, insult_wisdom_training, theater_ratio, 1, 0.5).
narrative_ontology:measurement(insult_tr_t2, insult_wisdom_training, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(insult_be_t0, insult_wisdom_training, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(insult_be_t1, insult_wisdom_training, base_extractiveness, 1, 0.45).
narrative_ontology:measurement(insult_be_t2, insult_wisdom_training, base_extractiveness, 2, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(insult_wisdom_training, enforcement_mechanism).
narrative_ontology:affects_constraint(insult_wisdom_training, ego_dissolution_pedagogy).
narrative_ontology:affects_constraint(insult_wisdom_training, authority_dependence_trap).

% DUAL FORMULATION NOTE:
% The insult-wisdom-training is downstream of broader wisdom traditions (which have their own ε values reflecting empirical claims about enlightenment and non-reactivity). The constraint represents a specific enforcement mechanism for achieving those wisdom outcomes. If the upstream tradition's claims are validated empirically, the constraint's extractiveness is justified as Tangled Rope; if upstream claims fail, the constraint collapses into Snare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(insult_wisdom_training, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
