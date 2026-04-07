% ============================================================================
% CONSTRAINT STORY: institutional_consensus_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_consensus_enforcement, []).

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
 *   constraint_id: institutional_consensus_enforcement
 *   human_readable: Institutional Consensus Enforcement Mechanism
 *   domain: governance/institutional_dynamics
 *
 * SUMMARY:
 *   Institutional consensus enforcement creates a structural tension between
 *   the genuine coordination problem (institutions need coherent direction to
 *   function effectively) and the extraction opportunity (leadership can use
 *   consensus enforcement to suppress internal dissent, eliminate
 *   accountability, and concentrate power without external challenge). This
 *   constraint operates across all institutional contexts — corporations,
 *   universities, governments, religious organizations, NGOs, professional
 *   bodies — and exhibits all six DR types from different structural
 *   positions. The same mechanism that coordinates institutional action
 *   (unified messaging, clear decision authority, stable norms)
 *   simultaneously extracts from members whose views diverge from the
 *   official consensus. The constraint's extractiveness has increased over
 *   the measurement interval (0.35 → 0.58) as institutions have built more
 *   sophisticated consensus-enforcement apparatus (internal investigation
 *   procedures, loyalty requirements, mandatory compliance training,
 *   reputation monitoring). Theater ratio has risen in parallel (0.42 → 0.68)
 *   as the mechanisms have become increasingly performative: formal
 *   procedures create appearance of due process while informal reputation
 *   destruction and career termination remain the real enforcement mechanism.
 *   The reform coalition perspective (Scaffold) identifies real sunset
 *   mechanisms: social media enabling whistleblowers, regulatory oversight of
 *   institutional conduct, academic freedom unions, public testimony
 *   platforms, and transparency mandates that increase the cost of visible
 *   suppression and create exit pathways for dissenting members.
 *
 * KEY AGENTS:
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — captures power concentration and authority certainty through consensus enforcement; can exit the constraint by reformulating consensus without institutional cost
 *   - Dissenting Members: Primary victims (powerless/trapped) — face career termination, social ostracism, institutional expulsion; trapped with no exit option
 *   - Cautious Reformers: Secondary victims (moderate/constrained) — benefit from institutional coordination but bear significant extraction costs through idea suppression and innovation dampening; face career risk for incremental dissent
 *   - Epistemic Diversity (abstract collective): Victim (powerless/trapped) — the shared good of diverse viewpoints and reality-testing cannot organize or exit; bears full cost of consensus enforcement through reduced institutional learning
 *   - Institutional Reform Coalition: Organized agents (organized/constrained) — whistleblowers, transparency advocates, academic freedom unions, social media platforms building alternative accountability mechanisms
 *   - Consensus Ritual Apparatus: Institutional actor maintaining performative mechanisms (loyalty oaths, investigation procedures, mandatory training) through inertia rather than function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_consensus_enforcement, 0.58).
domain_priors:suppression_score(institutional_consensus_enforcement, 0.65).
domain_priors:theater_ratio(institutional_consensus_enforcement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_consensus_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_consensus_enforcement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_consensus_enforcement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_consensus_enforcement, tangled_rope).
narrative_ontology:human_readable(institutional_consensus_enforcement, "Institutional Consensus Enforcement Mechanism").
narrative_ontology:topic_domain(institutional_consensus_enforcement, "governance/institutional_dynamics").

domain_priors:requires_active_enforcement(institutional_consensus_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_consensus_enforcement, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_consensus_enforcement, consensus_maintainers).
narrative_ontology:constraint_victim(institutional_consensus_enforcement, dissenting_members).
narrative_ontology:constraint_victim(institutional_consensus_enforcement, epistemic_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INTERNAL DISSIDENT (SNARE) — Member whose views diverge from consensus faces career termination, social ostracism, and institutional expulsion. Exit costs are total. The constraint operates through reputation destruction and institutional exile. Maximum extraction experienced by trapped agents with no exit path.
constraint_indexing:constraint_classification(institutional_consensus_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CAUTIOUS REFORMER (TANGLED ROPE) — Moderate power agent who values institutional membership but seeks incremental change. Benefits from coordination (institutional stability, clear norms) while bearing extraction costs (ideas constrained, innovation dampened). Significant cost but retains some agency through careful framing of dissent.
constraint_indexing:constraint_classification(institutional_consensus_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXECUTIVE LEADERSHIP (ROPE) — Views consensus enforcement as pure coordination mechanism: unified messaging, predictable institutional behavior, reduced decision paralysis. Leadership experiences the constraint as enabling their governance function. Net beneficiary with full exit option (can reformulate consensus without institutional cost).
constraint_indexing:constraint_classification(institutional_consensus_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized agents (internal dissidents, external critics, whistleblowers, transparency advocates) are building alternative accountability mechanisms (social media, regulatory oversight, public testimony, academic freedom unions) that create exit pathways and sunset clauses for consensus enforcement. See the constraint as temporary and solvable through institutional redesign with declining suppression over time.
constraint_indexing:constraint_classification(institutional_consensus_enforcement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSENSUS RITUAL APPARATUS (PITON) — The institutional mechanisms for enforcing consensus (loyalty oaths, internal investigation procedures, mandatory viewpoint training, peer pressure normalization) are substantially theatrical. The apparatus persists through institutional inertia: leadership maintains these rituals because they signal strength and control, but enforcement is often performative — the real power lies in informal reputation damage and career termination, not in the formal procedures. Theater ratio 0.68 reflects this performative character.
constraint_indexing:constraint_classification(institutional_consensus_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, all human institutions require some coordination mechanism, and consensus appears as a natural requirement for institutional function. The extraction attendant on consensus enforcement might appear as an inevitable cost of any organization. However, structural data contradicts this mountain classification — many institutions sustain coordination through alternative mechanisms (transparency, legitimacy, stakeholder buy-in, iterative dialogue) without heavy suppression. The mountain perspective naturalizes a specific institutional choice as universal law.
constraint_indexing:constraint_classification(institutional_consensus_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_consensus_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_consensus_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_consensus_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_consensus_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_consensus_enforcement, TR),
    TR >= 0.70.

:- end_tests(institutional_consensus_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. The original research (0.35) reflected a more benign assessment where consensus enforcement was primarily coordination-functional. The current assessment (0.58) reflects accumulated evidence that institutions use consensus enforcement as a primary mechanism for power concentration and suppression of internal accountability. The trajectory shows extractiveness increasing as institutions have built more sophisticated apparatus — this is not atrophy (declining function) but amplification (increasing extraction). The rising trend is a key diagnostic signal: institutions are doubling down on consensus enforcement as an extraction mechanism, not retreating from it. Suppression (0.65): High and sustained. Barriers to exit include career termination (most effective), social ostracism, reputational destruction, economic dependency on institutional affiliation, and internalized identity fusion with institutional viewpoint. Suppression is not total — some members do exit — but the costs are severe and widely understood. Theater ratio (0.68): High and rising. The formal consensus-enforcement apparatus (loyalty oaths, investigation procedures, grievance mechanisms, mandatory compliance training) is substantially performative. The apparatus creates appearance of fairness and due process while informal reputation damage and career termination remain the real enforcement mechanism. The rise in theater (0.42 → 0.68) reflects that institutions are investing more heavily in the appearance of consensus rather than building actual legitimacy or stakeholder buy-in.
 *
 * PERSPECTIVAL GAP:
 *   Maximum gap between beneficiary (Rope) and victim (Snare) perspectives. Leadership sees pure coordination function; dissidents see pure extraction mechanism. The gap reveals that consensus enforcement serves dual functions: genuine coordination of institutional action (legitimate) and power concentration preventing accountability (extractive). The tangled_rope classification captures this duality: both functions are simultaneously present.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary victim (powerless/trapped dissident) experiences maximum extraction because they bear full cost of consensus enforcement with no exit option and no institutional benefit. The primary beneficiary (institutional/arbitrage leadership) experiences negative effective extraction — the constraint subsidizes their power position by enabling suppression of accountability. The moderate-power cautious reformer experiences moderate extraction because they receive some institutional benefits (coordination, stability, clear authority) while bearing significant extraction costs (idea suppression, innovation constraint). The organized reform coalition experiences moderate-high extraction through their attempts to build alternatives, as institutions actively suppress transparency and accountability mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by disambiguating the coordination vs extraction functions: (1) Genuine coordination function: institutions do require coherent direction and unified messaging. Some consensus enforcement is legitimate coordination cost. (2) Extractive function: institutions use consensus enforcement far beyond the minimal level needed for coordination, deploying it as a mechanism for power concentration and suppression of internal accountability. The tangled_rope classification captures both simultaneously. The scaffold perspective identifies real sunset mechanisms: transparency mandates, regulatory oversight, whistleblower protections, and social media enabling dissidents to bypass institutional reputation management are creating rising costs for heavy consensus enforcement. The piton perspective reveals that institutions are increasingly maintaining consensus enforcement through theatrical rather than functional mechanisms — this is a degradation signal, indicating the constraint's legitimacy is eroding. The measurement trajectory (rising extractiveness, rising theater) shows institutions are compensating for declining legitimacy by escalating suppression, which is a classic pre-collapse pattern for inertial constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consensus_necessity_threshold,
    'What level of institutional consensus is actually necessary for function versus what level is enforced for power maintenance?',
    'Comparative institutional analysis: examine organizations with low consensus enforcement and high operational success (distributed leadership models, federated structures, pluralist firms) versus those with high enforcement and low innovation; measure correlation between suppression level and actual institutional outcomes',
    'If threshold low: consensus enforcement is largely extraction mechanism (Snare classification strengthens). If threshold high: enforcement is primarily coordination (Rope classification strengthens). This determines whether the constraint''s core function is coordination or domination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consensus_necessity_threshold, empirical, 'Institutional consensus necessity threshold for function').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.65) primarily structural (institutional mechanisms, career barriers, economic dependency) or internalized (members have internalized the consensus framing and police themselves)?',
    'Post-exit analysis: track members who leave the institution and measure whether suppression mechanisms persist (do they still self-censor, still fear reputational damage, still view dissent through the institution''s lens?); compare to members who exit with clean breaks; assess whether internalization decays over time after exit',
    'If primarily structural: suppression is changeable by institutional redesign. If primarily internalized: members carry the suppression with them — even exit does not fully liberate them. This affects whether the constraint is truly escapable (constrained exit) or whether it has created cognitive capture that persists beyond institutional membership (identity_locked dynamics).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    alternative_coordination_sufficiency,
    'Can institutional function be maintained through transparency, stakeholder input, and distributed authority rather than consensus enforcement?',
    'Field study of organizations using alternative coordination models (holacracy, consensus-based decision processes, transparency commitments); measure innovation rates, member satisfaction, institutional stability, and decision quality against consensus-enforcing institutions; assess failure modes of alternative approaches',
    'If alternatives work: consensus enforcement is revealed as preference for leadership control rather than institutional necessity (extraction-dominant classification). If alternatives fail: consensus enforcement may be necessary (coordination-dominant). This determines sunset possibility: scaffold perspective is structural if alternatives work, aspirational if they fail.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_coordination_sufficiency, empirical, 'Whether alternative coordination mechanisms can substitute for consensus enforcement').

omega_variable(
    identity_lock_mechanism,
    'Do members experience the suppression as external institutional barriers (trapped/constrained) or as internalized identity fusion with institutional viewpoint (identity_locked)?',
    'Interview and narrative analysis of members and former members: assess whether they frame consensus violation as ''external career cost'' (structural barrier framing) or ''betrayal of institutional identity/values'' (identity-fusion framing); measure post-exit continued identity alignment with institution; assess whether members update worldview after exit or continue applying institutional frames',
    'If identity_locked mechanisms dominate: the constraint operates on biographical time horizon as Rope (agent perceives mutability but cannot enact it), not as Mountain. This reveals cognitive capture rather than just economic dependence. Exit then requires not just bearing a cost but becoming a different person.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Identity lock vs structural suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_consensus_enforcement, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ice_tr_t0, institutional_consensus_enforcement, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ice_tr_t3, institutional_consensus_enforcement, theater_ratio, 3, 0.55).
narrative_ontology:measurement(ice_tr_t6, institutional_consensus_enforcement, theater_ratio, 6, 0.65).
narrative_ontology:measurement(ice_tr_t10, institutional_consensus_enforcement, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(ice_be_t0, institutional_consensus_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ice_be_t3, institutional_consensus_enforcement, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(ice_be_t6, institutional_consensus_enforcement, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(ice_be_t10, institutional_consensus_enforcement, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_consensus_enforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_consensus_enforcement, institutional_accountability_deficit).
narrative_ontology:affects_constraint(institutional_consensus_enforcement, organizational_epistemic_closure).
narrative_ontology:affects_constraint(institutional_consensus_enforcement, reputational_gatekeeping).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_consensus_enforcement, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
