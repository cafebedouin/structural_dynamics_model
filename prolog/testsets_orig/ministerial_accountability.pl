% ============================================================================
% CONSTRAINT STORY: ministerial_accountability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ministerial_accountability, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ministerial_accountability
 *   human_readable: Ministerial Accountability Constraint
 *   domain: political_governance
 *
 * SUMMARY:
 *   Ministerial accountability is the institutional constraint governing
 *   whether elected ministers remain responsible to legislative bodies and
 *   ultimately to the public. This constraint coordinates legitimate
 *   accountability functions (scrutiny of policy, constraint on arbitrary
 *   power) with extraction mechanisms (citizens bear policy costs without
 *   recourse; opposition parties have limited leverage). The constraint
 *   operates through parliamentary procedures (question periods, confidence
 *   votes, committee investigations), party discipline (whip system enforcing
 *   voting compliance), and electoral cycles. It is neither pure coordination
 *   (the accountability mechanisms are substantially performative) nor pure
 *   extraction (genuine oversight and policy scrutiny do occur). The theater
 *   ratio increasing from 0.52 to 0.68 reflects degradation: parliamentary
 *   procedures increasingly resemble ritual as party discipline hardens,
 *   reducing the correlation between questioning and policy change. The
 *   extractiveness rising from 0.42 to 0.58 reflects that without effective
 *   accountability mechanisms, ministers retain decision-making power while
 *   populations remain subject to their choices.
 *
 * KEY AGENTS:
 *   - Affected Public: Primary victim (powerless/trapped) — subject to ministerial decisions with no direct exit or accountability mechanism
 *   - Opposition Legislators: Secondary victim (moderate/constrained) — possess formal accountability tools (questions, motions) but limited by minority status and procedural constraints
 *   - Parliamentary Institution: Primary beneficiary (institutional/arbitrage) — benefits from coordination function; legitimacy derived from accountability role
 *   - Government Ministers: Secondary beneficiary (institutional/arbitrage) — benefit from procedural protections; confident that accountability is formalized and bounded
 *   - Party Leadership: Institutional actor (powerful/mobile) — uses party discipline to enforce voting compliance, reducing substantive accountability
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the hybrid structure: genuine coordination function + extraction mechanism maintained together
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ministerial_accountability, 0.58).
domain_priors:suppression_score(ministerial_accountability, 0.62).
domain_priors:theater_ratio(ministerial_accountability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ministerial_accountability, extractiveness, 0.58).
narrative_ontology:constraint_metric(ministerial_accountability, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(ministerial_accountability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ministerial_accountability, tangled_rope).
narrative_ontology:human_readable(ministerial_accountability, "Ministerial Accountability Constraint").
narrative_ontology:topic_domain(ministerial_accountability, "political_governance").

domain_priors:requires_active_enforcement(ministerial_accountability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ministerial_accountability, executive_power_holders).
narrative_ontology:constraint_beneficiary(ministerial_accountability, party_leadership).
narrative_ontology:constraint_victim(ministerial_accountability, public_welfare_outcomes).
narrative_ontology:constraint_victim(ministerial_accountability, affected_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED PUBLIC (SNARE) — Citizens subject to ministerial decisions have no mechanism to exit the jurisdiction or hold individual ministers accountable. Parliamentary procedures are opaque to direct participation. The constraint extracts compliance without reciprocal accountability, leaving the public bearing costs of failed policies with no direct recourse.
constraint_indexing:constraint_classification(ministerial_accountability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION LEGISLATORS (TANGLED ROPE) — Constrained by minority status and procedural rules that limit questioning. But also benefit from accountability mechanisms (interrogations, question periods) that provide political capital and media attention. High cost to exercising accountability (limited time, government control of agenda) alongside real coordination function (public scrutiny, policy debate).
constraint_indexing:constraint_classification(ministerial_accountability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARLIAMENTARY INSTITUTION (ROPE) — The institution itself coordinates between executive accountability and governmental stability. Parliament genuinely solves the coordination problem: how to scrutinize ministers while maintaining functional government. Benefits from the constraint through legitimacy and oversight function.
constraint_indexing:constraint_classification(ministerial_accountability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GOVERNMENT MINISTERS (ROPE) — Ministers benefit from the constraint through procedural protections (confidence votes require majorities, individual ministers cannot be removed without government collapse). The accountability mechanism is formalized and predictable. Ministers experience the constraint as coordination: they submit to interrogation in exchange for protection from ad-hoc removal.
constraint_indexing:constraint_classification(ministerial_accountability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PARTY LEADERSHIP (PITON) — The whip system and party discipline enforce accountability theater: MPs vote as directed regardless of constituent concerns. The accountability mechanism (question time, debates) is performative when underlying party discipline prevents real legislative consequences. Theater ratio reflects that parliamentary procedures produce appearance of accountability without corresponding decision-making power at individual minister level.
constraint_indexing:constraint_classification(ministerial_accountability, piton,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Structural analysis reveals ministerial accountability simultaneously coordinates three distinct functions: (1) legitimation of executive power through parliamentary consent, (2) periodic policy scrutiny, and (3) extraction of public compliance without effective recourse. All three mechanisms operate through the same institutional apparatus. The constraint is neither pure coordination nor pure extraction — it is hybrid, requiring active enforcement (party discipline) to maintain the mix.
constraint_indexing:constraint_classification(ministerial_accountability, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ministerial_accountability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ministerial_accountability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ministerial_accountability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ministerial_accountability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ministerial_accountability, TR),
    TR >= 0.70.

:- end_tests(ministerial_accountability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Ministers make consequential decisions affecting populations without requirement to reverse course based on public opposition or legislative dissent. The extractiveness is not absolute (some reversals occur; some scrutiny is effective) but substantial — the public cannot exit the jurisdiction and has limited mechanisms to compel accountability. The increasing trajectory reflects hardening party discipline and reduced effectiveness of opposition oversight. Suppression (0.62): Moderately high. Mechanisms exist to hold ministers accountable (parliamentary procedures, electoral accountability) but face significant barriers: party discipline overrides individual questioning; procedural limitations restrict opposition time; media coverage is often concentrated; electoral intervals are fixed (citizens cannot call immediate recall). Suppression is institutional, not purely coercive. Theater ratio (0.68): High. Parliamentary question periods, debates, and committee hearings are performed regularly but often with limited impact on ministerial decisions. Ministers prepare scripted responses; government controls parliamentary agenda; party discipline ensures voting compliance regardless of questioning effectiveness. The rituals maintain legitimacy (parliament can claim it is accountable; public can observe procedures) while actual decision-making power remains concentrated. Rising trajectory reflects increasing professionalization of political theater and hardening of party discipline.
 *
 * PERSPECTIVAL GAP:
 *   The affected public perceives a snare (trapped without recourse), while government ministers perceive a rope (accountable but protected). Opposition legislators perceive a tangled rope (genuine accountability function but constrained by minority status). The party leadership perceives a piton (accountability rituals maintained through discipline). The parliamentary institution itself perceives a rope (successfully coordinates accountability with governance). The analytical observer perceives a tangled rope (hybrid mechanism maintaining both coordination and extraction). The perspectival gap reveals that the same institutional apparatus produces genuinely different outcomes depending on structural position: for those benefiting from executive power, the constraint is manageable and coordinating; for those subject to executive decisions, it is inadequate and extractive. The gap is not bridgeable by better procedures alone — it reflects asymmetric stakes in the accountability outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position within the accountability mechanism. Citizens and opposition legislators are victims without exit options (d approaching 1.0 → high f(d) → high chi). They experience the full extraction flow: bear costs of policy without power to reverse it. Government ministers and party leadership are beneficiaries with substantial exit options (d approaching 0.0 → low f(d) → negative chi). They benefit from the constraint through protection and predictability. The parliamentary institution occupies a middle position (d ≈ 0.5) — it coordinates accountability (genuine function) while also enabling executive power (extraction mechanism). The directionality mapping explains why opposition sees snare while government sees rope: opposite positions in the extraction flow, identical institutional apparatus.
 *
 * MANDATROPHY ANALYSIS:
 *   Ministerial accountability exemplifies the mandatrophy at moderate extractiveness levels. The constraint simultaneously appears as: (1) essential democratic coordination mechanism (rope: parliament needs to coordinate executive accountability), (2) extraction mechanism (snare: citizens pay costs without recourse), and (3) degraded ritual (piton: procedures maintained despite reduced effectiveness). No single classification captures the structural reality. The resolution is perspectival: the classification depends on where you stand in the institutional hierarchy and whether you benefit from or pay the cost of executive decision-making. The tangled rope classification at the analytical level reflects this irreducible hybridity — the constraint genuinely coordinates accountability while also extracting compliance and maintaining asymmetric power. The increasing theater ratio signals mandatrophy risk: if parliamentary procedures become pure ritual without corresponding policy impact, the constraint risks collapsing from tangled rope (mixed coordination and extraction) toward piton (theatrical maintenance of degraded function) or even snare (extraction without pretense of accountability).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    confidence_vote_threshold,
    'Do confidence vote requirements genuinely constrain executive power or merely ratify predetermined party decisions?',
    'Historical analysis of confidence vote outcomes; correlation between parliamentary dissent and actual government survival; comparison of confidence vote vulnerability across majority/minority governments',
    'If genuinely constraining: accountability is rope-level (real coordination). If ratification only: accountability is piton-level (performative). Shifts classification from tangled_rope toward piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(confidence_vote_threshold, empirical, 'Whether confidence votes provide real executive constraint').

omega_variable(
    question_period_policy_impact,
    'Do parliamentary question periods produce measurable changes in ministerial decisions or are they performative ritual?',
    'Tracking of questions asked vs policy reversals; analysis of legislative record following high-profile interrogations; interview data on ministerial decision-making process',
    'If questions cause policy change: accountability has coordination function (rope). If purely ritualistic: accountability is theater (piton). Determines whether tangled_rope classification holds or collapses toward piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(question_period_policy_impact, empirical, 'Whether questioning produces genuine ministerial accountability').

omega_variable(
    party_discipline_binding_mechanism,
    'Is party discipline structural (threatens career/nomination/funding) or internalized (identity fusion with party ideology)?',
    'Analysis of defection patterns and consequences; study of backbench revolts and MP exits; distinction between external career pressure and ideological commitment',
    'If structural: extraction runs through institutional mechanism (snare classification for powerless agents holds). If internalized: victims are identity_locked rather than trapped (classification shifts in biographical horizon for opposition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(party_discipline_binding_mechanism, empirical, 'Whether party discipline is structural or ideological').

omega_variable(
    sunset_mechanism_credibility,
    'Do regular elections function as a genuine sunset clause for ministerial accountability constraint, or do they embed it further through normalize it?',
    'Analysis of whether electoral cycles produce alternating power; tracking of policy reversals post-election; assessment of whether non-governing party uses accountability mechanisms differently than governing party',
    'If genuine sunset: accountability could reclassify as scaffold (temporary coordination with real exit point). If embedding: elections ratify rather than constrain, maintaining tangled_rope or snare classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_mechanism_credibility, empirical, 'Whether elections provide genuine constraint reset').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ministerial_accountability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(minac_tr_t0, ministerial_accountability, theater_ratio, 0, 0.52).
narrative_ontology:measurement(minac_tr_t10, ministerial_accountability, theater_ratio, 10, 0.6).
narrative_ontology:measurement(minac_tr_t20, ministerial_accountability, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(minac_be_t0, ministerial_accountability, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(minac_be_t10, ministerial_accountability, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(minac_be_t20, ministerial_accountability, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ministerial_accountability, enforcement_mechanism).
narrative_ontology:affects_constraint(ministerial_accountability, parliamentary_representation).
narrative_ontology:affects_constraint(ministerial_accountability, executive_power_concentration).
narrative_ontology:affects_constraint(ministerial_accountability, electoral_legitimacy).

% DUAL FORMULATION NOTE:
% Ministerial accountability is structurally distinct from legislative representation (which coordinates diverse constituent voices into collective decision) and executive authority (which concentrates decision-making). This story focuses on the accountability mechanism that bridges executive power and parliamentary oversight. Upstream constraints include electoral legitimacy (whether elections produce representative outcomes); downstream constraints include regulatory capture (whether executive becomes captured by regulated industries) and party discipline effects (whether whip systems undermine individual accountability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
