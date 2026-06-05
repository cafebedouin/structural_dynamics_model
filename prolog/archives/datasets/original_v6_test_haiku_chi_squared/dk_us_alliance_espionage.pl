% ============================================================================
% CONSTRAINT STORY: dk_us_alliance_espionage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dk_us_alliance_espionage, []).

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
 *   constraint_id: dk_us_alliance_espionage
 *   human_readable: Implicit 'No Industrial Espionage' Norm within Western Alliances
 *   domain: geopolitical/alliance_governance
 *
 * SUMMARY:
 *   The implicit 'no industrial espionage' norm within NATO represents a
 *   structural tension between alliance coordination (genuine need for
 *   integrated intelligence for collective defense) and hegemonic extraction
 *   (US privileged access to allied industrial secrets, technological
 *   capabilities, and internal security assessments). This constraint emerged
 *   as a formal principle during the Cold War when the common Soviet threat
 *   aligned incentives for comprehensive intelligence-sharing. Post-1991, the
 *   threat justification degraded while the extraction mechanism persisted,
 *   creating the contemporary form: a hybrid coordination-extraction system
 *   maintained through diplomatic theatre and plausible deniability. The
 *   constraint exhibits all six DR types depending on perspective. The allied
 *   nation (Denmark) experiences maximal extraction (Snare). The NATO
 *   collective experiences mixed coordination and extraction (Tangled Rope).
 *   The US intelligence apparatus frames it as pure coordination (Rope) for
 *   interoperability and threat reduction. The NATO institutional framework
 *   performs commitment to the norm while tacitly accepting asymmetric
 *   enforcement (Tangled Rope). The Cold War intelligence architecture
 *   persists through institutional inertia despite its original justification
 *   degrading (Piton). The analytical observer risks naturalizing the
 *   asymmetry as inherent to all hegemonic systems (false summit). The
 *   constraint's theater_ratio has increased from ~0.35 (early Cold War, when
 *   the norm had real coordination content) to ~0.62 (contemporary, when much
 *   of the 'intelligence-sharing' is asymmetric extraction framed as
 *   partnership). Extractiveness has similarly increased from ~0.32 to ~0.58
 *   as allied nations developed industrial capabilities worth surveilling.
 *
 * KEY AGENTS:
 *   - US Intelligence Apparatus (NSA, CIA, Five Eyes): Primary beneficiary (institutional/arbitrage) — captures industrial, technological, and diplomatic intelligence from allies without equivalent exposure
 *   - Allied Nation (Denmark, Nordic/European states): Primary victim (powerless/trapped) — cannot exit alliance without security cost; bears surveillance and industrial espionage
 *   - European Coalition (EU/NATO institutions): Organized victim (organized/constrained) — can resist through GDPR, diplomatic pressure, counter-intelligence but constrained by dependence on US security guarantees
 *   - NATO Institutional Framework: Coordinating actor (institutional/constrained) — must maintain alliance cohesion while tacitly accepting US intelligence privilege
 *   - Cold War Intelligence Architecture (Five Eyes legacy): Historical institutional actor — persists through inertia despite original threat justification degrading
 *   - Analytical Observer (International Relations perspective): Civilizational view (analytical/analytical) — risks naturalizing hegemonic extraction as structural law rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dk_us_alliance_espionage, 0.58).
domain_priors:suppression_score(dk_us_alliance_espionage, 0.68).
domain_priors:theater_ratio(dk_us_alliance_espionage, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dk_us_alliance_espionage, extractiveness, 0.58).
narrative_ontology:constraint_metric(dk_us_alliance_espionage, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dk_us_alliance_espionage, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dk_us_alliance_espionage, tangled_rope).
narrative_ontology:human_readable(dk_us_alliance_espionage, "Implicit 'No Industrial Espionage' Norm within Western Alliances").
narrative_ontology:topic_domain(dk_us_alliance_espionage, "geopolitical/alliance_governance").

domain_priors:requires_active_enforcement(dk_us_alliance_espionage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dk_us_alliance_espionage, us_intelligence_apparatus).
narrative_ontology:constraint_beneficiary(dk_us_alliance_espionage, hegemon_information_advantage).
narrative_ontology:constraint_victim(dk_us_alliance_espionage, allied_industrial_security).
narrative_ontology:constraint_victim(dk_us_alliance_espionage, technological_sovereignty).
narrative_ontology:constraint_victim(dk_us_alliance_espionage, trust_in_alliance_framework).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALLIED NATION (SNARE) — Trapped within NATO; cannot credibly exit without loss of security guarantees, defense cooperation, and regional stability. Bears extraction of industrial/technological information. Cannot monitor surveillance targeting. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72. Maximal effective extraction from constrained-plus-trapped position.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EUROPEAN ALLIANCE COALITION (TANGLED ROPE) — Organized capacity to resist (GDPR, diplomatic pressure, counter-intelligence) but constrained by dependence on NATO/US security umbrella. Experiences genuine coordination (NATO interoperability, intelligence-sharing frameworks) alongside asymmetric intelligence extraction. d≈0.58, f(d)≈0.75, σ=1.1 → χ≈0.48. Mixed extraction with active resistance capacity.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US INTELLIGENCE APPARATUS (ROPE) — Frames the constraint as coordination: all-source intelligence reduces ambiguity in threat assessment, enables integrated defense planning, prevents strategic surprise. Benefits from privileged access to allied technical capabilities and industrial secrets. Interprets the 'norm' as a coordination device for information flow optimization. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; negative effective extraction indicates the apparatus gains more from the 'norm' than it pays.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NATO INSTITUTIONAL FRAMEWORK (TANGLED ROPE) — Must coordinate alliance defense while tacitly tolerating US intelligence asymmetry. Requires active enforcement: member states must maintain public commitment to intelligence-sharing while privately accepting that some 'sharing' flows only one direction. Theater_ratio reflects the performative language of 'partnership' masking extraction. d≈0.50, f(d)≈0.65, σ=1.1 → χ≈0.41. Symmetric interest in maintaining alliance; asymmetric enforcement (USA enforces norms against non-allies, not itself).
constraint_indexing:constraint_classification(dk_us_alliance_espionage, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: COLD WAR INSTITUTIONAL LEGACY (PITON) — The 'norm against industrial espionage' is a degraded form of the historical Five Eyes intelligence-sharing architecture, which emerged from genuine mutual threat (USSR). The norm persists through institutional inertia and diplomatic theater despite the original justification (common enemy threat) degrading post-1991. theater_ratio=0.62 reflects that the norm is maintained through performative language and plausible deniability rather than active institutional function. The extraction mechanism (surveillance, data access) continues without the coordination benefit that originally justified it.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT) — At civilizational scale, power asymmetry appears immutable: all hegemonies extract information from periphery; alliance subordinates always lose secrets to superiors; intelligence advantage is inherent to military hierarchy. However, this classification (ε=0.58, suppression=0.68, theater=0.62) contradicts mountain thresholds (ε≤0.25, suppression≤0.05). The 'natural law' framing naturalizes what is actually a contingent institutional choice: the USA could enforce the norm symmetrically (e.g., through binding intelligence audit protocols), but chooses not to. Engine will flag as false summit.
constraint_indexing:constraint_classification(dk_us_alliance_espionage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dk_us_alliance_espionage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dk_us_alliance_espionage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dk_us_alliance_espionage, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dk_us_alliance_espionage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dk_us_alliance_espionage, TR),
    TR >= 0.70.

:- end_tests(dk_us_alliance_espionage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing. The US gains significant advantage through asymmetric access to allied industrial secrets (semiconductor supply chain intelligence, defense contractors' IP, digital infrastructure vulnerabilities). The extraction is not maximal because: (1) allies do maintain counter-intelligence capabilities, (2) some intelligence-sharing is genuinely mutual (operationally relevant threat information), (3) surveillance is constrained by diplomatic sensitivity. The increasing trend (0.32→0.58) reflects post-Cold War degradation: as Soviet threat receded, the original coordination justification weakened, leaving pure extraction unmasked. Suppression (0.68): Moderately high. Allied nations face significant barriers to resisting: exiting NATO incurs security cost; counter-surveillance risks retaliation in intelligence-sharing; diplomatic complaints risk marginalization. However, suppression is not total—EU/GDPR provides some friction, allied counter-intelligence can document violations, leaks (Snowden) expose the system. Theater ratio (0.62): Moderate-high, increasing. The norm is maintained through performative language ('partnership,' 'intelligence-sharing allies,' 'trust'). The Snowden files revealed this theatre—the 'norm' was being violated at scale but through plausible deniability. Contemporary surveillance (post-Snowden) continues with slightly more transparency about the asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Allied nations see extraction (Snare d≈0.92) — they are trapped, surveilled, and cannot exit. The European coalition sees mixed extraction-coordination (Tangled Rope d≈0.58) — they have organized resistance capacity but constrained by alliance dependence. The US apparatus sees pure coordination (Rope d≈0.08) — framing surveillance as threat reduction and operational integration. NATO institutions perform commitment to the norm (Tangled Rope d≈0.50) — enforcing symmetric principles while tacitly permitting asymmetric practice. The Cold War institutional legacy sees degraded ritual (Piton) — the norm persists through theatre despite its original coordination function degrading. The civilizational analytical observer risks seeing immutable hegemonic structure (false summit) — 'all hegemonies extract secrets; this is structural.' The engine will flag the summit as false, revealing that the asymmetry is a contingent institutional choice (treaty-formalized symmetric verification is technically feasible but politically chosen against).
 *
 * DIRECTIONALITY LOGIC:
 *   Allied nation (victim + trapped): d≈0.92, f(d)≈1.38. Maximum extraction. No exit without security cost; cannot organize collective defense outside NATO. US apparatus (beneficiary + arbitrage): d≈0.08, f(d)≈-0.10. Net beneficiary. Can exit the norm with minimal cost (alternative intelligence partnerships); captures asymmetric advantage. European coalition (victim + constrained): d≈0.58, f(d)≈0.75. Significant extraction but with organized resistance. Can partially exit through counter-intelligence, GDPR enforcement, diplomatic pressure, but constrained by alliance value. NATO institutional (both + constrained): d≈0.50, f(d)≈0.65. Symmetric interest in maintaining alliance framework; asymmetric enforcement (USA enforces selectively against non-allies, not itself). Cold War legacy (institutional + arbitrage): d≈0.05, f(d)≈-0.12. Net beneficiary from institutional inertia. Analytical observer (analytical): d≈0.72, f(d)≈1.15. Mountain derivation attempts to naturalize the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint resolves the mandatrophy by showing how a single structural phenomenon can be legitimately read as Snare (allied perspective), Tangled Rope (coalition perspective), Rope (beneficiary perspective), Piton (institutional legacy perspective), and false-summit Mountain (naturalizing perspective). No single type is correct — the presheaf of perspectives IS the constraint. The mandatrophy is resolved by accepting that: (1) All classifications are empirically accurate from their respective observables. (2) The allied nation genuinely experiences Snare-level extraction. (3) The US apparatus genuinely frames it as Rope-level coordination. (4) The institutional system genuinely performs Tangled Rope dynamics while degrading toward Piton. (5) The false summit (naturalization of extraction as structural law) is a classification error that the engine detects. The constraint is structurally a Tangled Rope at the NATO level (mixed coordination + asymmetric extraction, active enforcement required) but with significant variance across perspectives. If the norm were formalized into explicit symmetric audit protocols, the entire constraint family would shift toward Rope. If the norm were abandoned, the classification would collapse into pure Snare from allied perspective. The machinery for renegotiation exists (treaty formalization is technically feasible); the constraint persists because the US apparatus benefits from asymmetric enforcement and the allied nations are trapped within the coordination framework that benefits them defensively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    surveillance_scope_boundary,
    'Does the implicit norm distinguish between: (a) foreign intelligence collection on allied governments/militaries (permitted), (b) industrial espionage targeting private companies (prohibited), or (c) no real distinction in practice?',
    'Declassified documents, leaks (e.g., Snowden files), forensic analysis of NSA targeting; historical cases where industrial espionage crossed a stated norm boundary',
    'If (a): the constraint is narrower than assumed — espionage on allied industry is actually permitted and the norm is theatre. If (c): there is no real norm, only plausible deniability, and the constraint is pure extraction (Snare from allied perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(surveillance_scope_boundary, empirical, 'Scope of the no-espionage norm: industrial vs governmental vs blanket').

omega_variable(
    enforcement_asymmetry_reversibility,
    'Is the asymmetry in enforcement (USA does not face credible espionage from allies) a structural feature of hegemonic power or a negotiable treaty provision?',
    'Counterfactual: if Denmark or France conducted equivalent surveillance on US industry, would they face sanction? Evidence from attempted counter-surveillance incidents; diplomatic responses to allied intelligence operations.',
    'If structural: no alternative norm is possible (mountain). If negotiable: the constraint could be renegotiated as symmetric enforcement (Rope) through binding protocols and verification mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_reversibility, conceptual, 'Whether enforcement asymmetry is inherent to hegemony or renegotiable').

omega_variable(
    coordination_benefit_residual,
    'How much of the contemporary intelligence-sharing benefit derives from: (a) genuine threat reduction (original Cold War justification), (b) interoperability and joint operations (ongoing), or (c) US access to allied industrial secrets (extraction with coordination framing)?',
    'Analysis of NATO intelligence product flow; case studies of operations where intelligence-sharing prevented specific threats vs operations where shared intelligence was primarily one-directional extraction',
    'If mostly (a)+(b): constraint is legitimate Rope or Tangled Rope with real coordination. If mostly (c): the ''coordination'' framing is theatre and the constraint is pure Snare (with elaborate institutional cover). Affects whether the piton (degraded) perspective is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_benefit_residual, empirical, 'Proportion of intelligence-sharing that serves coordination vs extraction').

omega_variable(
    treaty_formalization_feasibility,
    'Could the implicit norm be converted into explicit treaty language with symmetric verification (e.g., industrial espionage audit protocols, mutual inspection regimes)?',
    'Examination of similar formalization attempts (e.g., IAEA verification, arms control treaties); interviews with negotiators; analysis of why explicit treaties on industrial espionage do not exist in NATO framework',
    'If feasible: the constraint''s current form (implicit, asymmetric) is a choice favoring extraction over coordination; formalization could shift it to Rope. If infeasible: the asymmetry is structurally embedded and the constraint is locked in Snare/Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(treaty_formalization_feasibility, conceptual, 'Whether implicit norm can be formalized into explicit symmetric treaty').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dk_us_alliance_espionage, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dkus_tr_t0, dk_us_alliance_espionage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dkus_tr_t20, dk_us_alliance_espionage, theater_ratio, 20, 0.5).
narrative_ontology:measurement(dkus_tr_t35, dk_us_alliance_espionage, theater_ratio, 35, 0.62).

% Extraction over time
narrative_ontology:measurement(dkus_be_t0, dk_us_alliance_espionage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(dkus_be_t20, dk_us_alliance_espionage, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(dkus_be_t35, dk_us_alliance_espionage, base_extractiveness, 35, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dk_us_alliance_espionage, enforcement_mechanism).
narrative_ontology:affects_constraint(dk_us_alliance_espionage, nato_interoperability_standards).
narrative_ontology:affects_constraint(dk_us_alliance_espionage, five_eyes_intelligence_asymmetry).
narrative_ontology:affects_constraint(dk_us_alliance_espionage, allied_defensive_integration).

% DUAL FORMULATION NOTE:
% The no-espionage norm is downstream of the broader Five Eyes intelligence architecture (ε lower, more firmly embedded). It also affects downstream constraints on allied technological sovereignty (ε higher, more contested). The family decomposes along the coordination/extraction axis: Five Eyes architecture (more Rope, higher coordination benefit) → no-espionage norm (Tangled Rope, mixed) → allied industrial espionage exposure (more Snare, higher extraction). Each story has distinct ε reflecting the balance of coordination and extraction at its level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dk_us_alliance_espionage, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
