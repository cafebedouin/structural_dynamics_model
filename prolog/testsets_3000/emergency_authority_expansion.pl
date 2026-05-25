% ============================================================================
% CONSTRAINT STORY: emergency_authority_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emergency_authority_expansion, []).

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
 *   constraint_id: emergency_authority_expansion
 *   human_readable: Emergency Authority Expansion
 *   domain: political/governance
 *
 * SUMMARY:
 *   Emergency authority expansion creates a structural constraint where the
 *   genuine need for rapid crisis response becomes a mechanism for extracting
 *   civil liberties and centralizing political power. The constraint exhibits
 *   temporal drift: extractiveness increases from 0.35 at emergency
 *   declaration to 0.62 as emergency powers persist and normalize.
 *   Theater_ratio also increases (0.25 to 0.55), indicating growing
 *   performative justification as the original crisis urgency fades but the
 *   institutional expansion remains. The tension between Scaffold (temporary
 *   coordination mechanism with sunset) and Snare (permanent extraction
 *   dressed as temporary) is the central analytic question — most emergency
 *   powers begin with genuine coordination function but drift toward
 *   extraction through normalization and institutional inertia. Different
 *   perspectives reveal this duality: the executive sees pure coordination
 *   (Rope), citizens experience pure extraction (Snare), legislatures and
 *   courts see mixed function (Tangled Rope), oversight institutions maintain
 *   the fiction of sunset (Scaffold), the legal apparatus performs outdated
 *   justifications (Piton), and the civilizational observer risks
 *   naturalizing the arrangement as inevitable (false Mountain).
 *
 * KEY AGENTS:
 *   - Executive Leadership: Primary beneficiary (institutional/arbitrage) — captures authority expansion with minimal personal cost; can declare emergency end at will
 *   - Security Apparatus: Secondary beneficiary (institutional/arbitrage) — gains expanded surveillance, detention, and enforcement capacity with reduced oversight
 *   - Ordinary Citizens: Primary victim (powerless/trapped) — subject to suspension of normal legal protections with no exit option or ability to challenge expansion
 *   - Legislative Bodies: Secondary victim (moderate/constrained) — oversight capacity suspended or bypassed; career risk for those who resist emergency measures
 *   - Civil Society and Media: Secondary victim (moderate/constrained) — scrutiny dampened, organizing capacity restricted, reporting subject to state control claims
 *   - Constitutional Courts: Organized arbiter (organized/mobile) — possess capacity to review and terminate emergency powers but face pressure not to appear obstructionist during crisis
 *   - International Bodies: Organized arbiter (organized/mobile) — can invoke pressure on member states but limited enforcement capacity during national emergencies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emergency_authority_expansion, 0.62).
domain_priors:suppression_score(emergency_authority_expansion, 0.68).
domain_priors:theater_ratio(emergency_authority_expansion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emergency_authority_expansion, extractiveness, 0.62).
narrative_ontology:constraint_metric(emergency_authority_expansion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(emergency_authority_expansion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emergency_authority_expansion, tangled_rope).
narrative_ontology:human_readable(emergency_authority_expansion, "Emergency Authority Expansion").
narrative_ontology:topic_domain(emergency_authority_expansion, "political/governance").

domain_priors:requires_active_enforcement(emergency_authority_expansion).
narrative_ontology:has_sunset_clause(emergency_authority_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emergency_authority_expansion, executive_leadership).
narrative_ontology:constraint_beneficiary(emergency_authority_expansion, security_apparatus).
narrative_ontology:constraint_victim(emergency_authority_expansion, civil_liberties).
narrative_ontology:constraint_victim(emergency_authority_expansion, legislative_oversight).
narrative_ontology:constraint_victim(emergency_authority_expansion, ordinary_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY CITIZEN (SNARE) — Faces maximum extraction with no exit option. Subject to expanded surveillance, detention authority, movement restrictions, and property seizure with minimal due process. Cannot challenge the constraint or exit the jurisdiction during emergency. Suppression is structural: emergency powers explicitly suspend or constrain normal legal protections.
constraint_indexing:constraint_classification(emergency_authority_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LEGISLATOR / CIVIL SOCIETY (TANGLED ROPE) — Constrained by genuine emergency conditions (external threat or crisis) that create real coordination need for rapid response. But also extracted from: legislative capacity is suspended or bypassed, civil society scrutiny is dampened, and institutional memory of emergency powers' abuse creates career/reputational risk for those who resist. Mixed extraction and coordination — genuine emergency function alongside asymmetric power concentration.
constraint_indexing:constraint_classification(emergency_authority_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE LEADERSHIP (ROPE) — Primary beneficiary. Experiences emergency powers as pure coordination: the constraint enables rapid response to crisis that would be impossible under normal deliberative procedures. Has arbitrage exit (can declare emergency end, invoke alternative coordination mechanisms). Benefits from authority expansion far outweigh costs — extracts significant capacity with minimal personal exposure.
constraint_indexing:constraint_classification(emergency_authority_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL COURTS / INTERNATIONAL OVERSIGHT (SCAFFOLD) — Organized agents with independent authority see emergency expansion as a temporary measure with explicit sunset. Possess capacity to review, constrain, and terminate emergency powers. Have mobile exit options (can escalate to international bodies, invoke constitutional limits). Theater_ratio is moderate because some genuine oversight occurs alongside the performative aspect of declaring emergency 'temporary' while mechanisms persist. Sunset is real if institutional actors maintain independence — real constraint on extension if courts enforce time limits.
constraint_indexing:constraint_classification(emergency_authority_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL-INSTITUTIONAL APPARATUS (PITON) — The formal legal structure (constitutional articles on emergency, statutory emergency powers, procedural requirements) persists long after the original crisis has passed. Theater_ratio is moderate because the apparatus performs 'emergency justification' rituals even when the original rationale has atrophied. Courts review claims without reversing expansions; legislatures reassert authority without reclaiming suspended powers; citizens accept restrictions as 'temporary' while they persist indefinitely. Institutional inertia maintains the constraint despite degraded emergency function.
constraint_indexing:constraint_classification(emergency_authority_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From civilizational scope, emergency authority expansion appears as an immutable feature of political organization: every state faces crises that require rapid response, and no state can survive without emergency mechanisms. The constraint appears inherent to governance itself — the structural need for executive agility during crisis. However, this perspective risks naturalizing what is actually a contingent institutional choice: the extent of authority expansion, the duration, the oversight mechanisms, and the procedural safeguards are all variable. Different states with different constitutional designs show vastly different extraction profiles during emergencies — the constraint is not mountain-shaped from a comparative institutional perspective.
constraint_indexing:constraint_classification(emergency_authority_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emergency_authority_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(emergency_authority_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(emergency_authority_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(emergency_authority_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(emergency_authority_expansion, TR),
    TR >= 0.70.

:- end_tests(emergency_authority_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. Emergency authority expansion creates significant extraction from civil liberties, due process, and democratic participation. However, extraction is not maximal (0.80+) because genuine emergency coordination needs create some legitimate justification, and oversight mechanisms (even if degraded) retain some capacity to constrain. The intermediate value reflects the Tangled Rope reality: real coordination function mixed with asymmetric extraction. Suppression (0.68): Moderate-high. Emergency powers explicitly suspend or constrain normal legal protections, restrict movement and assembly, enable detention without normal due process, and expand surveillance. Suppression is structural, not internalized. Theater_ratio (0.55): Moderate-high, increasing over time. The constraint operates through performative justifications: declaring measures 'temporary,' invoking 'sunset clauses' that are renewed or replaced, maintaining oversight rituals that lack enforcement capacity. As the original crisis fades, the theater increases because institutional expansion persists without genuine emergency rationale.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence across structural positions. The executive sees pure coordination (Rope) — emergency measures genuinely solve the crisis-response problem that normal procedures cannot handle. The citizen sees pure extraction (Snare) — civil liberties suspended with no reciprocal benefit or exit option. The legislator sees mixed function (Tangled Rope) — some genuine emergency coordination but also displacement of democratic authority. Courts see temporary extraction (Scaffold) — if they enforce sunset clauses, the constraint is time-limited; if they don't, the 'temporary' framing becomes theater. The legal apparatus sees its own degradation (Piton) — formal procedures persist for ritual compliance while emergency powers bypass them. The civilizational observer risks seeing immutable necessity (false Mountain) — but the gap between this naturalization and the structured extraction visible from other perspectives reveals the false summit: emergency authority expansion is a contingent institutional choice, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural relationship to the extraction flow. Executive leadership and security apparatus benefit from authority expansion and can exit at will (arbitrage) — low d values, negative or low chi. Ordinary citizens are trapped by emergency jurisdiction and possess zero exit options — maximum d, maximum chi. Legislatures and civil society actors face constrained exit (high career/political cost to resistance) and bear costs while deriving some benefit from stability — moderate d, moderate chi. Courts and international bodies have mobile exit (can escalate, can invoke legal limits) and face reputational incentives to maintain constraint-limiting credibility — lower d than the trapped actors. The scaffolding perspective (courts, international oversight) sees the constraint as genuinely temporary if they maintain independence — mobile exit option produces lower effective extraction. The piton perspective (legal apparatus) experiences constrained exit because the institutional apparatus perpetuates the formal structure even when original rationale atrophies.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the core mandatrophy problem: is emergency authority expansion a genuine coordination mechanism (Rope/Scaffold) or a disguised extraction mechanism (Snare/Piton)? The classification depends entirely on whether sunset clauses are enforced and whether oversight institutions maintain independence. The theater_ratio drift (0.25 → 0.55) suggests that the original coordination function (rapid crisis response) is being replaced with performative justification (maintaining expanded authority without genuine emergency need). The measured extractiveness increase (0.35 → 0.62) confirms this drift: as crisis urgency fades, extraction becomes visible. Resolution requires empirical investigation of omega variables: Do sunset clauses actually terminate powers, or are they renewed in perpetuity? Do courts enforce constitutional limits, or do they defer to executive claims of continuing emergency? If sunset clauses are routinely renewed and courts defer, the constraint is fundamentally Snare dressed as Scaffold — the mandatrophy is resolved by recognizing the temporal dynamics: Scaffold at t=0 (genuine temporary mechanism), Piton at t=5+ (degraded apparatus maintained by institutional inertia), Snare overall (asymmetric extraction disguised by sunset theater).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_definition_scope,
    'What events qualify as genuine emergencies justifying authority expansion? Where is the boundary between legitimate crisis response and pretext for power consolidation?',
    'Comparative institutional analysis: what types of events do different constitutional systems classify as emergencies? Which events actually required expanded executive authority to resolve effectively vs which could have been managed through normal processes?',
    'If emergency definition is narrow and tightly enforced: constraint approaches Rope (genuine coordination for narrow crises). If definition expands over time or is loosely enforced: constraint becomes Snare (extraction disguised as emergency response).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_definition_scope, conceptual, 'Definition and scope of genuine emergencies').

omega_variable(
    sunset_clause_enforcement,
    'Do sunset clauses actually terminate emergency powers, or do they persist through re-declaration cycles, legislative extension, or institutional inertia?',
    'Historical tracking of emergency powers: what percentage are actually terminated at sunset vs renewed/extended/replaced with permanent legislation? Timeline of power persistence vs crisis duration.',
    'If sunset clauses are enforced: constraint is genuinely Scaffold with time-limited extraction. If routinely renewed or replaced: constraint becomes Snare or Piton (temporary framing conceals permanent extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_clause_enforcement, empirical, 'Whether sunset clauses actually terminate emergency powers').

omega_variable(
    oversight_effectiveness,
    'Does institutional oversight (courts, legislatures, international bodies) actually constrain emergency authority expansion, or do oversight institutions themselves become coopted or sidelined during emergencies?',
    'Institutional capacity analysis: how many emergency measures are actually struck down by courts? How much legislative scrutiny occurs vs rubber-stamp approval? What happens to institutional independence during prolonged emergencies?',
    'If oversight is effective: constraint is Tangled Rope or Scaffold. If oversight is bypassed or coopted: constraint becomes Snare (maximum extraction with minimal accountability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oversight_effectiveness, empirical, 'Whether institutional oversight constrains emergency authority').

omega_variable(
    normalization_trajectory,
    'Do emergency powers normalize into permanent institutional features? What is the typical trajectory from temporary measure to permanent apparatus?',
    'Historical analysis of emergency powers that became permanent: mass surveillance after 9/11, preventive detention frameworks, military command authority, etc. Timeline from emergency declaration to full normalization.',
    'If normalization is systematic: the constraint''s true classification is masked by the temporary framing — it is actually Snare pretending to be Scaffold. Theater_ratio reveals this (moderate values suggest performative ''sunset'' claims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(normalization_trajectory, empirical, 'Normalization of emergency powers into permanent institutions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emergency_authority_expansion, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emerg_tr_t0, emergency_authority_expansion, theater_ratio, 0, 0.25).
narrative_ontology:measurement(emerg_tr_t2, emergency_authority_expansion, theater_ratio, 2, 0.4).
narrative_ontology:measurement(emerg_tr_t5, emergency_authority_expansion, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(emerg_be_t0, emergency_authority_expansion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(emerg_be_t2, emergency_authority_expansion, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(emerg_be_t5, emergency_authority_expansion, base_extractiveness, 5, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emergency_authority_expansion, enforcement_mechanism).
narrative_ontology:affects_constraint(emergency_authority_expansion, civil_liberties_suspension).
narrative_ontology:affects_constraint(emergency_authority_expansion, democratic_accountability_constraint).
narrative_ontology:affects_constraint(emergency_authority_expansion, surveillance_normalization).

% DUAL FORMULATION NOTE:
% Emergency authority expansion is upstream of constraints on civil liberties and democratic accountability. The structural mechanism (suspension of normal oversight) is the same across all three — they are decomposed by observable and affected actor rather than by fundamental epsilon difference.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(emergency_authority_expansion, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
