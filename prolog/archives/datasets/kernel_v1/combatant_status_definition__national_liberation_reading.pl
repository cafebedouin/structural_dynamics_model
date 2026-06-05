% ============================================================================
% CONSTRAINT STORY: combatant_status_definition__national_liberation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_combatant_status_definition__national_liberation_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: combatant_status_definition__national_liberation_reading
 *   human_readable: Combatant Status for National Liberation Movements (AP I Article 1(4) Reading)
 *   domain: international_humanitarian_law/armed_conflict
 *
 * SUMMARY:
 *   AP I Article 1(4) extends combatant status and POW protections to members
 *   of non-state armed groups fighting against colonial, racist, or
 *   occupation regimes if they meet four organizational criteria: (1) belong
 *   to an organized armed group, (2) under responsible command, (3) carry
 *   arms openly, (4) wear fixed insignia. This constraint instantiates the
 *   'national liberation reading' of combatant status — the claim that
 *   non-state actors CAN achieve combatant immunity if their struggle
 *   qualifies as anti-colonial/anti-racist AND they meet the organizational
 *   gate. This reading contests two sibling framings: the 'state-centric
 *   reading' (only states can have armed forces; combatant status is a
 *   privilege of state sovereignty) and the 'functional-protection reading'
 *   (combatant status follows from functional behavior — carrying arms
 *   openly, following IHL — regardless of political status or organization).
 *   The national-liberation reading is a historically situated compromise: it
 *   emerged from post-1945 decolonization and represents the claim that
 *   legitimacy of the struggle (against occupation, colonialism, racism)
 *   matters, not just functional compliance. The constraint exhibits Tangled
 *   Rope structure: it provides genuine coordination (mutual protection,
 *   clear battlefield rules) while creating asymmetric extraction (occupying
 *   powers maintain control over the definitional gate and can deny status
 *   through reinterpretation). Extractiveness has risen over the 20-year
 *   observation window (0.42→0.58) as occupying powers have increasingly
 *   redefined organizational criteria to exclude inconvenient movements;
 *   theater has also risen as the formal rule persists while implementation
 *   becomes performative.
 *
 * KEY AGENTS:
 *   - Organized Liberation Movements: Primary beneficiary (organized/constrained) — if they meet AP I criteria, they gain POW immunity and battlefield legitimacy; extraction runs toward them from occupying powers
 *   - Unorganized or Loosely-Organized Resistance Fighters: Primary victim (powerless/trapped) — lack organizational standing; denied combatant immunity; treated as common criminals; face maximum extraction
 *   - Occupying State Authority: Secondary beneficiary/extractor (institutional/arbitrage) — maintains control over definitional criteria; can redefine 'organization' post-hoc to deny status; extracts through institutional capture of the constraint
 *   - Occupying Military Forces: Secondary beneficiary (institutional/arbitrage) — combatant status clarifies legitimate targets and requires adversary compliance with IHL; experiences coordination benefit
 *   - International Humanitarian Law Regime: Neutral institutional actor (institutional/analytical) — maintains the rule framework; sees genuine coordination function; constrained by inability to enforce against powerful occupying states
 *   - Anti-Colonial Coalition: Organized agent (organized/mobile) — sees AP I as temporary support with sunset logic; builds toward structural transformation where status disputes dissolve
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing state-centric sovereignty framing as immutable legal necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, 0.58).
domain_priors:suppression_score(combatant_status_definition__national_liberation_reading, 0.68).
domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(combatant_status_definition__national_liberation_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(combatant_status_definition__national_liberation_reading, tangled_rope).
narrative_ontology:human_readable(combatant_status_definition__national_liberation_reading, "Combatant Status for National Liberation Movements (AP I Article 1(4) Reading)").
narrative_ontology:topic_domain(combatant_status_definition__national_liberation_reading, "international_humanitarian_law/armed_conflict").

domain_priors:requires_active_enforcement(combatant_status_definition__national_liberation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(combatant_status_definition__national_liberation_reading, '7b53f3b1-fb5d-4126-a279-69a9c5260f46').
narrative_ontology:cs_kernel_codification('7b53f3b1-fb5d-4126-a279-69a9c5260f46', formalized).
narrative_ontology:cs_authority_grounding('7b53f3b1-fb5d-4126-a279-69a9c5260f46', lineage).
narrative_ontology:cs_interpretation_layer_present('7b53f3b1-fb5d-4126-a279-69a9c5260f46').
narrative_ontology:cs_reading_relation('7b53f3b1-fb5d-4126-a279-69a9c5260f46', combatant_status_definition__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b53f3b1-fb5d-4126-a279-69a9c5260f46', combatant_status_definition__functional_protection_reading, influences).
narrative_ontology:cs_axiom('7b53f3b1-fb5d-4126-a279-69a9c5260f46', foundational, legitimacy_of_struggle_determinative).
narrative_ontology:cs_axiom_status(legitimacy_of_struggle_determinative, holdable).
narrative_ontology:cs_axiom_grounding('7b53f3b1-fb5d-4126-a279-69a9c5260f46', legitimacy_of_struggle_determinative, deontological).
narrative_ontology:cs_axiom('7b53f3b1-fb5d-4126-a279-69a9c5260f46', foundational, organizational_criteria_gate).
narrative_ontology:cs_axiom_status(organizational_criteria_gate, holdable).
narrative_ontology:cs_axiom_grounding('7b53f3b1-fb5d-4126-a279-69a9c5260f46', organizational_criteria_gate, instrumental).
narrative_ontology:cs_reference_frame('7b53f3b1-fb5d-4126-a279-69a9c5260f46', anti_colonial_entitlement_framework).
narrative_ontology:cs_drift_state('7b53f3b1-fb5d-4126-a279-69a9c5260f46', contemporary_occupation_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7b53f3b1-fb5d-4126-a279-69a9c5260f46', '').
narrative_ontology:cs_kernel_id(combatant_status_definition__national_liberation_reading, combatant_status_definition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, organized_liberation_movements).
narrative_ontology:constraint_beneficiary(combatant_status_definition__national_liberation_reading, resistance_fighters_meeting_criteria).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_military_forces).
narrative_ontology:constraint_victim(combatant_status_definition__national_liberation_reading, occupying_state_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNORGANIZED RESISTANCE FIGHTER (SNARE) — Cannot meet AP I Article 1(4) organizational and command-control criteria; faces maximum extraction: captured fighters are treated as criminals rather than POWs, denied combatant immunity, subject to trial and execution. Zero exit capacity; maximum suppression. The constraint extracts combatant status from this agent group.
constraint_indexing:constraint_classification(combatant_status_definition__national_liberation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ORGANIZED LIBERATION MOVEMENT (TANGLED ROPE) — Meets AP I Article 1(4) criteria: organized command structure, responsible armed wing, carries arms openly, wears fixed insignia, conducts operations in compliance with laws of war. Genuine coordination function: the constraint codifies mutual protection (combatants get POW status if captured; both sides subject to same protections). Asymmetric extraction: occupying power maintains superior enforcement capacity and can redefine 'organization' to exclude inconvenient movements; movement bears higher verification burden. Mixed benefit: immunity from trial as criminals, but constrained by obligation to follow IHL, which may limit tactics.
constraint_indexing:constraint_classification(combatant_status_definition__national_liberation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IHL REGIME / PROTECTIVE FUNCTION (ROPE) — Pure coordination function: AP I Article 1(4) exists to enable rational battlefield organization. Both parties benefit from clear combatant status rules (reduces civilian targeting, enables prisoner exchanges, reduces war crimes incentives). Institutional actor with arbitrage capacity (can shift between strict and permissive interpretations); experiences the constraint as coordination with minimal extraction. The regime sees this as a well-functioning rule.
constraint_indexing:constraint_classification(combatant_status_definition__national_liberation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANTI-COLONIAL COALITION (SCAFFOLD) — Sees AP I Article 1(4) as a temporary support structure enabling national liberation struggles with a sunset: as colonial/racist regimes are delegitimized and international recognition grows, the need to prove 'organized combatant status' diminishes (Namibia, Palestine trajectory). The constraint has a real sunset — once the occupation ends or legitimacy shifts, the status dispute dissolves. Theater is low because the organizational criteria are concrete and verifiable, not performative. Coalition experiences moderate extraction but sees a path to structural transformation.
constraint_indexing:constraint_classification(combatant_status_definition__national_liberation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OCCUPYING STATE AUTHORITY (PITON) — The constraint persists through institutional inertia despite functional degradation. Many occupying powers formally accept AP I but redefine 'organization' and 'command control' to exclude inconvenient movements (Israel/Palestinian factions, India/Kashmir, Myanmar/ethnic armies). The written rule persists; the institutional practice hollows it out. Theater is high: formal recognition of combatant status occurs rarely despite meeting criteria; the constraint becomes a performative ritual of denial rather than a functioning classification. The occupying state sees its own institutional process as largely theater but maintains it because wholesale rejection would isolate it diplomatically.
constraint_indexing:constraint_classification(combatant_status_definition__national_liberation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, this classification appears as a natural necessity: states are and must be the primary subjects of international law; non-state actors cannot have combatant status by definition because only states have the legal capacity to wage lawful war. Combatant status is inherent to state sovereignty and cannot be extended to non-state groups — this appears as a logical law of the international system. However, this perspective naturalizes what is actually a contested reading of AP I. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(combatant_status_definition__national_liberation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(combatant_status_definition__national_liberation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(combatant_status_definition__national_liberation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(combatant_status_definition__national_liberation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(combatant_status_definition__national_liberation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(combatant_status_definition__national_liberation_reading, TR),
    TR >= 0.70.

:- end_tests(combatant_status_definition__national_liberation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint benefits organized movements that meet criteria (genuine POW immunity) but creates a high threshold that benefits occupying powers (who control the definitional gate). The asymmetry is significant: occupying powers can redefine 'organization' and 'responsible command' to exclude inconvenient movements. The 20-year trajectory shows rising extractiveness (0.42→0.58) as reinterpretation has become systematic. Suppression (0.68): High. Multiple barriers prevent resistance fighters from claiming status: practical difficulty of maintaining formal organization under active suppression, occupying-power definitional capture, weak enforcement mechanisms against denial, lack of international adjudicatory body that can override occupying-power classifications. Unorganized fighters face near-total suppression — zero pathway to combatant immunity. Theater ratio (0.45): Moderate. The organizational criteria (open arms, fixed insignia, responsible command, combatants distinction) are concrete and verifiable — not purely performative. However, theater has risen (0.30→0.45) as occupying powers increasingly deny recognition despite apparent compliance. The rise indicates that the formal rule increasingly operates as theater: movements that meet criteria are denied recognition through definitional moves, and the rule persists as a formal commitment without functional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Organized movements meeting AP I criteria see Tangled Rope (genuine protection + enforced labor) or even Rope (pure coordination). Unorganized fighters see Snare (maximum extraction, zero exit). Occupying powers see Rope (useful rule that clarifies targeting and enforces IHL compliance on adversary). The anti-colonial coalition sees Scaffold (temporary, with sunset). The IHL regime sees Rope (functional coordination). But the piton perspective reveals that the formal rule is increasingly theater: occupying powers deny recognition through redefinition despite formal acceptance. The analytical mountain perspective naturalizes state sovereignty and treats non-state combatant status as logically impossible — but AP I contradicts this, making the mountain a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by whether the agent is a beneficiary or victim of the constraint's extractive asymmetry. Organized movements that meet criteria are net beneficiaries (low d, experiencing the constraint as coordination + immunity); unorganized fighters are net victims (high d, experiencing maximum extraction). The occupying power benefits from controlling the definitional gate (low d, arbitrage capacity to reinterpret). The IHL regime sees coordination function but lacks enforcement capacity against powerful states (moderate d, constrained). The anti-colonial coalition experiences the constraint as temporary (low effective d because they see a sunset path). The piton perspective reflects institutional inertia: the occupying state sees its own process as degraded theater but maintains it for diplomatic cover. The mountain perspective risks naturalizing state sovereignty as immutable law, which the engine's false-summit detector will identify as committer capture.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the kernel reading frame: the national-liberation reading is one legitimate interpretation of AP I Article 1(4), not 'the' correct answer. The state-centric reading rejects non-state combatant status categorically; the functional-protection reading focuses on actual behavior rather than political status; the national-liberation reading ties status to the legitimacy of the struggle. All three remain live interpretations in current international practice. The mandatrophy dissolves when we recognize that AP I is a contested kernel with multiple valid readings, each with different extraction profiles. The engine's job is to model the constraint under this reading while tracking how sibling readings would change the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    organizational_criteria_definition,
    'What constitutes ''organized'' and ''under responsible command'' for AP I Article 1(4)? Where is the threshold between militia, insurgency, and combatant-status-bearing liberation movement?',
    'Comparative legal analysis of how different parties adjudicate organizational criteria; empirical documentation of which movements have been recognized as meeting criteria vs. denied recognition despite similar structural features; longitudinal study of criteria application across occupations (Israeli, Indian, Myanmar, others).',
    'If criteria are strict and universally applied: many resistance movements fail the gate, extractiveness remains high. If criteria are flexible and good-faith applied: extractiveness drops, movements can achieve combatant immunity. If criteria are manipulable: occupying power extracts by definitional control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(organizational_criteria_definition, empirical, 'Definition and adjudication of organizational and command-control criteria').

omega_variable(
    national_liberation_scope_boundary,
    'What qualifies as a ''national liberation struggle'' or ''struggle against colonial or racist regimes''? Does AP I Article 1(4) apply only to anti-colonial wars, or also to struggles against occupation, apartheid, or other forms of domination?',
    'Historical mapping of which conflicts ICJ and ICRC have recognized as falling under AP I Article 1(4); comparative analysis of occupying-power vs liberation-movement vs neutral-third-party framings; longitudinal documentation of whether the scope expands or contracts as international norms shift.',
    'If scope is narrow (only decolonization proper, e.g., 1960s Africa): AP I application drops; extractiveness remains high. If scope is expansive (occupation, apartheid, racist domination): AP I application broadens; more movements claim combatant status. Scope definition determines how many movements fall within the reading''s protective gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(national_liberation_scope_boundary, conceptual, 'Scope of ''national liberation'' and applicability triggers for AP I Article 1(4)').

omega_variable(
    occupying_power_redefinition_vulnerability,
    'Can occupying powers credibly redefine organizational criteria post-hoc to exclude inconvenient movements? Does AP I contain enforcement mechanisms against systematic denial of combatant status to movements meeting criteria?',
    'Review of ICRC adjudications and ICJ rulings on disputes over status recognition; longitudinal case study of whether formal denial by occupying powers affects combatant immunity in practice; analysis of whether IHL enforcement mechanisms (sanctions, war crimes prosecutions) penalize occupying powers for systematic misclassification.',
    'If occupying powers can safely redefine criteria: the constraint is structurally captured; extracted extractiveness drops only on paper while implementation remains extractive. If AP I has real enforcement teeth: redefinition risks sanctions; occupying power faces genuine constraint. Enforcement vulnerability determines whether extractiveness is real or theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupying_power_redefinition_vulnerability, empirical, 'Occupying-power definitional capture and enforcement vulnerability of AP I Article 1(4)').

omega_variable(
    reading_committer_ambiguity,
    'This constraint is one reading of the contested kernel ''combatant_status_definition'': the national-liberation reading vs the state-centric reading vs the functional-protection reading. Do these readings coexist in current international practice, or is one foreclosing the others?',
    'Comparative legal analysis of how different state actors, ICRC, and ICJ apply AP I Article 1(4) in contemporary conflicts; documentation of whether all three readings remain ''live'' positions or whether one has achieved institutional dominance; historical analysis of whether the readings'' relative institutional strength has shifted over the 50-year life of AP I.',
    'If readings coexist: each applies in different conflict theaters and institutional contexts; extractiveness varies by context. If one reading is foreclosing others: international law is stabilizing toward one interpretation; extractiveness trajectory is determined by which reading wins. The mandatrophy for the kernel depends on whether the readings are genuinely coexistent or locked in a struggle for dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Committer ambiguity: coexistence vs foreclosure among three AP I Article 1(4) readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(combatant_status_definition__national_liberation_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(combnat_tr_t0, combatant_status_definition__national_liberation_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(combnat_tr_t10, combatant_status_definition__national_liberation_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(combnat_tr_t20, combatant_status_definition__national_liberation_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(combnat_be_t0, combatant_status_definition__national_liberation_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(combnat_be_t10, combatant_status_definition__national_liberation_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(combnat_be_t20, combatant_status_definition__national_liberation_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(combnat_su_t0, combatant_status_definition__national_liberation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(combnat_su_t10, combatant_status_definition__national_liberation_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(combnat_su_t20, combatant_status_definition__national_liberation_reading, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(combatant_status_definition__national_liberation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__state_centric_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, combatant_status_definition__functional_protection_reading).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, civilian_protection_targeting_rules).
narrative_ontology:affects_constraint(combatant_status_definition__national_liberation_reading, prisoner_of_war_treatment_obligations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the combatant_status_definition kernel. The state_centric_reading and functional_protection_reading are sibling constraints with different epsilon values reflecting their different positions on the legitimacy criterion. All three should be modeled as separate stories linked by network.affects_constraints to enable comparison of how the readings' epistemic differences produce different classification outcomes from the same base IHL text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(combatant_status_definition__national_liberation_reading, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
