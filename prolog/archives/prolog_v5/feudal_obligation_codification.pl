% ============================================================================
% CONSTRAINT STORY: feudal_obligation_codification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feudal_obligation_codification, []).

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
 *   constraint_id: feudal_obligation_codification
 *   human_readable: Feudal Obligation Codification
 *   domain: political/economic/historical
 *
 * SUMMARY:
 *   Feudal obligation codification represents a structural constraint that
 *   binds peasant populations to land and labor obligations through law,
 *   custom, and internalized identity. From the 9th through 15th centuries,
 *   what began as negotiated reciprocal relationships (protection in exchange
 *   for service) hardened into codified, heritable, asymmetrically enforced
 *   obligations. The constraint exhibits pure extraction characteristics
 *   (high extractiveness, suppression, mandated enforcement) masked by
 *   coordination rhetoric and reciprocal framing. The analytical observer
 *   classifies it as snare; victims experience it as imprisonment;
 *   beneficiaries experience it as legitimate coordination; organized actors
 *   develop alternatives (guilds, town charters) that create sunset pathways.
 *   The constraint's extractiveness rises initially (0.55 to 0.72) as
 *   codification becomes more elaborate and enforcement more systematic, then
 *   begins to decline (0.72 to 0.68) as alternative institutions (markets,
 *   states, guilds) emerge. Theater ratio rises throughout (0.38 to 0.62) as
 *   the functional coordination mechanisms degrade and the rituals become
 *   increasingly performative — late feudalism maintains the forms of
 *   obligation without the original functions.
 *
 * KEY AGENTS:
 *   - Peasant Serf Class: Primary victims (powerless/trapped at immediate, identity_locked at generational time) — experience maximum extraction with no legal exit; obligations are heritable and absolute
 *   - Aristocratic Landowning Class: Primary beneficiary (institutional/arbitrage) — captures surplus labor, military service, and status legitimacy; experiences constraint as reciprocal coordination
 *   - Merchant Intermediaries: Secondary victims (moderate/constrained) — face monopoly restrictions, tariff obligations, service levies; also benefit from codified trade regulations
 *   - Guild Organizations: Organized actors (organized/constrained) — develop alternative coordination mechanisms (apprenticeship, quality standards) that gradually reduce feudal extraction dependence
 *   - Church/Monastic Institutions: Secondary beneficiary and complicating actor — benefit from feudal obligations while also serving as escape route for some peasants; legitimize the system through theological justification
 *   - Town Charter Movements: Organized exit agents (organized/mobile) — establish alternative legal frameworks that free residents from feudal obligation; create sunset pathways
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as verifiable snare, not rope; identifies reciprocity rhetoric as legitimation cover
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feudal_obligation_codification, 0.68).
domain_priors:suppression_score(feudal_obligation_codification, 0.72).
domain_priors:theater_ratio(feudal_obligation_codification, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feudal_obligation_codification, extractiveness, 0.68).
narrative_ontology:constraint_metric(feudal_obligation_codification, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(feudal_obligation_codification, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feudal_obligation_codification, snare).
narrative_ontology:human_readable(feudal_obligation_codification, "Feudal Obligation Codification").
narrative_ontology:topic_domain(feudal_obligation_codification, "political/economic/historical").

domain_priors:requires_active_enforcement(feudal_obligation_codification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feudal_obligation_codification, aristocratic_landowning_class).
narrative_ontology:constraint_victim(feudal_obligation_codification, peasant_serf_class).
narrative_ontology:constraint_victim(feudal_obligation_codification, merchant_intermediaries).
narrative_ontology:constraint_victim(feudal_obligation_codification, independent_craftspeople).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BOUND SERF (SNARE) — The peasant is trapped by law, custom, and geography. Exit is legally prohibited; escape carries severe punishment. Obligations are absolute and inherited. The serf experiences maximum extraction with no alternative pathways. Suppression is structural (legal prohibition + control of subsistence resources).
constraint_indexing:constraint_classification(feudal_obligation_codification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SERF GENERATIONAL IDENTITY (SNARE) — Across generational time, the serf's identity becomes constituted through obligation: 'a peasant is one who owes.' The binding mechanism shifts from purely structural (legal/economic barriers) to cognitive (identity fusion). The serf cannot imagine an alternative social position. The constraint appears not as a law imposed from outside but as the natural order of things. Suppression includes internalized norms.
constraint_indexing:constraint_classification(feudal_obligation_codification, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: MERCHANT INTERMEDIARY (TANGLED ROPE) — Independent merchants face constraints (monopoly on certain trades reserved for nobility, tariff obligations, service levies) but also benefit from the codified system. The obligation structure coordinates resource flows that enable their commerce. They experience genuine coordination alongside asymmetric extraction. Exit is costly (relocation, loss of trading network access) but possible. Suppression is high but surmountable.
constraint_indexing:constraint_classification(feudal_obligation_codification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ARISTOCRATIC LANDOWNING CLASS (ROPE) — From the beneficiary's perspective, feudal obligation is a coordination mechanism solving the problem of extracting surplus and maintaining social hierarchy without constant military coercion. The code of chivalry and obligation creates legitimacy. The landowner experiences the constraint as reciprocal: they owe protection and justice in exchange for obligation. This is genuine coordination from their position. They have exit options (migration to other fiefs, military authority) that reduce experienced extraction. Net beneficiary position.
constraint_indexing:constraint_classification(feudal_obligation_codification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: GUILD ORGANIZATION MOVEMENT (SCAFFOLD) — Organized crafts and merchant guilds gradually develop alternative coordination mechanisms (apprenticeship standards, quality controls, collective bargaining) that reduce dependence on feudal obligation. These are temporary scaffolds with sunset clauses — as they mature, feudal extraction loses force because alternative status pathways emerge. Organized agents see the constraint as a temporary obstacle being bypassed through institutional innovation.
constraint_indexing:constraint_classification(feudal_obligation_codification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: LATE FEUDAL RITUAL SYSTEM (PITON) — By the late medieval period, feudal obligation has become largely performative in many regions. The actual military/economic functions have degraded; the rituals persist through institutional inertia. Homage ceremonies, written obligations, and formal hierarchies continue despite low functional coordination. Theater ratio rises as the real function declines. The system sees itself as degraded — acknowledged even by participants as archaic.
constraint_indexing:constraint_classification(feudal_obligation_codification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational perspective, feudal obligation codification is a pure extraction mechanism disguised as natural social order. High extractiveness (0.68), high suppression (0.72), low theater relative to extraction (0.55). The constraint persists through legal enforcement and identity lock, not through genuine coordination function. Mandatrophy resolved: this is verifiably snare, not rope or mountain.
constraint_indexing:constraint_classification(feudal_obligation_codification, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feudal_obligation_codification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(feudal_obligation_codification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(feudal_obligation_codification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(feudal_obligation_codification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(feudal_obligation_codification, TR),
    TR >= 0.70.

:- end_tests(feudal_obligation_codification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The peasant's obligation to provide labor, service, and tribute with minimal reciprocal protection creates sustained asymmetric extraction. Measurement trajectory shows initial rise (codification increasing extractiveness from 0.55 to 0.72) then slight decline (0.72 to 0.68) as alternatives emerge. High extractiveness is the definition; reciprocal obligations do not lower this because enforcement is asymmetric — peasants bear punishment for breach; lords rarely face equivalent consequences. Suppression (0.72): High. Multiple overlapping mechanisms prevent exit: legal prohibition on peasant mobility, control of subsistence resources (land access conditional on obligation), geographic isolation, armed enforcement, and crucially, internalized identity lock by the generational perspective. Suppression mechanisms include structural (legal/economic) and cognitive (identity fusion). Theater ratio (0.55): Moderate, rising. Early feudalism (t=0) has lower theater because genuine military/economic coordination functions exist; the obligation serves real purposes. By late feudalism (t=300), theater rises (0.62) as military and economic functions have been replaced by markets and states, yet the ceremonial forms persist — homage rituals, written obligations, status hierarchies continue as institutional inertia. Mandatrophy is resolved: this is verifiably snare. The reciprocal framing ('the lord owes protection') is not sufficient to make it rope, because (a) enforcement is asymmetric — breach by peasant is criminal, breach by lord is not, and (b) alternatives exist. By the comparative standard, true coordination mechanisms survive when alternatives emerge; feudal obligation requires suppression to persist. Therefore, snare.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence. From the serf's trapped position, feudal obligation is pure snare — extraction with no exit and no coordination benefit. From the identity_locked generational perspective, it remains snare but with an additional dimension: the binding is internalized, making the constraint appear natural rather than imposed. From the merchant's constrained position, it is tangled rope — genuine coordination (trade rules, commercial protocols) exists alongside extraction (monopoly restrictions, levies). From the aristocratic beneficiary's perspective, it is rope — a coordination mechanism solving the problem of maintaining surplus extraction while providing reciprocal legitimacy. From the guild organization's perspective, it is a scaffold with sunset — a temporary obstacle being replaced by guild coordination mechanisms. From the late feudal ritual system's perspective, it is piton — the functions have atrophied but the ceremonies persist. The analytical observer sees verifiable snare: extractiveness is high, suppression is high, and the constraint persists through enforcement, not through genuine coordination function (which has been replaced by markets and states).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from structural position relative to extraction flow. The peasant is structurally trapped (no legal exit, economic dependency on land access) and is the target of extraction (owes labor/service/tribute), yielding d ≈ 0.95, producing high f(d) ≈ 1.42, resulting in maximum experienced extractiveness χ. At generational time with identity_locked exit, d ≈ 0.89, producing f(d) ≈ 1.28, still very high χ. The identity lock adds perceptual entrenchment to structural entrenchment. The aristocratic beneficiary has institutional power and arbitrage exit options (can migrate to other fiefs, has military authority), yielding d ≈ 0.05, producing f(d) ≈ -0.12, negative χ — they experience the constraint as benefiting them without extraction cost. The merchant has moderate power and constrained exit (relocation is costly but possible), yielding d ≈ 0.50, producing moderate f(d) ≈ 0.65, moderate χ. Guild organizations have organizational power and constrained exit (guild restrictions can be escaped by joining a town charter movement), yielding d ≈ 0.45, producing f(d) ≈ 0.40, lower χ — they see alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by verifying snare classification: (1) Extractiveness 0.68 ≥ 0.46 threshold. (2) Suppression 0.72 ≥ 0.60 threshold. (3) Chi computation: primary victim perspective (powerless/trapped/local) yields d ≈ 0.95, f(d) ≈ 1.42, σ(local) = 0.8, χ ≈ 0.68 × 1.42 × 0.8 ≈ 0.77 ≥ 0.66 threshold. (4) Primary beneficiary perspective (institutional/arbitrage/continental) yields d ≈ 0.05, f(d) ≈ -0.12, σ(continental) = 1.1, χ ≈ 0.68 × (-0.12) × 1.1 ≈ -0.09. Negative chi confirms beneficiary. (5) Constraint persists through enforcement and suppression, not through coordination benefit. All beneficiaries are institutional; all victims are powerless or moderate. No beneficiary at powerless level; no victim at institutional level (guild organizations are organized, not powerless). This asymmetry is diagnostic of snare. Rope would require balanced perspectives where the same agents experience both coordination and extraction benefit; snare has beneficiaries who experience pure benefit and victims who experience pure extraction. Mandatrophy resolved: this is snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocal_obligation_authenticity,
    'Is the reciprocal obligation on the lord (protection, justice) functionally equivalent to the peasant''s obligation (labor, service, tribute)?',
    'Historical analysis of enforcement patterns: frequency of lord prosecution for breach vs peasant punishment; comparison of material benefit flowing to lord vs serf; examination of dispute resolution when both parties claim breach.',
    'If genuinely reciprocal: constraint is Tangled Rope from both positions (coordination + extraction). If asymmetric enforcement: constraint is Snare; reciprocity is cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reciprocal_obligation_authenticity, empirical, 'Whether reciprocal obligation is functionally symmetric or asymmetrically enforced').

omega_variable(
    exit_option_availability,
    'How many serfs actually attempted escape in a given period and what fraction succeeded in establishing alternative social position?',
    'Historical records: flight rates from manorial records, town migration registers, monastic refuge patterns; longitudinal tracking of escapee outcomes; comparison to theoretical population turnover if exit were freely chosen.',
    'If <1% succeed and <5% attempt: trapped classification confirmed. If >10% succeed: exit_options should be reclassified from trapped to constrained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Escape success rate and exit option availability for bound peasants').

omega_variable(
    identity_lock_internalization_mechanism,
    'At what generational depth does peasant identity become fused with obligation status, such that alternative social position becomes unthinkable rather than merely prohibited?',
    'Historical sources: peasant narratives, confessional records, court testimony; linguistic analysis of how peasants describe their own status; transmission of expectations across generations in family documents.',
    'If identity lock emerges by generation 2: suppression becomes internalized and more stable. If never fully internalized: constraint remains dependent on external enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_internalization_mechanism, empirical, 'Generational timeline for identity lock in feudal obligation').

omega_variable(
    alternative_coordination_function_loss,
    'What specific coordination functions did feudal obligation perform that could not be performed by markets, contracts, or other mechanisms once those alternatives emerged?',
    'Historical analysis of what breaks down in early-modern transition away from feudalism; identification of coordination gaps that required new institutions (state revenue systems, labor markets, legal codes); comparison of efficiency metrics before and after transition.',
    'If functions were genuinely irreplaceable: classification as rope becomes more defensible. If easily replaced: classification as snare confirmed; codification was extraction dressed as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_function_loss, conceptual, 'Uniqueness of coordination functions provided by feudal obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feudal_obligation_codification, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feudal_obligation_codification, theater_ratio, 0, 0.38).
narrative_ontology:measurement(feud_tr_t100, feudal_obligation_codification, theater_ratio, 100, 0.45).
narrative_ontology:measurement(feud_tr_t200, feudal_obligation_codification, theater_ratio, 200, 0.55).
narrative_ontology:measurement(feud_tr_t300, feudal_obligation_codification, theater_ratio, 300, 0.62).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feudal_obligation_codification, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(feud_be_t100, feudal_obligation_codification, base_extractiveness, 100, 0.68).
narrative_ontology:measurement(feud_be_t200, feudal_obligation_codification, base_extractiveness, 200, 0.72).
narrative_ontology:measurement(feud_be_t300, feudal_obligation_codification, base_extractiveness, 300, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feudal_obligation_codification, resource_allocation).
narrative_ontology:affects_constraint(feudal_obligation_codification, guild_charter_emergence).
narrative_ontology:affects_constraint(feudal_obligation_codification, peasant_revolt_movements).
narrative_ontology:affects_constraint(feudal_obligation_codification, merchant_class_ascendancy).

% DUAL FORMULATION NOTE:
% Feudal obligation codification is upstream of three constraint families: guild charter emergence (alternative coordination mechanism), peasant revolt movements (organized exit attempt), and merchant class ascendancy (structural replacement of feudal extraction with market mechanisms). Each downstream constraint has higher extractiveness or lower suppression, reflecting the decay of feudal extraction as alternatives mature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feudal_obligation_codification, moderate, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
