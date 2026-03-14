% ============================================================================
% CONSTRAINT STORY: patronage_collapse_hellenistic_institutions
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_patronage_collapse_hellenistic_institutions, []).

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
 *   constraint_id: patronage_collapse_hellenistic_institutions
 *   human_readable: Patronage Collapse in Hellenistic Institutions
 *   domain: political_economy/institutional_collapse
 *
 * SUMMARY:
 *   The collapse of patronage networks in Hellenistic institutions
 *   (approximately 323-30 BCE) exemplifies a structural constraint that
 *   transitions from pure coordination (Rope) through hybrid
 *   extraction-coordination (Tangled Rope) to degraded ritual (Piton) and
 *   finally institutional collapse. During the early Hellenistic period
 *   following Alexander's conquests, wealthy individuals (landowners,
 *   military officers, merchant princes) funded public infrastructure,
 *   scholarly communities, and civic institutions through voluntary but
 *   socially expected patronage. This system coordinated essential functions:
 *   it provided subsistence for scholars, artists, and civic workers; funded
 *   public goods (theaters, libraries, gymnasia); and conferred social
 *   prestige and political legitimacy on patrons. However, the system relied
 *   on continuous external tribute and territorial expansion. As external
 *   revenue sources dried up (end of conquest cycles, political
 *   fragmentation, Roman expansion), patrons faced three choices: (1)
 *   maintain patronage at reduced levels, (2) withdraw patronage entirely, or
 *   (3) substitute toward private accumulation. The dataset shows
 *   extractiveness rising from 0.35 to 0.62 over the interval, while theater
 *   ratio (performative vs functional content) rises from 0.42 to 0.71. This
 *   pattern indicates the system evolved into pure display — civic ceremonies
 *   and public honors for patrons increased while actual public benefit
 *   declined. By the late Hellenistic and early Roman Imperial period,
 *   patronage had become a degraded ritual: patrons maintained the formal
 *   structure (funding and public visibility) while reducing actual public
 *   goods delivery.
 *
 * KEY AGENTS:
 *   - Wealthy Patron Class: Primary beneficiary (institutional/arbitrage) — captures social prestige, political legitimacy, and honor through patronage; can exit by redirecting capital to private accumulation
 *   - Dependent Scholars and Artists: Primary victim (powerless/trapped) — economically dependent on patronage with no alternative employment or income sources; loss of patronage means destitution
 *   - Professional Civic Workforce: Secondary victim (moderate/constrained) — employed in civic institutions, schools, theaters; face high barriers to relocation or alternative employment but can potentially migrate or adapt
 *   - Civic Institutions: Victims (institutional/constrained) — depend entirely on patron funding for operation; collapse with patron withdrawal unless alternative funding emerges
 *   - Institutional Administrators: Secondary actor (institutional/constrained) — manage resource redistribution and maintain patron relationships; caught between patron expectations and civic workforce needs
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks treating economic limits as immutable laws when collapse is contingent on political and military conditions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(patronage_collapse_hellenistic_institutions, 0.58).
domain_priors:suppression_score(patronage_collapse_hellenistic_institutions, 0.68).
domain_priors:theater_ratio(patronage_collapse_hellenistic_institutions, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(patronage_collapse_hellenistic_institutions, extractiveness, 0.58).
narrative_ontology:constraint_metric(patronage_collapse_hellenistic_institutions, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(patronage_collapse_hellenistic_institutions, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(patronage_collapse_hellenistic_institutions, tangled_rope).
narrative_ontology:human_readable(patronage_collapse_hellenistic_institutions, "Patronage Collapse in Hellenistic Institutions").
narrative_ontology:topic_domain(patronage_collapse_hellenistic_institutions, "political_economy/institutional_collapse").

domain_priors:requires_active_enforcement(patronage_collapse_hellenistic_institutions).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(patronage_collapse_hellenistic_institutions, wealthy_patron_class).
narrative_ontology:constraint_beneficiary(patronage_collapse_hellenistic_institutions, institutional_administrators).
narrative_ontology:constraint_victim(patronage_collapse_hellenistic_institutions, dependent_scholars_artists).
narrative_ontology:constraint_victim(patronage_collapse_hellenistic_institutions, civic_institutions).
narrative_ontology:constraint_victim(patronage_collapse_hellenistic_institutions, professional_workforce).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT SCHOLAR (SNARE) — Structurally trapped by economic dependency on patron patronage; no alternative income sources or career pathways exist. Suppression is extreme: loss of patronage means loss of housing, subsistence, and professional identity simultaneously. Bears full extraction with no exit route.
constraint_indexing:constraint_classification(patronage_collapse_hellenistic_institutions, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PROFESSIONAL WORKFORCE (TANGLED ROPE) — Civic institutions (theaters, schools, libraries) provide genuine coordination benefits (public goods, training, cultural transmission) alongside asymmetric extraction through patron-controlled resource flows. High suppression (career risk, geographic immobility) constrains alternatives but does not eliminate them entirely.
constraint_indexing:constraint_classification(patronage_collapse_hellenistic_institutions, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PATRON CLASS (ROPE) — Experiences patronage as pure coordination: distributing wealth to scholars, artists, and civic institutions solves the problem of maintaining cultural prestige and social stability. Can exit (by withdrawing patronage) without personal cost. Benefits from coordination without bearing suppression costs.
constraint_indexing:constraint_classification(patronage_collapse_hellenistic_institutions, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONAL ADMINISTRATOR (TANGLED ROPE) — Must actively enforce resource redistribution and maintain patron relationships while coordinating public goods delivery. Experiences genuine coordination function (temples, gymnasiums, schools) alongside extraction through mandatory patron contribution networks. Constrained by dependency on both patron capital and civic legitimacy.
constraint_indexing:constraint_classification(patronage_collapse_hellenistic_institutions, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CIVIC RITUAL COMPLEX (PITON) — Public festivals, religious ceremonies, and public works ceremonies maintained through patronage increasingly become performative theater (theatrical display of patron piety and generosity) disconnected from actual public benefit. Theater ratio rises as genuine civic functions are displaced by symbolic display. System persists through institutional inertia — civic identity remains constituted through patronage rituals even as economic substrate deteriorates.
constraint_indexing:constraint_classification(patronage_collapse_hellenistic_institutions, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ECONOMIC LIMIT VIEW (MOUNTAIN) — From civilizational perspective, patronage collapse emerges as natural law: finite patron wealth cannot perpetually grow while dependent populations expand; geometric mismatch between patron capital and institutional infrastructure costs creates inherent system brittleness. However, empirical data reveals this as false summit — patronage systems persisted for centuries with stable protocols; collapse is contingent on specific political-military conditions (loss of territorial tribute, succession wars, economic dislocation), not inherent resource scarcity.
constraint_indexing:constraint_classification(patronage_collapse_hellenistic_institutions, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(patronage_collapse_hellenistic_institutions_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(patronage_collapse_hellenistic_institutions, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(patronage_collapse_hellenistic_institutions, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(patronage_collapse_hellenistic_institutions, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(patronage_collapse_hellenistic_institutions, TR),
    TR >= 0.70.

:- end_tests(patronage_collapse_hellenistic_institutions_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Patronage creates genuine asymmetry: patrons benefit from social prestige and political power while dependents bear economic risk. However, the system does provide real coordination benefits (public goods, training, cultural transmission), preventing classification as pure extraction. The rising trend (0.35→0.62) reflects increasing concentration of patron capital and diminishing public benefit. Suppression (0.68): High. Barriers to exit for dependent scholars and civic workers are severe: no alternative employment markets exist; geographic mobility is low; social identity is fused with patron-dependent roles (identity_locked elements present but not dominant). Suppression is not total because some individuals do successfully transition (to private tutoring, relocation), but the barriers are extreme. Theater ratio (0.64): High-moderate. Civic rituals increasingly become displays of patron piety and public honor (ceremonies, public inscriptions, commemorative structures) disconnected from actual public benefit delivery. As resources contract, theatrical elements (ceremony, ritual, honor) are maintained while substantive functions (library acquisitions, school operations, temple maintenance) decline.
 *
 * PERSPECTIVAL GAP:
 *   The patron class sees patronage as coordination (Rope) — a solution to maintaining order and legitimacy. The dependent scholar sees it as extraction (Snare) — a structurally closed system from which they cannot exit. The administrator sees mixed coordination and extraction (Tangled Rope) — genuine public benefits exist alongside enforced patron relationships. The civic ritual system sees degraded performance (Piton) — ceremonies persist but function has atrophied. The analyst risks naturalizing contingent arrangements (Mountain) — treating economic limits and patron dependency as immutable laws of ancient societies when they are actually institutional arrangements vulnerable to political and military disruption.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (patron class) have arbitrage exit options: they can redirect capital to private accumulation without personal loss. This produces low d (≈0.15) and negative χ — beneficiaries experience the constraint as enabling their interests. Victims (dependent scholars, civic institutions) have trapped or constrained exit options with no viable alternatives, producing high d (≈0.85-0.95) and high χ — they experience maximal extraction. Institutional actors (administrators) experience intermediate d reflecting their mixed position: they benefit from authority and status but are constrained by dependency on both patron capital and civic legitimacy, producing moderate χ (≈0.50-0.65). The rising extractiveness trend reflects increasing concentration of patron capital: as external revenue sources dry up, patrons reduce public investment while maintaining ceremonial display, raising effective extraction for dependent populations.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH PERSPECTIVAL DECOMPOSITION: The mandatrophy is resolved by recognizing that patronage operates as different constraint types from different structural positions. It is genuinely Rope from the patron's perspective (they are solving a coordination problem); genuinely Snare from the trapped scholar's perspective (they experience maximal extraction with no exit); genuinely Tangled Rope from the administrator's perspective (mixed coordination and extraction); and genuinely Piton from the ceremonial system's perspective (performative theater replacing functional coordination). The analytical 'Mountain' perspective is a false summit — the constraint is not an immutable law but a contingent institutional arrangement. The resolution is not finding the 'true' type but recognizing that all six perspectives are structurally accurate readings of the same extractive-coordinative system from different agent positions. The rising theater ratio (0.42→0.71) and rising extractiveness (0.35→0.62) over the interval reveal Goodhart drift: as the patronage system faces resource pressure, performative elements (ritual, honor, ceremony) are maintained while functional elements (actual public goods delivery) are reduced. This drift confirms the piton perspective — the system is degrading into pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patronage_withdrawal_trigger,
    'What specific economic or political condition triggers patron capital withdrawal — loss of external tribute, succession uncertainty, or ideological shift toward private accumulation?',
    'Comparative analysis of patronage collapse across multiple Hellenistic cities and kingdoms; correlation of collapse timing with military defeats, succession crises, and market integration events',
    'If withdrawal is externally triggered (military/political): patronage is contingent institutional arrangement vulnerable to external shocks, not inherent resource limit. If withdrawal reflects ideological shift: patron class is exercising agency (arbitrage exit), confirming rope perspective. If withdrawal reflects resource exhaustion: mountain perspective gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patronage_withdrawal_trigger, empirical, 'Trigger mechanism for patron capital withdrawal').

omega_variable(
    dependent_workforce_substitution,
    'Can dependent scholars and artists substitute to market-based employment or slave-based artisanal production without loss of status or income?',
    'Prosopographic analysis of individual career transitions during collapse periods; tracking of scholar/artist geographic mobility and income diversification; comparative status metrics',
    'If substitution is possible: ''trapped'' exit option overstates structural immobility — reclassify to ''constrained''. If substitution carries severe status penalty: identity_locked mechanism may be operative (self-concept fused with patron-dependent role). If substitution is impossible (no alternative employment): confirms trapped classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dependent_workforce_substitution, empirical, 'Whether dependent workforce can substitute to alternative employment').

omega_variable(
    civic_institution_resilience,
    'Do civic institutions (schools, libraries, theaters) collapse immediately with patron withdrawal or exhibit independent sustainability mechanisms?',
    'Archaeological and documentary evidence of institutional persistence/discontinuity during patronage collapse periods; identification of alternative funding mechanisms (municipal taxes, user fees, peer-to-peer support networks)',
    'If institutions collapse immediately: tangled_rope (coordination function is real but fully dependent on extraction flow). If institutions show independent resilience: coordination function is more robust than extraction mechanism — reclassify closer to rope. If institutions transform into degraded forms (smaller, less public): piton perspective confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_institution_resilience, empirical, 'Resilience of civic institutions during patronage withdrawal').

omega_variable(
    patron_identity_lock,
    'Is patron withdrawal driven by resource constraints or by shift in patron class identity — from public benefactor to private accumulator?',
    'Documentary evidence of patron class rhetoric and behavior; analysis of wills, donation inscriptions, and correspondence; tracking of wealth accumulation vs distribution patterns across generational transitions',
    'If identity-driven: patron class exhibits identity_locked exit option (arbitrage framing may mask identity-fusion to accumulation role). If resource-constrained: arbitrage exit is genuinely available, supporting rope perspective. If mixed: institutional perspective requires directionality override reflecting captured/constrained status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patron_identity_lock, empirical, 'Whether patron withdrawal reflects identity shift or resource constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(patronage_collapse_hellenistic_institutions, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(patr_tr_t0, patronage_collapse_hellenistic_institutions, theater_ratio, 0, 0.42).
narrative_ontology:measurement(patr_tr_t40, patronage_collapse_hellenistic_institutions, theater_ratio, 40, 0.53).
narrative_ontology:measurement(patr_tr_t80, patronage_collapse_hellenistic_institutions, theater_ratio, 80, 0.64).
narrative_ontology:measurement(patr_tr_t100, patronage_collapse_hellenistic_institutions, theater_ratio, 100, 0.71).

% Extraction over time
narrative_ontology:measurement(patr_be_t0, patronage_collapse_hellenistic_institutions, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(patr_be_t40, patronage_collapse_hellenistic_institutions, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(patr_be_t80, patronage_collapse_hellenistic_institutions, base_extractiveness, 80, 0.58).
narrative_ontology:measurement(patr_be_t100, patronage_collapse_hellenistic_institutions, base_extractiveness, 100, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(patronage_collapse_hellenistic_institutions, resource_allocation).
narrative_ontology:affects_constraint(patronage_collapse_hellenistic_institutions, hellenistic_institutional_decline).
narrative_ontology:affects_constraint(patronage_collapse_hellenistic_institutions, transition_to_roman_patronage_system).

% DUAL FORMULATION NOTE:
% Patronage collapse in Hellenistic institutions is downstream of military-political disruption (loss of territorial tribute, succession wars) and upstream of institutional collapse (library closures, school dissolution, temple disrepair). The constraint operates at the institutional level but manifests through individual economic dependency and civic workforce precarity. Separate stories track the patronage system's functional transition (ε=0.35 early period; ε=0.62 late period) and the resulting labor market effects (dependent scholar displacement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(patronage_collapse_hellenistic_institutions, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
