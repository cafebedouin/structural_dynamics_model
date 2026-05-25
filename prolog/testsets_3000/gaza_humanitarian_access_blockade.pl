% ============================================================================
% CONSTRAINT STORY: gaza_humanitarian_access_blockade
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gaza_humanitarian_access_blockade, []).

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
 *   constraint_id: gaza_humanitarian_access_blockade
 *   human_readable: Gaza Humanitarian Access Blockade
 *   domain: geopolitical/humanitarian_access/conflict
 *
 * SUMMARY:
 *   The Gaza humanitarian access blockade represents a constraint where
 *   military occupying powers restrict physical access to essential resources
 *   (food, medicine, fuel, water) and humanitarian services through a
 *   combination of legal restrictions, border control, infrastructure
 *   destruction, and selective aid authorization. The blockade operates
 *   across multiple institutional levels: occupying military command,
 *   neighboring states, international donor states, and humanitarian
 *   organizations. The constraint exhibits the characteristic signature of a
 *   Snare: high extractiveness (0.78), extreme suppression (0.92), and
 *   moderate theater ratio (0.58). The extractiveness reflects that the
 *   blockade's primary function is to extract compliance and concessions from
 *   the civilian population and international actors by leveraging control
 *   over survival resources. The suppression is near-maximal because the
 *   trapped civilian population has zero exit capacity and faces
 *   multi-layered enforcement (military, legal, administrative). The theater
 *   ratio is moderate rather than high because while some humanitarian access
 *   frameworks create performative legitimacy, the underlying blockade
 *   mechanism is functionally real — aid is genuinely restricted and
 *   humanitarian need genuinely unmet. Unlike a pure theater-based piton, the
 *   blockade would collapse immediately if enforcement mechanisms were
 *   removed.
 *
 * KEY AGENTS:
 *   - Civilian Population of Gaza: Primary victim (powerless/trapped) — 2+ million people with no exit capacity; bear full humanitarian costs of blockade through food insecurity, medical deprivation, water contamination, infrastructure collapse
 *   - Humanitarian Organizations: Secondary victims (moderate/constrained) — face suppression through approval delays, route restrictions, cargo confiscation; high costs to continuing operations but higher costs to exit
 *   - Occupying Military Apparatus: Primary beneficiary (institutional/arbitrage) — maintains security control through resource leverage; can adjust enforcement parameters without facing exit costs
 *   - Neighboring States (Israel, Egypt): Institutional beneficiaries (powerful/mobile) — coordinate blockade enforcement; experience both coordination function (security buffer, border control) and extraction (leveraging humanitarian need for political concessions)
 *   - International Donor States: Secondary beneficiaries (institutional/arbitrage) — benefit from predictable security environment and geopolitical leverage; donate humanitarian aid while avoiding direct responsibility for blockade enforcement
 *   - International Civil Society: Tertiary victims (organized/constrained) — advocacy organizations face narrative suppression and access denial; cannot exit (withdrawal abandons population) but experience extraction through constrained effectiveness
 *   - Analytical Observer: Observes risk of false natural law framing (analytical/analytical) — must distinguish between blockade as immutable military necessity vs contingent policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_humanitarian_access_blockade, 0.78).
domain_priors:suppression_score(gaza_humanitarian_access_blockade, 0.92).
domain_priors:theater_ratio(gaza_humanitarian_access_blockade, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_humanitarian_access_blockade, extractiveness, 0.78).
narrative_ontology:constraint_metric(gaza_humanitarian_access_blockade, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(gaza_humanitarian_access_blockade, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gaza_humanitarian_access_blockade, snare).
narrative_ontology:human_readable(gaza_humanitarian_access_blockade, "Gaza Humanitarian Access Blockade").
narrative_ontology:topic_domain(gaza_humanitarian_access_blockade, "geopolitical/humanitarian_access/conflict").

domain_priors:requires_active_enforcement(gaza_humanitarian_access_blockade).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gaza_humanitarian_access_blockade, occupying_military_apparatus).
narrative_ontology:constraint_beneficiary(gaza_humanitarian_access_blockade, blockade_enforcing_states).
narrative_ontology:constraint_victim(gaza_humanitarian_access_blockade, civilian_population_gaza).
narrative_ontology:constraint_victim(gaza_humanitarian_access_blockade, humanitarian_organizations).
narrative_ontology:constraint_victim(gaza_humanitarian_access_blockade, medical_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATION (SNARE) — Completely trapped within geographic boundaries with no exit capacity. Bears full costs of blockade: acute food insecurity, medical supply scarcity, contaminated water access, structural collapse of social services. Maximum experienced extraction with zero degrees of freedom. Suppression is absolute — physical confinement, legal restrictions on movement, military enforcement.
constraint_indexing:constraint_classification(gaza_humanitarian_access_blockade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: HUMANITARIAN ORGANIZATIONS (SNARE) — Face extreme barriers to delivering aid despite nominal access agreements. Suppression mechanisms: approval delays, route restrictions, cargo confiscation, facility damage, security threats. High exit costs — organizations that withdraw lose operational presence and access; those that remain operate under severe constraints with minimal effective aid distribution. Significant asymmetric extraction: their resources flow to managing blockade compliance rather than direct service delivery.
constraint_indexing:constraint_classification(gaza_humanitarian_access_blockade, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NEIGHBORING STATES (TANGLED ROPE) — Experience the blockade as both a coordination mechanism (controlling border flow, managing refugee pressure, security buffer) and an extraction mechanism (leveraging humanitarian need for political concessions, restricting market competition, maintaining geopolitical leverage). Have structural mobility and can adjust enforcement; experience costs and benefits simultaneously. Active enforcement required; extraction is real but paired with coordination function.
constraint_indexing:constraint_classification(gaza_humanitarian_access_blockade, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: INTERNATIONAL DONOR STATES (ROPE) — Primarily benefit from coordination function: blockade maintains predictable security environment, enables geopolitical positioning, extracts minimal cost from donors themselves. Donors negotiate humanitarian access frameworks that align with their strategic interests while maintaining humanitarian reputation. Low effective extraction relative to beneficiaries — they have full exit capacity (can withdraw aid, redirect resources) and experience the constraint as coordination.
constraint_indexing:constraint_classification(gaza_humanitarian_access_blockade, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL CIVIL SOCIETY (SNARE) — Organized agents (NGOs, humanitarian networks, media) face suppression through access denial, narrative control, data blockade. Cannot exit (withdrawal abandons vulnerable population) but constrained in effectiveness through documentation barriers, infrastructure destruction, and systematic obstruction. Experience extraction: their advocacy capacity is severely limited; resources devoted to circumventing blockade rather than addressing root causes.
constraint_indexing:constraint_classification(gaza_humanitarian_access_blockade, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN CLASSIFICATION AT RISK) — From civilizational view, blockades might appear as natural law of conflict: 'military powers always restrict enemy logistics.' However, structural data contradicts natural law signature. The blockade requires continuous active enforcement, political negotiation, and institutional maintenance. It is not an inevitable feature of geopolitical reality but a specific policy choice with measurable costs and alternatives. Engine will flag this as false summit — naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(gaza_humanitarian_access_blockade, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaza_humanitarian_access_blockade_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gaza_humanitarian_access_blockade, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gaza_humanitarian_access_blockade, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gaza_humanitarian_access_blockade, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gaza_humanitarian_access_blockade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The blockade's primary mechanism is extraction — forcing compliance through resource control. The occupying power extracts political concessions, intelligence cooperation, and stability of dominance from the trapped population by withholding food, medicine, and fuel. Extractiveness has risen from 0.55 to 0.78 over the measurement interval as blockade intensity has increased and humanitarian alternatives have been progressively eliminated. Suppression (0.92): Near-maximal. Multiple enforcement layers: (1) Physical confinement — sealed borders with military checkpoints; (2) Legal restrictions — residence permits, travel bans, citizenship categories; (3) Administrative delays — aid approval processes, clearing procedures; (4) Infrastructure destruction — hospitals, water systems, power generation; (5) Resource scarcity — cumulative effect of all above. The trapped population has zero exit capacity and no alternative survival strategies. Theater ratio (0.58): Moderate. Some components are performative: humanitarian access frameworks, periodic aid announcements, international monitoring mechanisms create legitimacy theater. But the underlying resource restriction is functionally real — people actually die from medical deprivation and hunger. Theater has increased from 0.42 to 0.58 as international pressure has required nominal humanitarian frameworks while blockade intensity persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across power levels. The trapped civilian experiences Snare — pure extraction with zero alternatives. The constrained humanitarian organization experiences Snare with more agency — can theoretically exit but faces costs that approach trapped status. The institutional occupier experiences Rope or Tangled Rope — the blockade solves legitimate security coordination problems while benefiting them. Neighboring states experience Tangled Rope — security coordination paired with political extraction (leveraging humanitarian need). Donor states experience Rope — the blockade maintains a predictable regional security environment that protects their interests. International civil society experiences Snare — trapped in advocacy role with constrained effectiveness. The analytical observer risks seeing Mountain (natural law of conflict) but structural data reveals this as false summit — the blockade requires active maintenance and faces realistic alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from agent structural positions. Trapped civilians have d = 0.95 (maximum target status) — they experience the full force of f(d) ≈ 1.42 producing maximum chi. Constrained humanitarians have d ≈ 0.85 producing f(d) ≈ 1.15 because they can nominally exit but face severe costs. Institutional occupiers have d ≈ 0.10 (beneficiary status with arbitrage exit) producing f(d) ≈ -0.01 — negative effective extraction because the extraction flows toward them. Neighboring states have d ≈ 0.50 (mixed coordination-extraction) producing f(d) ≈ 0.65. Donor states have d ≈ 0.05 (pure beneficiary status) producing f(d) ≈ -0.12. The perspectival gap emerges because the power axis separates these structural positions: the powerless experience maximum extraction; the institutional actors experience coordination benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through seven-perspective analysis: The snare classification is confirmed from the powerless perspective (trapped civilians, trapped civil society) and strongly from the constrained perspective (humanitarian organizations). The tangled rope appears at the regional state level because these actors coordinate security while extracting geopolitical advantage. The rope appears at the international donor level because donors experience pure coordination benefit with minimal personal cost. The false mountain appears at the civilizational analytical level where blockades might seem inevitable but are revealed as contingent policy. The classification variance is not error but perspectival legitimacy — different power positions produce different experienced constraint types. The mandatrophy resolves by showing that Snare is dominant but not universal: institutional actors experience genuine coordination alongside extraction; analytical observers risk naturalizing contingency. This is precisely the DR framework's function — to disambiguate which type is operative from which position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_vs_security_framing,
    'Is the blockade framed as security necessity or humanitarian restriction, and does this framing hide the extraction mechanism?',
    'Comparative analysis of stated security justifications against documented humanitarian impacts; assessment of whether stated security objectives require blockade intensity or whether intensity exceeds security requirements',
    'If framing hides extraction: classification remains snare. If security necessity is genuine: classification shifts toward tangled_rope with legitimate (if tragic) coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_vs_security_framing, conceptual, 'Whether blockade framing obscures underlying extraction').

omega_variable(
    alternative_mechanism_availability,
    'Are there alternative security mechanisms that would achieve stated security objectives with significantly lower humanitarian cost?',
    'Comparative case analysis: blockaded vs non-blockaded conflict zones with similar security contexts; modeling of alternative enforcement mechanisms (targeted restrictions, intelligence-based monitoring, demilitarization protocols)',
    'If alternatives exist: blockade intensity reveals extraction motive beyond security — higher classification as pure snare. If no alternatives: snare classification is justified but with reduced confidence in pure extraction framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_mechanism_availability, empirical, 'Whether alternatives to blockade exist for security objectives').

omega_variable(
    exit_option_empirical_assessment,
    'Can civilians physically exit Gaza through any pathway, and at what structural cost?',
    'Documentation of crossing availability, permit processes, financial requirements, destination capacity constraints; tracking of exit applications and approval rates',
    'If exit is nominally possible but practically impossible: maintains ''trapped'' classification. If significant exit is feasible: downgrades to ''constrained'' and shifts snare toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_empirical_assessment, empirical, 'Whether civilian exit from Gaza is structurally possible').

omega_variable(
    aid_diversion_and_leakage_rates,
    'What proportion of authorized humanitarian aid reaches intended civilian beneficiaries versus diverts to alternative uses?',
    'Independent tracking studies, cross-organizational audits, recipient-side assessment of aid receipt rates, documentation of aid confiscation or rerouting',
    'If diversion exceeds 40%: blockade functions as extraction mechanism even for authorized aid. If diversion < 20%: aid flow is functionally constrained rather than wholly captured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aid_diversion_and_leakage_rates, empirical, 'Proportion of aid reaching intended beneficiaries').

omega_variable(
    temporal_blockade_reversibility,
    'Is the blockade structure designed for permanent control or temporary security response, and what evidence supports either interpretation?',
    'Historical institutional analysis: when was blockade instituted, what were stated endpoints, what infrastructure investments suggest permanence vs temporality, what policy frameworks govern potential lifting',
    'If permanent structure: pure snare. If reversible framework: opens scaffold or tangled_rope interpretation with potential sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_blockade_reversibility, conceptual, 'Whether blockade is structured as permanent or temporary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gaza_humanitarian_access_blockade, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaza_tr_t0, gaza_humanitarian_access_blockade, theater_ratio, 0, 0.42).
narrative_ontology:measurement(gaza_tr_t5, gaza_humanitarian_access_blockade, theater_ratio, 5, 0.51).
narrative_ontology:measurement(gaza_tr_t10, gaza_humanitarian_access_blockade, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(gaza_be_t0, gaza_humanitarian_access_blockade, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gaza_be_t5, gaza_humanitarian_access_blockade, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(gaza_be_t10, gaza_humanitarian_access_blockade, base_extractiveness, 10, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gaza_humanitarian_access_blockade, enforcement_mechanism).
narrative_ontology:affects_constraint(gaza_humanitarian_access_blockade, palestinian_administrative_authority_autonomy).
narrative_ontology:affects_constraint(gaza_humanitarian_access_blockade, regional_arms_supply_chains).
narrative_ontology:affects_constraint(gaza_humanitarian_access_blockade, international_refugee_system).

% DUAL FORMULATION NOTE:
% The Gaza blockade decomposes into structurally distinct constraints: (1) military_siege_mechanism (ε=0.85, pure snare) — direct resource restriction; (2) international_aid_coordination (ε=0.35, tangled rope) — humanitarian frameworks paired with donor leverage; (3) regional_security_architecture (ε=0.50, tangled rope) — security coordination by neighboring states. This story addresses the primary siege mechanism. The international aid coordination and regional security perspectives are downstream constraints benefiting from the siege but maintaining their own structural logic.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gaza_humanitarian_access_blockade, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
