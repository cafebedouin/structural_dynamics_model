% ============================================================================
% CONSTRAINT STORY: canadian_transfer_payment_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canadian_transfer_payment_capture, []).

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
 *   constraint_id: canadian_transfer_payment_capture
 *   human_readable: Canadian Transfer Payment Capture by Provincial Elites
 *   domain: federalism/political_economy
 *
 * SUMMARY:
 *   Canadian transfer payments (Equalization, Canada Health Transfer, Canada
 *   Social Transfer) represent a federal-provincial coordination mechanism
 *   intended to equalize fiscal capacity across provinces and establish
 *   national standards in healthcare, education, and social services.
 *   However, the constraint exhibits classic regulatory capture: provincial
 *   governments have systematically negotiated exemptions from federal
 *   conditions, reinterpreted compliance requirements, and leveraged transfer
 *   dependence to secure federal fiscal support without proportional
 *   compliance. This creates a tangled rope structure: genuine coordination
 *   function (transfers do equalize fiscal capacity and fund national
 *   programs) combined with asymmetric extraction (benefits accrue to
 *   provincial political elites and bureaucracies; costs borne by low-income
 *   recipients and fiscal federalism integrity). The theater ratio (0.68)
 *   reflects that federal enforcement mechanisms have degraded—compliance
 *   reviews, reporting requirements, and inter-governmental negotiations
 *   persist as ritual, but functional enforcement has atrophied. The
 *   constraint exhibits a 23-year deterioration pattern (1990–2013 interval):
 *   extractiveness increased 23 percentage points (0.35→0.58) as provinces
 *   accumulated successful precedents for non-compliance and federal
 *   enforcement capacity diminished through institutional attrition.
 *
 * KEY AGENTS:
 *   - Low-Income Recipients: Primary victim (powerless/trapped) — depends on transfer-funded programs; cannot exit or negotiate terms; bears costs of discretionary provincial allocation
 *   - Provincial Governments: Primary beneficiary (organized/constrained) — controls allocation, captures political credit, generates bureaucratic rents; constrained by federal conditions (formally) but enjoys substantial de facto discretion
 *   - Federal Government: Secondary actor (institutional/arbitrage) — nominally enforces conditions; benefits from coordination function; constrained by political cost of withholding transfers
 *   - Resource-Extraction Industries: Secondary beneficiary (powerful/mobile) — captures favorable regulatory treatment from provinces prioritizing resource revenue over social spending
 *   - Intergovernmental Affairs Bureaucracy: Institutional actor (institutional/arbitrage) — maintains compliance theater; enforcement rituals persist despite functional atrophy
 *   - Fiscal Federalism System: Victim (powerless/trapped) — abstract collective good; cannot organize; bears cost of erosion in conditional transfer integrity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canadian_transfer_payment_capture, 0.58).
domain_priors:suppression_score(canadian_transfer_payment_capture, 0.62).
domain_priors:theater_ratio(canadian_transfer_payment_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canadian_transfer_payment_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(canadian_transfer_payment_capture, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(canadian_transfer_payment_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canadian_transfer_payment_capture, tangled_rope).
narrative_ontology:human_readable(canadian_transfer_payment_capture, "Canadian Transfer Payment Capture by Provincial Elites").
narrative_ontology:topic_domain(canadian_transfer_payment_capture, "federalism/political_economy").

domain_priors:requires_active_enforcement(canadian_transfer_payment_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canadian_transfer_payment_capture, provincial_government_bureaucracies).
narrative_ontology:constraint_beneficiary(canadian_transfer_payment_capture, resource_extraction_industries).
narrative_ontology:constraint_beneficiary(canadian_transfer_payment_capture, provincial_political_elites).
narrative_ontology:constraint_victim(canadian_transfer_payment_capture, low_income_recipients).
narrative_ontology:constraint_victim(canadian_transfer_payment_capture, fiscal_federalism_integrity).
narrative_ontology:constraint_victim(canadian_transfer_payment_capture, interprovincial_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RECIPIENT (SNARE) — Trapped by material dependency on transfer-funded programs (welfare, healthcare, education). Provincial governments control allocation and eligibility; federal conditions are suppressed through regulatory flexibility and discretion. The recipient has no exit: cannot relocate without severing access, cannot negotiate terms, cannot organize effectively. Maximum extraction: benefits flow to provincial bureaucracies and political elites; costs borne entirely by the powerless.
constraint_indexing:constraint_classification(canadian_transfer_payment_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL GOVERNMENT (ROPE) — Experiences the transfer system as coordination: establishing national standards, equalizing fiscal capacity across provinces, addressing inter-provincial spillovers. Federal actors can exit via conditional grant enforcement or funding reallocation; they benefit from the legitimacy of a functioning transfer system. Net beneficiary of coordination function: federal government establishes national policy consensus and distributes fiscal burden.
constraint_indexing:constraint_classification(canadian_transfer_payment_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PROVINCIAL GOVERNMENT (TANGLED ROPE) — Operates at the nexus of coordination and extraction. Coordinates delivery of programs funded by federal transfers; extracts political credit and bureaucratic rents by controlling allocation, creating discretionary programs, and directing resources to favored constituencies. Constrained by federal conditions (formally) but enjoys substantial de facto discretion through regulatory interpretation and compliance theater. Benefits from coordination (receives steady funding, no need to raise equivalent provincial taxation); bears costs of coordination (must administer programs, justify spending to federal oversight). Asymmetric extraction toward political elites and bureaucratic agencies.
constraint_indexing:constraint_classification(canadian_transfer_payment_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: INTERGOVERNMENTAL AFFAIRS BUREAUCRACY (PITON) — The enforcement mechanism for transfer conditions has degraded substantially. Conditional grants nominally impose national standards on healthcare, education, and social services; in practice, provinces routinely negotiate exemptions, reinterpret conditions, or accept financial penalties rather than comply. The bureaucratic apparatus (federal and provincial) continues performing oversight, reporting, and negotiation rituals, but functional verification of condition compliance has atrophied. Theater ratio = 0.68: compliance reviews, reporting requirements, and federal-provincial negotiation consume resources with limited actual enforcement outcome.
constraint_indexing:constraint_classification(canadian_transfer_payment_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RESOURCE-EXTRACTION INDUSTRY (TANGLED ROPE) — Benefits from provincial governments that prioritize resource revenue over transfer-funded social programs; coordinates with provincial elites to ensure favorable regulatory treatment and minimal environmental enforcement. Mobile (can relocate operations) but constrained by provincial regulatory capture: extraction depends on provincial political stability and continued subsidization of provincial budgets via transfer diversion. Experiences the constraint as coordination (stable regulatory environment) with embedded extraction (captured regulatory process).
constraint_indexing:constraint_classification(canadian_transfer_payment_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FEDERALISM VIEW (MOUNTAIN) — From a civilizational perspective, federalism creates inherent coordination costs and monitoring gaps. Multiple jurisdictions with overlapping authority always face collective action problems and monitoring failures. This perspective naturalizes the transfer payment capture as inevitable — an irreducible feature of federalism itself. However, structural data reveals this as a false summit: the capture mechanism depends on contingent political choices (non-enforcement, discretionary interpretation, resource dependence) rather than inevitable coordination limits.
constraint_indexing:constraint_classification(canadian_transfer_payment_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canadian_transfer_payment_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canadian_transfer_payment_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canadian_transfer_payment_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canadian_transfer_payment_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(canadian_transfer_payment_capture, TR),
    TR >= 0.70.

:- end_tests(canadian_transfer_payment_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Provincial governments capture approximately 40–60% of transfer benefits through discretionary allocation, regulatory capture, and redirection toward politically favored constituencies. Low-income recipients receive nominal benefits but on terms dictated by provincial agents with incentives misaligned from recipient welfare. The value is not higher (e.g., 0.72) because federal transfers do genuinely fund social programs and equalize provincial fiscal capacity; the extraction is embedded within a functioning coordination mechanism, not pure rent-seeking. Suppression (0.62): Moderate-high. Low-income recipients face material barriers to alternative service access (geographic immobility, income dependency, policy opacity), knowledge barriers (complexity of provincial eligibility interpretation), and organizational barriers (low political power). Federal enforcement is nominally a constraint but has degraded through successive waivers and negotiated exemptions. Theater ratio (0.68): Moderately high. Federal-provincial compliance negotiations, audits, and reporting requirements consume resources but rarely produce enforced policy change. Provinces routinely negotiate around conditions; federal government routinely accepts non-compliance rather than incur political cost of withholding transfers. The apparatus persists through institutional inertia and mutual interest in maintaining federal-provincial cooperative veneer.
 *
 * PERSPECTIVAL GAP:
 *   The federal government and provincial governments perceive structurally different constraints. The federal perspective is rope (coordination mechanism establishing national standards and fiscal equalization). The provincial perspective is tangled rope (genuine coordination with embedded extraction opportunity). The low-income recipient perspective is snare (trapped dependency on discretionary provincial allocation). The federal bureaucracy perspective is piton (degraded enforcement ritual). The resource industry perspective is tangled rope (captures regulatory benefit from provincial resource prioritization). The civilizational analytical perspective risks misidentifying the constraint as a natural federalism cost (mountain/false summit) when the capture mechanism is contingent on non-enforcement choices and resource dependence dynamics. The perspectival gaps reveal that the constraint's classification depends critically on whether one measures from the beneficiary position (rope/tangled rope) or the victim position (snare) or the institutional ritual position (piton).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows the beneficiary/victim and exit-option structure. Low-income recipients (victims + trapped exit) derive high d (≈0.92), experiencing maximum effective extraction. Provincial governments (beneficiaries + constrained exit) derive low-to-moderate d (≈0.25), experiencing positive or neutral effective extraction (rope experience). Federal government (nominal enforcement + arbitrage exit) derives low d (≈0.10), experiencing negative effective extraction (coordination benefit). Resource industries (beneficiaries + mobile exit) derive low d (≈0.18), experiencing modest positive extraction (capture benefit without full trap). The intergovernmental bureaucracy (institutional maintenance + arbitrage exit) derives canonical institutional d (≈0.00), experiencing negative effective extraction (organizational benefit from ongoing system). Directionality ranges from 0.10–0.92 across perspectives, producing varied χ values and perspectival classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clearly separating the coordination function (equalization of fiscal capacity, establishment of national standards) from the extraction mechanism (provincial discretionary capture of program benefits). The tangled rope classification is stable because: (1) genuine coordination benefits exist (transfers do fund programs and equalize capacity); (2) asymmetric extraction is measured and structural (provincial elites capture disproportionate benefits); (3) active enforcement is nominally present but degraded (federal conditions are stated but not enforced). The snare classification from the low-income recipient perspective is equally stable because the recipient's exit is genuinely blocked by material and organizational barriers. The piton classification reflects institutional degradation: the federal enforcement apparatus persists through ritual rather than functional constraint. The false summit (mountain) from the civilizational perspective is revealed by structural data showing contingent capture dynamics rather than inevitable federalism costs. The mandatrophy is resolved by recognizing that the constraint is simultaneously a coordination mechanism (rope), an extraction system (snare), a degraded ritual (piton), and a hybrid hybrid (tangled rope)—depending on the structural position from which it is observed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    provincial_discretion_boundary,
    'Where is the boundary between legitimate provincial discretion in program delivery and regulatory capture that subverts federal conditions?',
    'Comparative analysis of provincial enforcement variation; identification of provinces maintaining compliance vs. those systematically negotiating exemptions; correlation between transfer dependence and compliance rates',
    'If boundary favors discretion: many ''captures'' are legitimate delegation (reduces snare classification). If boundary favors compliance: widespread non-enforcement reveals systematic capture (strengthens snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_discretion_boundary, conceptual, 'Legitimacy boundary between provincial discretion and federal condition subversion').

omega_variable(
    transfer_dependence_leverage,
    'Does federal leverage over transfer-dependent provinces increase or decrease with fiscal strain and interprovincial inequality?',
    'Time-series analysis of federal enforcement actions vs. provincial fiscal capacity; correlation between transfer dependence ratio and federal concessions on conditions; historical cases of federal enforcement vs. negotiated waivers',
    'If leverage increases: federal government can restore conditional enforcement (rope perspective strengthens). If leverage decreases: fiscal crises empower provincial capture through threat credibility (snare and tangled rope perspectives deepen).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transfer_dependence_leverage, empirical, 'Directional relationship between transfer dependence and federal enforcement capacity').

omega_variable(
    recipient_substitution_possibility,
    'Can low-income recipients substitute provincial programs for private alternatives or inter-provincial mobility in response to capture, or is trap immutability structural?',
    'Analysis of inter-provincial migration patterns for low-income recipients; cost comparison of public vs. private alternatives; elasticity of program participation to provincial discretionary changes',
    'If substitution possible: exit is more available than trapped classification suggests (reclassify toward constrained). If substitution impossible: trap is structural (confirms snare/mountain reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recipient_substitution_possibility, empirical, 'Availability of exit alternatives for transfer recipients').

omega_variable(
    compliance_theater_measurement,
    'How much of federal oversight (audits, reporting, negotiation) is performative ritual vs. functionally constraining provincial behavior?',
    'Process tracing of federal-provincial negotiation cases; analysis of audit findings vs. actual policy change; comparison of stated compliance conditions vs. implemented provincial programs',
    'If mostly theater: piton classification confirmed (institutional maintenance of degraded enforcement ritual). If meaningful constraint: tangled rope classification strengthened (federal enforcement is real but negotiable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_theater_measurement, empirical, 'Proportion of federal oversight that is performative vs. functionally constraining').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canadian_transfer_payment_capture, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ctpc_tr_t0, canadian_transfer_payment_capture, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ctpc_tr_t15, canadian_transfer_payment_capture, theater_ratio, 15, 0.62).
narrative_ontology:measurement(ctpc_tr_t30, canadian_transfer_payment_capture, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(ctpc_be_t0, canadian_transfer_payment_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ctpc_be_t15, canadian_transfer_payment_capture, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ctpc_be_t30, canadian_transfer_payment_capture, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canadian_transfer_payment_capture, resource_allocation).
narrative_ontology:affects_constraint(canadian_transfer_payment_capture, canadian_provincial_healthcare_capture).
narrative_ontology:affects_constraint(canadian_transfer_payment_capture, canadian_equalization_formula_rigidity).

% DUAL FORMULATION NOTE:
% Canadian transfer payment capture has two structurally distinct sub-constraints: (1) the nominal coordination mechanism (transfers equalize fiscal capacity across provinces), which is mountain-like in its functional necessity; (2) the provincial capture mechanism (discretionary allocation and non-enforcement of federal conditions), which is tangled rope/snare depending on observation position. This story addresses the capture mechanism. The coordination mechanism upstream is treated as a rope constraint with minimal theater. The downstream constraints (provincial healthcare capture, equalization formula degradation) are affected by the capture dynamics described here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(canadian_transfer_payment_capture, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
