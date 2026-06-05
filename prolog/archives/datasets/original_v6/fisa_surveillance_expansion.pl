% ============================================================================
% CONSTRAINT STORY: fisa_surveillance_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_surveillance_expansion, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: fisa_surveillance_expansion
 *   human_readable: FISA Surveillance Expansion Authority
 *   domain: national_security/governance/legal
 *
 * SUMMARY:
 *   The Foreign Intelligence Surveillance Act (FISA) surveillance expansion
 *   represents a core tension in democratic governance: the institutional
 *   coordination need for legal authorization of surveillance versus the
 *   asymmetric extraction this creates for surveilled populations. Enacted in
 *   1978 as a mechanism to constrain ad hoc executive surveillance, FISA
 *   created a statutory framework and FISA court for warrant authorization.
 *   Successive expansions — Section 215 (bulk records collection), Section
 *   702 (foreign-targeting with domestic incidental collection), and periodic
 *   reauthorizations — have transformed FISA from a constraints regime into
 *   an enablement regime. The constraint exhibits complex perspectival
 *   structure: intelligence agencies experience it as pure coordination
 *   (rules replacing arbitrary authority), surveil citizens experience it as
 *   pure extraction (invisible, inescapable), oversight bodies experience it
 *   as mixed coordination-extraction, and the FISA court institution has
 *   degraded from gatekeeping to performative approval. Extractiveness has
 *   increased over time (0.35 → 0.62 across the interval) as scope expanded
 *   and theater has increased (0.52 → 0.68) as reauthorizations moved to
 *   bulk, classified processing replacing individual warrant review.
 *
 * KEY AGENTS:
 *   - Intelligence Agencies (FBI/NSA/CIA): Primary beneficiaries (institutional/arbitrage) — gain statutory authority for expanded collection, retain and expand authorities through reauthorization cycles
 *   - Domestic Populations: Primary victims (powerless/trapped) — no exit from surveillance once enacted, cannot challenge collection through normal legal channels, suppression is near-total through classification and technical inaccessibility
 *   - Congressional Intelligence Committees: Mixed role (organized/constrained) — formal oversight authority but constrained by compartmentalization, classified briefings, and political asymmetry favoring security narratives
 *   - Civil Liberties Organizations: Secondary beneficiaries through mobilization (moderate/constrained) — constrained by litigation barriers and state secrets privilege, but benefit from expansion as rallying issue
 *   - FISA Court Institution: Institutional performer (institutional/arbitrage) — maintains formal gatekeeping appearance while approving >99% of applications, sustained by intelligence agency reliance
 *   - Reform Coalitions: Mobile reformers (organized/mobile) — treat expansion as solvable through transparency, sunset clauses, adversarial proceedings; see scaffold pathway
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_surveillance_expansion, 0.62).
domain_priors:suppression_score(fisa_surveillance_expansion, 0.75).
domain_priors:theater_ratio(fisa_surveillance_expansion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_surveillance_expansion, extractiveness, 0.62).
narrative_ontology:constraint_metric(fisa_surveillance_expansion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fisa_surveillance_expansion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_surveillance_expansion, tangled_rope).
narrative_ontology:human_readable(fisa_surveillance_expansion, "FISA Surveillance Expansion Authority").
narrative_ontology:topic_domain(fisa_surveillance_expansion, "national_security/governance/legal").

domain_priors:requires_active_enforcement(fisa_surveillance_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_surveillance_expansion, intelligence_agencies).
narrative_ontology:constraint_beneficiary(fisa_surveillance_expansion, executive_branch).
narrative_ontology:constraint_beneficiary(fisa_surveillance_expansion, law_enforcement_apparatus).
narrative_ontology:constraint_victim(fisa_surveillance_expansion, domestic_populations).
narrative_ontology:constraint_victim(fisa_surveillance_expansion, civil_liberties_protection).
narrative_ontology:constraint_victim(fisa_surveillance_expansion, democratic_accountability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED CITIZEN (SNARE) — Ordinary persons have no exit from FISA surveillance once enacted. Cannot refuse participation, cannot challenge data collection through normal legal channels (FISA court is non-adversarial), cannot opt out. Bears full extraction cost with zero escape option. Suppression is maximal: the surveillance is invisible, legally classified, and technically inaccessible for independent verification. No alternative coordination pathway exists.
constraint_indexing:constraint_classification(fisa_surveillance_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CIVIL LIBERTIES ADVOCATES (TANGLED ROPE) — Organizations like CAIR, ACLU experience both extraction and coordination function. They are trapped by resource constraints and doctrinal barriers (litigation standing, state secrets privilege) but also benefit from the surveillance expansion itself as a mobilizing issue that enables fundraising, coalition building, and legal precedent-setting. Extraction is real but not absolute — they have constrained exit through litigation and public advocacy, though these cost-bearing exits face institutional barriers. The constraint does coordinate: intelligence agencies and civil society both orient around FISA's rules, even when opposing them.
constraint_indexing:constraint_classification(fisa_surveillance_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTELLIGENCE AGENCIES (ROPE) — FBI, NSA, CIA experience the FISA expansion as pure coordination with benefits. They gain legal authority to expand collection and retain data according to statutory rules rather than ad hoc authority. The constraint solves a real coordination problem: enabling agencies to operate with legislative authorization rather than executive fiat. Agencies have arbitrage exit (can pivot to emergency authorities, can request fresh expansion) and experience net benefit. From their perspective, the theater is low — the FISA court provides the appearance of judicial review, which is what coordinates agencies around statutory authority rather than unilateral presidential action.
constraint_indexing:constraint_classification(fisa_surveillance_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONGRESSIONAL OVERSIGHT (TANGLED ROPE) — Congress has formal authority to expand or restrict FISA but faces significant coordination-extraction hybrid. The genuine coordination function: rules for authorization and disclosure enable legislative oversight in principle. The extraction: real barriers to exercising this authority. Surveillance programs receive only cursory briefing, classified documents are compartmentalized preventing meaningful review, members voting on expansions cannot discuss details with staff or constituents. Congress has constrained exit (can theoretically limit FISA, but faces political costs and information asymmetry) while appearing to coordinate (formal authorization votes). Theater is moderate-high: voting on classified programs they cannot fully understand creates performative democratic accountability.
constraint_indexing:constraint_classification(fisa_surveillance_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSPARENCY/REFORM COALITION (SCAFFOLD) — Civil society coalitions built around declassification, warrant requirements, and transparency provisions treat FISA expansion as a temporary problem with a sunset. They advocate for statutory sunset clauses, mandatory transparency reports, and periodic reauthorization requiring affirmative vote. This perspective sees the constraint as removable through legislative reform over a 5-10 year horizon. The coalitions have mobile exit (can pivot strategies, dissolve and reform around new legislation) and explicit temporal thinking. Theater is moderate: transparency advocates use FISA expansion as a mobilizing issue precisely because the theater value (claimed oversight mechanisms) is incomplete. As transparency and declassification succeed, the expansion's functional justification weakens.
constraint_indexing:constraint_classification(fisa_surveillance_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: FISA COURT INSTITUTION (PITON) — The FISA court maintains a performative function while its actual gatekeeping capacity has degraded. Historically, the court approved ~99% of government applications; recent years show slightly higher denial rates (0.5-1%) on specific applications but near-universal approval for bulk programs and reauthorizations. The court performs judicial review — written opinions, adversarial procedures — but lacks genuine adversarial function (government presents, court approves). The theater persists through institutional inertia: the FISA court continues as the formal oversight mechanism because alternatives haven't fully replaced it, not because it functions as originally conceived. The institution is maintained by arbitrage: intelligence agencies want formal authorization, and the court provides it; the court's institutional survival depends on agencies' continued reliance.
constraint_indexing:constraint_classification(fisa_surveillance_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, FISA expansion represents a genuine coordination-extraction hybrid in democratic governance. The coordination function is real: democratic societies need rules for surveillance authorization that balance security and privacy through legislative and judicial processes. The extraction is real: the expansion increases government power over domestic populations asymmetrically. This perspective classifies as tangled rope, not mountain, because surveillance expansion is contingent institutional choice, not immutable necessity. The engine's classification should reflect that FISA expansion is a tractable political problem, not a law of nature.
constraint_indexing:constraint_classification(fisa_surveillance_expansion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_surveillance_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fisa_surveillance_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fisa_surveillance_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fisa_surveillance_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fisa_surveillance_expansion, TR),
    TR >= 0.70.

:- end_tests(fisa_surveillance_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high, driven by core asymmetry — intelligence agencies gain authority to collect and retain data on domestic populations without individual consent or knowledge, while populations bear maximum cost and bear zero exit option. The 0.62 value reflects that some surveillance is genuinely coordinated (warrants, oversight structures exist) but much occurs in bulk collection modes where individual evaluation is illusory. Theater ratio (0.68): High and increasing. The FISA court performs judicial review (written opinions, formal procedures) but lacks genuine gatekeeping (approval rates ~99%). Bulk reauthorizations receive theatrical review but no individualized examination. The theater has increased as expansion has moved surveillance from targeted warrants to bulk authorities processed in classified batch mode. Suppression (0.75): Very high. Surveillance is invisible to subjects, technical access to underlying data is classified, FISA court proceedings are one-sided (no adversarial representation), and legal challenges are blocked by state secrets privilege and lack of standing. Citizens cannot verify they are surveilled, cannot challenge collection, cannot access data about themselves. The measured extractiveness (0.62) plus suppression (0.75) yields a snare classification from the powerless perspective; moderate perspectives see extraction with some coordination function (tangled rope) because some oversight exists, even if ineffective.
 *
 * PERSPECTIVAL GAP:
 *   Maximum perspectival gap driven by asymmetric information and structural position. Beneficiaries (intel agencies) see the constraint as enabling legal coordination. Victims (surveilled publics) see invisible extraction. Oversight (Congress) sees formal authority they cannot exercise. The court sees performative gatekeeping. Reformers see a removable problem. The analytical observer sees a genuine hybrid. The gap is not measurement error — it is structural. The constraint genuinely produces different reality for each position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows power, exit, and beneficiary/victim structure. Intelligence agencies: institutional power + arbitrage exit + beneficiary status = low d (0.15-0.25) → negative χ component. They experience coordination, not extraction. Surveilled citizens: powerless + trapped + victim status = high d (0.95+) → maximum f(d) ≈ 1.42 → high χ. They experience maximum extraction. Congressional committees: organized power + constrained exit + mixed beneficiary/victim = moderate d (0.45-0.55) → moderate χ. They see real extraction despite formal authority. Civil liberties orgs: moderate power + constrained exit + victim status (advocacy on behalf of publics) = moderate-high d (0.60-0.75) → high χ from their perspective, but beneficiary status through mobilization creates tension. FISA court: institutional power + arbitrage exit (survival depends on agency use) + paradoxical position (appears to control surveillance, actually approves most applications) = low d nominally but theta-capture effect (the court's interests align with expansions) makes directionality analysis complex. The override table captures this through the institution's actual structural relationship (beneficiary through reliance, victim through mission creep).
 *
 * MANDATROPHY ANALYSIS:
 *   FISA expansion resolves mandatrophy through explicit tangled rope classification: the constraint simultaneously coordinates (enables legal surveillance authority replacing ad hoc executive action) and extracts (asymmetric collection affecting domestic populations without consent). The coordination is real: FISA did displace purely presidential surveillance decisions and created statutory rules. The extraction is real: those rules enable bulk collection affecting millions with zero individual notice or consent. Neither classification (pure coordination, pure extraction) is accurate. The analytical observer's perspective confirms: this is a tractable political problem, not a natural law. The classification prevents false mountains (naturalizing surveillance as inherent to security) and false ropes (claiming pure coordination when extraction is clear). The theater (FISA court's performative gatekeeping) is correctly identified as Piton from the institution's own view, indicating degradation from the original constraining function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fisa_court_gatekeeper_capacity,
    'Does the FISA court function as a genuine check on surveillance expansion, or is it institutionally incapable of meaningful denial at scale?',
    'Longitudinal analysis of denial rates across application types; comparison of denied applications to approved applications; analysis of whether reauthorizations receive genuine individual evaluation or bulk approval',
    'If court has capacity: classification shifts toward Rope (pure coordination with judicial review). If court lacks capacity: classification solidifies as Tangled Rope (theatrical review masking expansion).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fisa_court_gatekeeper_capacity, empirical, 'Whether FISA court provides genuine surveillance gatekeeping or performative review').

omega_variable(
    intelligence_necessity_asymmetry,
    'How much of FISA expansion is justified by genuine security coordination needs versus how much is driven by bureaucratic expansion and scope creep?',
    'Comparative analysis of surveillance volume before/after expansion; correlation between expansion and identified threats; audit of actual intelligence production from expanded authorities versus claimed necessity',
    'If security needs dominant: extractiveness should decrease to ~0.45, reclassify toward Rope. If scope creep dominant: extractiveness should increase to ~0.75, reclassify toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligence_necessity_asymmetry, empirical, 'Whether FISA expansion reflects genuine security needs or bureaucratic expansion').

omega_variable(
    democratic_reform_viability,
    'Can democratic institutions meaningfully constrain surveillance expansion, or are the informational and political asymmetries irreversible?',
    'Analysis of successful surveillance restrictions in comparable democracies; tracking of legislative reform proposals through Congress; assessment of public engagement levels and voting patterns on FISA reauthorizations',
    'If constrainable: scaffold perspective is structural (sunset is viable). If irreversible: constraint trends toward Snare as suppression increases and exit capacity decreases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_reform_viability, conceptual, 'Whether democratic mechanisms can effectively constrain surveillance expansion').

omega_variable(
    adversarial_proceedings_necessity,
    'Would addition of genuine adversarial representation in FISA court proceedings (government vs. privacy advocate) fundamentally alter surveillance expansion dynamics?',
    'Comparative study of jurisdictions with adversarial warrant proceedings; analysis of FISA court denial rates under rule changes allowing government to hear opposition; modeling of how adversarial presentation affects application design',
    'If adversarial proceedings substantially increase denials: theater_ratio decreases, chi decreases, classification shifts toward Rope/Scaffold. If minimal effect: theater_ratio persists, Tangled Rope/Piton classifications confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adversarial_proceedings_necessity, empirical, 'Whether adversarial FISA proceedings would reduce surveillance expansion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_surveillance_expansion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_surveillance_expansion, theater_ratio, 0, 0.52).
narrative_ontology:measurement(fisa_tr_t10, fisa_surveillance_expansion, theater_ratio, 10, 0.61).
narrative_ontology:measurement(fisa_tr_t20, fisa_surveillance_expansion, theater_ratio, 20, 0.68).
narrative_ontology:measurement(fisa_tr_t5, fisa_surveillance_expansion, theater_ratio, 5, 0.56).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_surveillance_expansion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fisa_be_t10, fisa_surveillance_expansion, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(fisa_be_t20, fisa_surveillance_expansion, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(fisa_be_t5, fisa_surveillance_expansion, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_surveillance_expansion, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fisa_surveillance_expansion, 0.12).
narrative_ontology:affects_constraint(fisa_surveillance_expansion, state_secrets_privilege_doctrine).
narrative_ontology:affects_constraint(fisa_surveillance_expansion, fourth_amendment_standing_doctrine).
narrative_ontology:affects_constraint(fisa_surveillance_expansion, national_security_classification_system).

% DUAL FORMULATION NOTE:
% FISA expansion is downstream of several legal/doctrinal constraints that make challenge difficult. State secrets privilege prevents litigation discovery; fourth amendment standing doctrine prevents individual standing for surveillance; classification system prevents public knowledge of actual programs. These constraints are upstream — they enable FISA expansion by blocking the normal exit pathways (litigation, political mobilization based on disclosed information) available to victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fisa_surveillance_expansion, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
