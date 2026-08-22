% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__security_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__security_necessity_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy__security_necessity_reading
 *   human_readable: Security-Necessity Doctrine of Territorial Legitimacy (Post-1967 Control Regime)
 *   domain: political theory/international law/territorial sovereignty
 *
 * SUMMARY:
 *   Since 1967 a controlling state has administered territory beyond its 1949
 *   armistice lines — the West Bank and the Golan Heights — under a
 *   legitimacy doctrine that reads control as defensive necessity: borders
 *   redrawn for strategic depth, neighboring sovereignty made conditional on
 *   demilitarization, and civilian presence beyond the lines framed as
 *   security deployment. This file instantiates ONE reading of the contested
 *   territorial_legitimacy kernel — the security_necessity_reading — and
 *   authors epsilon for the standing arrangement (the post-1967 control
 *   regime) as THAT reading assesses it: the control core is endorsed as
 *   legitimate, while the reading's own lights register substantial costs
 *   imposed beyond strict necessity (deep-interior settlement growth, the
 *   permit and closure regime, revenue withholding). Sibling readings are
 *   separate constraint files with their own epsilon values; nothing here
 *   averages across them. KEY AGENTS (by structural relationship): -
 *   palestinian_residents_west_bank: Primary target (powerless/trapped) —
 *   bears the regime's daily costs - syrian_golan_druze: Secondary target
 *   (powerless/trapped) — annexed population, land expropriated -
 *   west_bank_settler_population: Primary beneficiary
 *   (organized/identity_locked) — receives land, subsidy, protection -
 *   israeli_security_establishment: Agenda setter
 *   (institutional/identity_locked) — administers and defines necessity -
 *   israeli_citizenry_general: Diffuse beneficiary-payer (organized/mobile) —
 *   receives perceived security, pays in service and exposure -
 *   palestinian_authority: Subcontracted administrator-payer
 *   (moderate/constrained) — enforces conditionality, bears its costs -
 *   us_executive_branch: Patron beneficiary-agenda setter
 *   (institutional/arbitrage) — shields and brokers -
 *   international_legal_institutions: Excluded objector
 *   (institutional/analytical) — holds the contrary frame, no seat in the
 *   room
 *
 * KEY AGENTS:
 *   - palestinian_residents_west_bank: Primary target (powerless/trapped) — bears extraction
 *   - syrian_golan_druze: Secondary target (powerless/trapped)
 *   - west_bank_settler_population: Primary beneficiary (organized/identity_locked)
 *   - israeli_security_establishment: Agenda setter (institutional/identity_locked)
 *   - israeli_citizenry_general: Diffuse beneficiary-payer (organized/mobile)
 *   - palestinian_authority: Subcontracted administrator-payer (moderate/constrained)
 *   - us_executive_branch: Patron beneficiary-agenda setter (institutional/arbitrage)
 *   - international_legal_institutions: Excluded objector (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, 0.58).
domain_priors:suppression_score(territorial_legitimacy__security_necessity_reading, 0.68).
domain_priors:theater_ratio(territorial_legitimacy__security_necessity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(territorial_legitimacy__security_necessity_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__security_necessity_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy__security_necessity_reading, "Security-Necessity Doctrine of Territorial Legitimacy (Post-1967 Control Regime)").
narrative_ontology:topic_domain(territorial_legitimacy__security_necessity_reading, "political theory/international law/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__security_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__security_necessity_reading, '45721a9e-1db8-419d-bc39-363483656657').
narrative_ontology:cs_kernel_codification('45721a9e-1db8-419d-bc39-363483656657', formalized).
narrative_ontology:cs_authority_grounding('45721a9e-1db8-419d-bc39-363483656657', expertise).
narrative_ontology:cs_interpretation_layer_present('45721a9e-1db8-419d-bc39-363483656657').
narrative_ontology:cs_reading_relation('45721a9e-1db8-419d-bc39-363483656657', territorial_legitimacy__partition_reading, influences).
narrative_ontology:cs_reading_relation('45721a9e-1db8-419d-bc39-363483656657', territorial_legitimacy__indigenous_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('45721a9e-1db8-419d-bc39-363483656657', foundational, territorial_depth_equals_survival_margin).
narrative_ontology:cs_axiom_status(territorial_depth_equals_survival_margin, holdable).
narrative_ontology:cs_axiom_grounding('45721a9e-1db8-419d-bc39-363483656657', territorial_depth_equals_survival_margin, empirically_contingent).
narrative_ontology:cs_axiom('45721a9e-1db8-419d-bc39-363483656657', foundational, sovereignty_conditional_on_demilitarization).
narrative_ontology:cs_axiom_status(sovereignty_conditional_on_demilitarization, holdable).
narrative_ontology:cs_axiom_grounding('45721a9e-1db8-419d-bc39-363483656657', sovereignty_conditional_on_demilitarization, empirically_contingent).
narrative_ontology:cs_reference_frame('45721a9e-1db8-419d-bc39-363483656657', defensible_borders_strategic_depth).
narrative_ontology:cs_drift_state('45721a9e-1db8-419d-bc39-363483656657', post_peace_treaty_threat_shift, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('45721a9e-1db8-419d-bc39-363483656657', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__security_necessity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_security_establishment).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, west_bank_settler_population).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, israeli_citizenry_general).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__security_necessity_reading, us_executive_branch).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_residents_west_bank).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, syrian_golan_druze).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, palestinian_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(territorial_legitimacy__security_necessity_reading, israeli_citizenry_general).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, defensible_borders_principle).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, demilitarization_conditionality).
narrative_ontology:constraint_vindicates(territorial_legitimacy__security_necessity_reading, deterrence_through_depth).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Plans, administers, and enforces the post-1967 control regime: the military government, coordination mechanisms, permit systems, and the definitions of what counts as a security requirement. Its budgets, career structures, and institutional self-conception are bound to continued administration of the territories; stepping back would mean redefining the institution's core mission and admitting that decades of its central task were misdirected. It defines necessity and then reports on necessity.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_security_establishment, agenda_setter,
    institutional, generational, identity_locked, regional).

% Resides beyond the 1949 armistice lines under a framework that reads civilian presence as security deployment. Receives land allocation, housing subsidies, infrastructure investment, and military protection. Leaving would mean abandoning homes, communities, and a national-religious project fused with residence itself; the population is politically mobilized to make the current boundaries permanent.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, west_bank_settler_population, beneficiary,
    organized, generational, identity_locked, regional).

% Lives under military administration with a separate legal system, movement restrictions, a permit regime, and periodic land requisition. Has no vote in the polity that sets the rules governing daily life. Sovereignty prospects are conditional on terms it does not set. Exit means exile, and the surrounding states and politics close that door as firmly as the administering power does.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_residents_west_bank, payer,
    powerless, biographical, trapped, regional).

% Resident population of the Golan captured in 1967. Most declined offered citizenship and retain Syrian identity documents while living under law extended by the administering state after the 1981 annexation application. Agricultural land was expropriated for settlements and buffer zones. Identification remains with Syria; physical exit means displacement from ancestral villages.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, syrian_golan_druze, payer,
    powerless, generational, trapped, local).

% Administers urban enclaves under the interim architecture, runs the security coordination that operationalizes the demilitarization conditionality on the ground, and receives tax revenues collected and remitted — or withheld — by the administering state. Depends on the arrangement for its own existence and funding while bearing its legitimacy costs among its constituency. It cannot dissolve the regime it helps administer, and its offices lapse under the framework's own timeline.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, palestinian_authority, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, palestinian_authority, agenda_setter).

% Receives perceived security from strategic depth, early-warning terrain, and the forward positioning of threats away from population centers. Pays through conscription and reserve service, defense spending, and casualty exposure in the administered territories. Votes for the governments that set the framework. Emigration is a real option exercised by minorities; the majority's stake is expressed at the ballot box rather than by exit.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, israeli_citizenry_general, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, israeli_citizenry_general, payer).

% Extends diplomatic protection — security-council vetoes, military assistance, aid — and brokers frameworks whose terms are priced in security language. Gains alliance leverage, intelligence cooperation, and regional influence from the relationship. Can shift policy at electoral cost but faces no structural lock; its support is a chosen input to the regime's persistence, not a compelled one.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, us_executive_branch, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__security_necessity_reading, us_executive_branch, agenda_setter).

% Court, assembly majorities, and council members hold the contrary frame: the territories are held under belligerent occupation, acquisition by force is inadmissible, and civilian transfer into occupied land breaches treaty law. They issue advisory opinions, resolutions, and referrals. The enforcing coalition does not recognize these seats as authoritative over its arrangements, so the objection is voiced but holds no place in the conversation where the regime is actually set.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__security_necessity_reading, international_legal_institutions, excluded,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__security_necessity_reading, west_bank_settler_population).
narrative_ontology:fixing_cost_class(territorial_legitimacy__security_necessity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Substitutes security guarantees for trust between armed neighbors: it makes territorial compromise computable by pricing sovereignty in demilitarization terms, gives both parties a common vocabulary for land-for-peace trades, and lets a small state trade withdrawal for verified restraint instead of requiring mutual confidence it cannot verify.
% TRANSFER_FUNCTION: Moves land, water access, movement freedom, and administrative authority from the governed populations of the West Bank and Golan to the controlling state and its settler population; moves security risk-bearing from the controlling state's core population outward onto the administered periphery; moves revenue handling and security-coordination labor to the subcontracted administrator.
% ABSENT_VOICES: The governed populations hold no vote in the polity that sets the rules governing them — West Bank residents vote in Palestinian elections whose offices the framework constrains, not in the Knesset that administers them. International legal institutions hold the contrary legitimacy frame and issue rulings, but the enforcing coalition does not recognize those seats as authoritative, so the strongest objections exist outside the room where the regime is set.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished overnight, the settlement enterprise loses its legitimating vocabulary, the negotiated-framework architecture (security annexes, phased sovereignty, demilitarization pricing) collapses back to raw power contestation, and the control regime would have to re-legitimate itself on other grounds — annexationist, religious-historical, or international-consent — each of which rearranges governing coalitions on all sides.
% FOUNDING_PROBLEM: After 1948 and again after 1967, the state's planners judged the armistice lines militarily indefensible: a nine-to-fifteen-mile coastal waist, hostile armies on three land borders, and artillery range covering airports, power plants, and population centers. The doctrine was built to answer how a small state obtains defensible borders against existential-scale conventional attack.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the Egyptian and Jordanian peace treaties attest that the state-army invasion vector the depth doctrine targeted was closable by agreement rather than territory; retired security chiefs from inside the security establishment have publicly attested that permanent control no longer serves defense and that the founding configuration is superseded; independent strategic-studies literature documents the shift toward threat vectors that territory does not address. Current-serving security leadership contests all of this. Stated plainly: corroboration exists but is disputed, and no uncontested external attestation exists in either direction.
narrative_ontology:disappearance_verdict(territorial_legitimacy__security_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__security_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__security_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__security_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__security_necessity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__security_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__security_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__security_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58) is moderate-high by this reading's own assessment: the doctrine legitimizes the control core while conceding that costs to the governed population have accumulated well past what defense requires. Suppression (0.68) is a raw structural property — military administration, the permit regime, closure policy, parallel legal systems — and is deliberately NOT scaled by power or scope; only extractiveness is scaled downstream by directionality and verification difficulty. Theater (0.32) reflects a growing share of justification activity that maintains the necessity claim rhetorically rather than functionally, notably settlements of negligible defensive value presented as security assets. Accessibility collapse (0.48) is mid-range: rival legitimacy frames dominate international legal discourse while being nearly foreclosed inside the enforcing polity's domestic arena. Resistance (0.66) registers sustained uprising cycles, diplomatic isolation campaigns, litigation, and coalition-building among the stateless parties — the coalition channel is the main counterweight available to otherwise powerless targets. The claim (tangled_rope) and the metrics are independently authored: the doctrine does coordinate — it is the shared vocabulary in which every land-for-peace framework is priced — and it extracts asymmetrically through that same structure; the engine computes per-seat types from the structural data, and payer seats will legitimately compute harsher than the claim. All three series run on one shared eight-point grid. The trajectories show a ratchet-and-pause cycle rather than smooth drift: each violence episode (1987, 2000) spikes enforcement and extraction, each negotiation window (1993) relaxes them briefly, and relaxation never returns to the prior baseline — the oscillation functions as intermittent reinforcement that ratchets the steady state upward, and is itself part of the maintenance mechanism, not noise around it. Scalars reflect the 2024 endpoint of that cycle.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the agenda-setter seat and the settler seat, the arrangement presents as coordination: protection, order, a negotiable framework in which sovereignty is priced rather than denied. From the payer seats, the identical structure presents as enforced subordination: permits, land requisition, conditional standing under military law. The subcontracted administrator experiences both at once — it operationalizes the demilitarization conditionality while absorbing its legitimacy costs among its own constituency. The excluded international-legal seat computes the arrangement as an illegitimate assertion outright. The engine derives these divergences from power, exit, and role data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low d for the settler population, the security establishment, the general citizenry, and the US patron; victim declarations drive high d for West Bank and Golan residents. Exit modulation sharpens the spread: trapped residents sit nearest the full-target end, while the US patron's arbitrage-grade exit keeps it near the beneficiary end despite its agenda-setting role. One override is authored: the Palestinian Authority derives a near-full-target d from its victim declaration, but its security-coordination role gives it an administrative stake in the regime's continuation, so d is corrected down to 0.65. The general citizenry's mixed position (beneficiary declaration offset by conscription, taxation, and casualty exposure) is left to the derivation; its net position remains beneficiary-side and no override is needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — armistice lines that left population centers within artillery range of massed state armies — has been transformed rather than solved: peace treaties closed the state-army invasion vector from Egypt and Jordan, while successor threats are invoked to keep the mandate live. Founding_problem_status is therefore contested rather than dead, and the mismatch consumer will find no clean zombie flag; but the theater_ratio trajectory (0.08 to 0.32) marks the necessity claim's growing performative maintenance, and the extraction accumulation series (0.22 to 0.58) is the accumulating-rent signature of a defensive core acquiring layers justified by the original mandate. The tangled_rope classification prevents double mislabeling: a pure-extraction reading would erase the genuine coordination the framework performed — withdrawal-for-peace trades were priced in exactly this vocabulary — while a pure-coordination reading would erase who pays for the coordination. The same structure that made withdrawal negotiable makes the extraction enforceable; that conjunction is the finding, not an artifact to be reconciled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the territorial_legitimacy kernel; would adopting the partition_reading or indigenous_continuity_reading restructure the victim set, the beneficiary set, and the epsilon ordering?',
    'Comparative classification of the sibling constraint files; engine foreclosure computation from the grounding types of each reading''s axioms.',
    'Under the partition_reading the same control regime computes as unlawful occupation with near-full-target directionality for the controlling state; under the indigenous_continuity_reading the victim set widens to include the 1948-displaced populations and epsilon rises sharply. The disagreement is located in the foundational premise of what grounds title, not in the facts of control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the territorial-legitimacy kernel governs, and what each sibling would change structurally.').

omega_variable(
    necessity_expansion_ambiguity,
    'Does the security justification track actual defense requirements, or has ''necessity'' expanded over the interval to cover ideologically and economically driven settlement growth?',
    'Declassified planning records; geographic analysis of settlement placement against documented defense needs; internal assessments distinguishing security assets from civilian enterprise.',
    'If necessity has expanded, the effective extraction of the regime rises toward a pure-extraction profile and the theater ratio understates performative maintenance; if the justification remains accurate, the coordination framing strengthens and epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_expansion_ambiguity, empirical, 'Whether the necessity claim still refers to defense or has become cover.').

omega_variable(
    threat_vector_substitution,
    'Has the founding threat configuration (conventional armies crossing narrow borders) been superseded by vectors — rocketry, drones, proxy forces, cyber — that territorial depth does not mitigate?',
    'Strategic-studies synthesis comparing the marginal protective value of depth across threat classes; revealed preference in defense procurement between terrain fortification and missile/air defense.',
    'If superseded, the founding problem is dead and the arrangement persists increasingly by inertia and performance, pressing toward degraded-inertial classification; if live, the doctrine retains substantive coordinating function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_vector_substitution, empirical, 'Whether the founding problem the arrangement was built for still exists in its original form.').

omega_variable(
    conditionality_symmetry,
    'Does demilitarization conditionality bind both parties symmetrically — each side''s security conditioned on the other''s verified restraint — or does it operate unilaterally, conditioning only the weaker party''s sovereignty?',
    'Textual comparison of negotiated frameworks (Camp David accords, Oslo Annex I, Clinton parameters) for reciprocal obligations; behavioral record of which side''s obligations were enforced.',
    'A symmetric reading supports the coordination half of the hybrid classification; a unilateral reading concentrates effective extraction on the conditioned side and pushes payer-seat classifications toward pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(conditionality_symmetry, conceptual, 'Whether the conditionality structure is reciprocal or one-directional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__security_necessity_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__security_necessity_reading, theater_ratio, 1967, 0.08).
narrative_ontology:measurement(terr_tr_t1977, territorial_legitimacy__security_necessity_reading, theater_ratio, 1977, 0.14).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__security_necessity_reading, theater_ratio, 1987, 0.2).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__security_necessity_reading, theater_ratio, 1993, 0.17).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__security_necessity_reading, theater_ratio, 2000, 0.25).
narrative_ontology:measurement(terr_tr_t2005, territorial_legitimacy__security_necessity_reading, theater_ratio, 2005, 0.29).
narrative_ontology:measurement(terr_tr_t2017, territorial_legitimacy__security_necessity_reading, theater_ratio, 2017, 0.31).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__security_necessity_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1967, 0.22).
narrative_ontology:measurement(terr_be_t1977, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1977, 0.3).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1987, 0.36).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__security_necessity_reading, base_extractiveness, 1993, 0.34).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2000, 0.43).
narrative_ontology:measurement(terr_be_t2005, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2005, 0.47).
narrative_ontology:measurement(terr_be_t2017, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2017, 0.53).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__security_necessity_reading, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1967, 0.3).
narrative_ontology:measurement(terr_su_t1977, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1977, 0.36).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1987, 0.52).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__security_necessity_reading, suppression_requirement, 1993, 0.44).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement(terr_su_t2005, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement(terr_su_t2017, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2017, 0.67).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__security_necessity_reading, suppression_requirement, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__security_necessity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__security_necessity_reading, territorial_legitimacy__indigenous_continuity_reading).

% DUAL FORMULATION NOTE:
% 'Territorial legitimacy' in colloquial usage conflates three structurally distinct claims with different epsilon, different victim sets, and different failure modes. Decomposed per the epsilon-invariance principle into a three-story family: partition_reading (upstream — the international-law frame the other readings negotiate against), security_necessity_reading (this file), and indigenous_continuity_reading. This file links both siblings. The upstream partition frame shapes the operating environment of the other two readings without resolving the contest; the security reading in turn exerts downstream pressure on how partition lines get priced (every boundary negotiation now carries security adjustments), which is why the relation to the partition sibling is influences rather than coexists_with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy__security_necessity_reading, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
