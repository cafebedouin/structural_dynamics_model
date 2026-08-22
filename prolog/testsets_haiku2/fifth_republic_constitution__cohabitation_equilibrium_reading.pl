% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Dual Executive Cohabitation Equilibrium
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   The Fifth Republic's constitutional structure (1958) created a dual
 *   executive with ambiguous authority allocation: a president elected
 *   independently and a prime minister drawn from the parliamentary majority.
 *   During periods when the president's party controls the Assembly,
 *   executive authority concentrates near presidential authority
 *   (hyper-presidential reading). When opposing coalitions control the
 *   presidency and the Assembly majority (cohabitation), a negotiated
 *   equilibrium emerges in which neither executive can unilaterally impose
 *   policy. This story instantiates the cohabitation equilibrium reading: a
 *   constraint that requires continuous negotiation between two competing
 *   authority centers, with neither able to claim sovereign decision-making
 *   authority without the other's consent or cooperation.
 *
 * KEY AGENTS:
 *   - President: Head of state with claimed sovereignty over foreign policy, defense, security; must negotiate domestic policy during cohabitation
 *   - Prime Minister: Head of government controlling domestic policy and administration; depends on Assembly majority; must negotiate foreign policy during cohabitation
 *   - National Assembly: Holds legislative power and can dissolve the government; directly empowers the prime minister during cohabitation
 *   - Bureaucratic state apparatus: Must implement policy under dual authority; absorbs coordination costs and policy incoherence
 *   - Civil society and public: Bears cost of executive inefficiency and policy uncertainty during cohabitation
 *   - Constitutional interpreters: Issue rulings that clarify or shift authority boundaries; their interpretation shapes which reading prevails
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.58).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.42).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Dual Executive Cohabitation Equilibrium").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '7baf1f60-6857-435d-8c20-e80aa2c5d7e0').
narrative_ontology:cs_kernel_codification('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', fixed_text).
narrative_ontology:cs_authority_grounding('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', lineage).
narrative_ontology:cs_interpretation_layer_present('7baf1f60-6857-435d-8c20-e80aa2c5d7e0').
narrative_ontology:cs_reading_relation('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', fifth_republic_constitution__hyper_presidential_reading, forecloses).
narrative_ontology:cs_reading_relation('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', fifth_republic_constitution__parliamentary_constraint_reading, forecloses).
narrative_ontology:cs_axiom('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', foundational, dual_executive_authority_requires_negotiation).
narrative_ontology:cs_axiom_status(dual_executive_authority_requires_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', dual_executive_authority_requires_negotiation, deontological).
narrative_ontology:cs_axiom('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', secondary, cohabitation_as_constitutional_practice).
narrative_ontology:cs_axiom_status(cohabitation_as_constitutional_practice, holdable).
narrative_ontology:cs_axiom_grounding('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', cohabitation_as_constitutional_practice, conventional).
narrative_ontology:cs_reference_frame('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', ambiguous_constitutional_text_1958).
narrative_ontology:cs_drift_state('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', contemporary_cohabitation_norm, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7baf1f60-6857-435d-8c20-e80aa2c5d7e0', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, whichever_institutional_actor_controls_key_policy_domain).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, executive_efficiency).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, competing_ideological_coalitions).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, president_office).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_office).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, bureaucratic_state).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_society_and_public).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, separation_of_powers_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_balance_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Occupies the head-of-state role with claimed sovereignty over foreign policy and defense. During cohabitation, must negotiate with a prime minister drawn from the opposing parliamentary majority. Cannot unilaterally implement domestic policy without prime minister consent. Controls constitutional framing and appointment authority but faces real legislative constraint. Must perform state representation while sharing executive authority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president_office, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, president_office, payer).

% Heads government and controls domestic policy, administration, and legislative agenda. Depends on parliamentary majority for survival but during cohabitation faces a president from the opposing coalition who controls foreign policy and defense. Must negotiate key decisions with the presidential office. Cannot claim sole executive authority; authority must be negotiated or split by domain.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_office, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_office, payer).

% Holds the constitutional power to dissolve and reconstitute the executive during parliamentary crises. Holds legislative authority and can be a check on both president and prime minister. During cohabitation, the Assembly majority directly empowers the prime minister and constrains presidential domestic action. Benefits from the constraint because it maintains parliamentary input on policy.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly, agenda_setter).

% Must implement policy under dual authority. Receives conflicting signals from president and prime minister on foreign affairs and internal security. Cannot optimize administration without clear chain of command. Absorbs the cost of coordination overhead and the inconsistency of split authority.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, bureaucratic_state, payer,
    organized, generational, trapped, national).

% Experiences policy incoherence during cohabitation: foreign and domestic policy may work at cross purposes; executive efficiency declines; implementation delays while the two executives negotiate. Public cannot exit the system; must bear the cost of negotiation delays and policy inconsistency.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, civil_society_and_public, payer,
    powerless, biographical, trapped, national).

% Each ideological faction (left/right, etc.) benefits when their coalition controls the majority-granting pole (presidency or Assembly). When opposed, both face negotiation; when aligned, the aligned faction gains concentrated control. The constraint structure shifts the distribution of power depending on coalition alignment at any moment.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, competing_ideological_coalitions, beneficiary,
    organized, generational, analytical, national).

% Interpret constitutional authority boundaries when conflicts arise between president and prime minister. Issue rulings that clarify or redraw the authority domain split. Their interpretation shapes which reading (cohabitation, hyper-presidential, or parliamentary) prevails in practice.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_interpreters_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__cohabitation_equilibrium_reading, diffuse).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__cohabitation_equilibrium_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents tyranny from concentrating in either the presidency or the Assembly by requiring negotiation between two elected executives with overlapping but contested authority. Forces policy to pass through multiple power centers (cohabitation) rather than flowing from a single sovereign actor.
% TRANSFER_FUNCTION: Transfers legislative power and domestic policy authority from president to prime minister (when cohabitation occurs); transfers foreign policy and defense authority remain contested between president and prime minister through continuous negotiation. Both executives can invoke constitutional framing favoring their position, extracting time and negotiation costs from each other and from the state apparatus.
% ABSENT_VOICES: The Gaullist vision of strong presidential authority (the hyper-presidential reading) is represented by the president's office but not by current constitutional doctrine in cohabitation mode. The pure-parliamentary reading (president as figurehead) is held by opposition theorists but not embedded in the current constitutional text. Neither reading is excluded from discourse but cohabitation doctrine mutes both in favor of a negotiated middle ground.
% DISAPPEARANCE_RATIONALE: If the cohabitation equilibrium vanished (either the president monopolized foreign policy and defense or the Assembly directly elected the executive), the balance of powers would shift dramatically. Policy would flow from a single source; legislative check on executive power would weaken or strengthen depending on the replacement. The state structure would reorganize around whatever the new reading of authority distribution became.
% FOUNDING_PROBLEM: The Fifth Republic's ambiguous constitutional text (1958) left unresolved whether the president or the Assembly held ultimate authority over foreign policy and defense during periods when the president's party did not control the Assembly majority. The founding problem was solved by creating a negotiated practice: cohabitation.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and political scientists (Duverger, Avril, Gaxie) from outside any current political coalition attest that cohabitation practice emerged empirically from 1986-onward as the settlement of this founding ambiguity. The Constitutional Council and successive presidents/prime ministers corroborate that cohabitation remains the operative practice when coalition control splits, confirming the founding problem persists as a structural feature of the Fifth Republic's design.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) and shows cyclical drift: it rises during periods of active cohabitation (time 8-32) when negotiation overhead is highest and executive efficiency declines, then stabilizes when one coalition aligns or gains electoral dominance. Theater ratio hovers near 0.48, reflecting continuous constitutional performativity: both executives invoke constitutional framing to justify their authority claims, and much cohabitation activity consists of negotiating the boundary through public statements and constitutional interpretation rather than direct policy execution. Suppression is moderate (0.42) because neither executive can unilaterally suppress the other's power — suppression attempts trigger constitutional crises (e.g., presidential dissolution of the Assembly, impeachment threats). The measurement series is shared across all three metrics on the 0-65 interval, with time points marking significant electoral transitions and cohabitation episodes in French Fifth Republic history (1986-88, 1993-95, 1997-2002 cohabitations; alignment periods in between). The cyclical pattern reflects the institutional rhythm: cohabitation → negotiation overhead and inefficiency rise → electoral pressure → coalition realignment or electoral victory → suppression of cohabitation → cycle repeats.
 *
 * PERSPECTIVAL GAP:
 *   From the presidency seat, cohabitation appears as a constraint on what should be sovereign executive authority; from the Assembly/prime minister seat, cohabitation appears as a defense against presidential overreach. From the bureaucratic state seat, cohabitation is purely extractive overhead — coordination costs with no benefit to service delivery. From the constitutional interpreter seat, cohabitation is a legitimate reading of an ambiguous text, not a constraint at all but a required practice. The engine computes these divergences from power atoms and exit options: the president and prime minister have symmetric power and similarly constrained exits (both are trapped once elected), so their d values are close to symmetric (d ≈ 0.5); the civil society seat faces no exit and high cost (d → 1.0, approaching a target position); the constitutional interpreter has analytical exit (d ≈ 0.5, symmetric in a different way).
 *
 * DIRECTIONALITY LOGIC:
 *   The cohabitation reading structures as a Tangled Rope: (1) genuine coordination function exists — the constraint solves a real problem (tyranny prevention, balance-of-powers maintenance) that beneficiaries cite; (2) asymmetric extraction is present — policy coherence and executive efficiency are sacrificed to maintain the equilibrium; (3) active enforcement required — neither executive can unilaterally dissolve the cohabitation; exit is constitutionally blocked. Beneficiaries are whichever institutional actors currently control key policy domains and can veto the other's initiatives. Victims are policy coherence (no single coherent executive strategy), the bureaucratic state (coordination overhead), and civil society (policy delays and inconsistency). The victim list records abstract entities (policy coherence, efficiency) because the extraction from civil society is distributed and diffuse, not captured by a single named agent.
 *
 * MANDATROPHY ANALYSIS:
 *   Cohabitation was not mandated by the Fifth Republic's constitutional text; it emerged empirically as a practice when the founding ambiguity about authority allocation could not be resolved by text alone. The mandate of the constraint is implicit: preserve separation of powers and prevent tyranny. The mandate has not died — preventing power concentration remains a live constitutional goal — but the mechanism has become contested. Each reading (cohabitation, hyper-presidential, parliamentary) claims to better serve the founding mandate. The Tangled Rope classification prevents misreading cohabitation as pure extraction (snare) or as pure coordination (rope): it is both simultaneously. The constraint persists because political actors find it preferable to a decisive choice between readings (which would create a winner and a loser), but the persistence is unstable — each cohabitation episode generates pressure toward one of the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_authority_allocation,
    'Does the Fifth Republic''s constitutional text (Articles 1-19, 44-47) allocate sovereign authority to the president or require shared authority with the prime minister during non-cohabitation periods?',
    'Constitutional court interpretation or amendment. If the text is read as presidential supremacy (hyper-presidential reading prevails), cohabitation becomes a temporary deviation; if the text is read as requiring parliamentary authorization (parliamentary reading prevails), cohabitation becomes the normal state.',
    'If text clearly allocates to president, cohabitation becomes a Snare (forced negotiation extracting efficiency); if text requires shared authority, cohabitation becomes a Rope (genuine coordination). This is the central reading-selection mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_text_authority_allocation, conceptual, 'Which constitutional reading (hyper-presidential vs. parliamentary vs. cohabitation) is the text''s true meaning?').

omega_variable(
    cyclical_vs_terminal_cohabitation,
    'Is cohabitation a cyclical institutional pattern (emerges and recedes with electoral cycles) or a terminal institutional state (once established empirically, will persist despite electoral realignments)?',
    'Historical observation: if cohabitation occurs predictably after each election in which opposing coalitions gain control of presidency and Assembly, pattern is cyclical; if cohabitation persists even after electoral victories that could justify hyper-presidential or parliamentary readings, pattern is terminal.',
    'Cyclical cohabitation remains a Tangled Rope (unstable equilibrium, extractiveness varies). Terminal cohabitation shifts toward Rope (stabilized coordination mechanism). This affects the predictability and evolution of the constraint type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cyclical_vs_terminal_cohabitation, empirical, 'Is the cohabitation equilibrium stable as an institutional form or transitional between electoral outcomes?').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression of cohabitation (attempts to move toward hyper-presidential or parliamentary readings) structural and external (constitutional court rulings, electoral mandates) or internalized (political actors have adopted cohabitation as legitimate practice and resist exit)?',
    'Test case: would a newly elected president with claimed hyper-presidential mandate attempt to suppress cohabitation if the Assembly remained in opposing control? Historical precedent (1995, 2002, 2007, 2017 transitions) suggests formal acceptance of cohabitation in principle but repeated pressure against it in practice.',
    'If suppression is structural, cohabitation persists by external mechanism (constitutional text, court interpretation, electoral reality). If internalized, suppression persists because actors have adopted cohabitation as legitimate, making exit costlier. Internalization would raise the constraint''s holding power but also its theatrical component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Is cohabitation suppression a structural feature or an internalized norm?').

omega_variable(
    hyper_presidential_sibling_foreclosure,
    'Does the cohabitation reading logically foreclose the hyper-presidential reading, or can both coexist as readings held by different parties of the same constitutional text?',
    'Constitutional theory: if hyper-presidentialism requires that the president holds sovereign authority over foreign policy and defense regardless of Assembly control, and cohabitation requires that the president must negotiate with the prime minister when the Assembly is in opposing control, then both claims cannot be simultaneously true for the same period. Foreclosure occurs.',
    'If cohabitation forecloses hyper-presidentialist, the two readings cannot both instantiate valid constraints from the same constitutional kernel — one is simply false. If they coexist, both are valid readings held by different political actors, and neither is foreclosed by the other''s epistemic structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hyper_presidential_sibling_foreclosure, conceptual, 'Does cohabitation reading logically foreclose the hyper-presidential reading?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fr5_cohabitation_tr_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(fr5_cohabitation_tr_t0, projected).
narrative_ontology:measurement(fr5_cohabitation_tr_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(fr5_cohabitation_tr_t8, observed).
narrative_ontology:measurement(fr5_cohabitation_tr_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement_basis(fr5_cohabitation_tr_t16, observed).
narrative_ontology:measurement(fr5_cohabitation_tr_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(fr5_cohabitation_tr_t24, observed).
narrative_ontology:measurement(fr5_cohabitation_tr_t32, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 32, 0.51).
narrative_ontology:measurement_basis(fr5_cohabitation_tr_t32, observed).
narrative_ontology:measurement(fr5_cohabitation_tr_t40, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement_basis(fr5_cohabitation_tr_t40, observed).
narrative_ontology:measurement(fr5_cohabitation_tr_t48, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 48, 0.46).
narrative_ontology:measurement_basis(fr5_cohabitation_tr_t48, observed).
narrative_ontology:measurement(fr5_cohabitation_tr_t56, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 56, 0.48).
narrative_ontology:measurement_basis(fr5_cohabitation_tr_t56, observed).
narrative_ontology:measurement(fr5_cohabitation_tr_t65, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 65, 0.48).
narrative_ontology:measurement_basis(fr5_cohabitation_tr_t65, observed).

% Extraction over time
narrative_ontology:measurement(fr5_cohabitation_be_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(fr5_cohabitation_be_t0, projected).
narrative_ontology:measurement(fr5_cohabitation_be_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(fr5_cohabitation_be_t8, observed).
narrative_ontology:measurement(fr5_cohabitation_be_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(fr5_cohabitation_be_t16, observed).
narrative_ontology:measurement(fr5_cohabitation_be_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(fr5_cohabitation_be_t24, observed).
narrative_ontology:measurement(fr5_cohabitation_be_t32, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 32, 0.62).
narrative_ontology:measurement_basis(fr5_cohabitation_be_t32, observed).
narrative_ontology:measurement(fr5_cohabitation_be_t40, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(fr5_cohabitation_be_t40, observed).
narrative_ontology:measurement(fr5_cohabitation_be_t48, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 48, 0.54).
narrative_ontology:measurement_basis(fr5_cohabitation_be_t48, observed).
narrative_ontology:measurement(fr5_cohabitation_be_t56, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 56, 0.58).
narrative_ontology:measurement_basis(fr5_cohabitation_be_t56, observed).
narrative_ontology:measurement(fr5_cohabitation_be_t65, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 65, 0.58).
narrative_ontology:measurement_basis(fr5_cohabitation_be_t65, observed).

% Suppression requirement over time
narrative_ontology:measurement(fr5_cohabitation_su_t0, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(fr5_cohabitation_su_t0, projected).
narrative_ontology:measurement(fr5_cohabitation_su_t8, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement_basis(fr5_cohabitation_su_t8, observed).
narrative_ontology:measurement(fr5_cohabitation_su_t16, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 16, 0.39).
narrative_ontology:measurement_basis(fr5_cohabitation_su_t16, observed).
narrative_ontology:measurement(fr5_cohabitation_su_t24, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 24, 0.42).
narrative_ontology:measurement_basis(fr5_cohabitation_su_t24, observed).
narrative_ontology:measurement(fr5_cohabitation_su_t32, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 32, 0.44).
narrative_ontology:measurement_basis(fr5_cohabitation_su_t32, observed).
narrative_ontology:measurement(fr5_cohabitation_su_t40, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(fr5_cohabitation_su_t40, observed).
narrative_ontology:measurement(fr5_cohabitation_su_t48, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 48, 0.4).
narrative_ontology:measurement_basis(fr5_cohabitation_su_t48, observed).
narrative_ontology:measurement(fr5_cohabitation_su_t56, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 56, 0.42).
narrative_ontology:measurement_basis(fr5_cohabitation_su_t56, observed).
narrative_ontology:measurement(fr5_cohabitation_su_t65, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 65, 0.42).
narrative_ontology:measurement_basis(fr5_cohabitation_su_t65, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.12).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% The cohabitation_equilibrium_reading is one of three constraint stories decomposed from the kernel 'fifth_republic_constitution'. Sibling readings are hyper_presidential_reading and parliamentary_constraint_reading. Each reading instantiates a different constraint from the same ambiguous constitutional text; each ε is stable within its reading and distinct from the others. The three readings coexist as live positions held by different French political actors; no single reading has foreclosed the others, though each reading creates pressure on the others through constitutional interpretation and electoral outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fifth_republic_constitution__cohabitation_equilibrium_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
