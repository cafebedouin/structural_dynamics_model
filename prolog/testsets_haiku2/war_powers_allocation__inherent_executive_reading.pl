% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive War Powers Authority (Commander-in-Chief Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The inherent-executive reading of Commander-in-Chief power interprets
 *   Article II, Section 2 as granting the president unilateral authority to
 *   deploy military force in defense of national interests without prior
 *   legislative authorization. This is ONE reading of a contested
 *   constitutional kernel: the war-powers allocation text. The constraint's
 *   structure establishes the executive as the agenda-setter and interpreter
 *   of national interest, subordinates congressional authorization to
 *   executive fait accompli, and transfers authorization authority from a
 *   shared structure to unilateral executive judgment. The reading's
 *   beneficiary is the executive branch (gains speed and operational
 *   sovereignty); its victims are the legislative branch (loses ex-ante gate
 *   control) and the public (loses ex-ante democratic authorization, retains
 *   fiscal and casualty costs). The measurement series track how extraction
 *   has accumulated from 1945 through 2026 as successive presidents have
 *   normalized unilateral deployment.
 *
 * KEY AGENTS:
 *   - executive_branch: President and war-making apparatus; claims inherent constitutional authority; sets deployment agenda unilaterally
 *   - legislative_branch: Congress; holds formal war-powers authority but finds it suppressed by executive fait accompli; constrained by political cost of mid-campaign defunding
 *   - american_public: Bears military costs; lacks ex-ante authorization mechanism; subject to appropriations-as-ratification rather than prior consent
 *   - supreme_court: Applies deference doctrine; removes judicial check on executive reading
 *   - constitutional_scholars: Contest the reading's textual fidelity; this observer seat supplies the external scholarly record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.68).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.42).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.51).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers Authority (Commander-in-Chief Reading)").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional/political").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, 'b0a5d920-a11c-4b3a-b98f-047c1b09a843').
narrative_ontology:cs_kernel_codification('b0a5d920-a11c-4b3a-b98f-047c1b09a843', fixed_text).
narrative_ontology:cs_authority_grounding('b0a5d920-a11c-4b3a-b98f-047c1b09a843', extraction).
narrative_ontology:cs_interpretation_layer_present('b0a5d920-a11c-4b3a-b98f-047c1b09a843').
narrative_ontology:cs_reading_relation('b0a5d920-a11c-4b3a-b98f-047c1b09a843', war_powers_allocation__congressional_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0a5d920-a11c-4b3a-b98f-047c1b09a843', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('b0a5d920-a11c-4b3a-b98f-047c1b09a843', foundational, commander_in_chief_grants_unilateral_operational_authority).
narrative_ontology:cs_axiom_status(commander_in_chief_grants_unilateral_operational_authority, holdable).
narrative_ontology:cs_axiom_grounding('b0a5d920-a11c-4b3a-b98f-047c1b09a843', commander_in_chief_grants_unilateral_operational_authority, deontological).
narrative_ontology:cs_axiom('b0a5d920-a11c-4b3a-b98f-047c1b09a843', foundational, national_interest_defense_justifies_speed_over_deliberation).
narrative_ontology:cs_axiom_status(national_interest_defense_justifies_speed_over_deliberation, holdable).
narrative_ontology:cs_axiom_grounding('b0a5d920-a11c-4b3a-b98f-047c1b09a843', national_interest_defense_justifies_speed_over_deliberation, instrumental).
narrative_ontology:cs_reference_frame('b0a5d920-a11c-4b3a-b98f-047c1b09a843', constitutional_executive_prerogative).
narrative_ontology:cs_drift_state('b0a5d920-a11c-4b3a-b98f-047c1b09a843', post_war_on_terror_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0a5d920-a11c-4b3a-b98f-047c1b09a843', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, legislative_branch).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, american_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The president and executive war-making apparatus claim inherent constitutional authority to deploy military force in defense of national interests without prior legislative authorization. They defend the necessity of unilateral action given threats that move faster than congressional process; they interpret Commander-in-Chief power as granting operational sovereignty over force deployment. They control the initiation of military action and the narrative framing of threats.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch, agenda_setter,
    institutional, biographical, arbitrage, global).

% Congress claims constitutional war-powers authority (Declare War clause, funding authority) but finds its formal powers subordinated to executive fait accompli: military deployments proceed without declaration, funded through appropriations Congress passes reluctantly or repurposes from other authorized budgets. Congress can theoretically defund or impeach, but defunding mid-campaign is politically costly and impeachment is a constitutional extreme. Its constraint is that the executive's interpretation of its own powers limits Congress's practical leverage.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, legislative_branch, payer,
    institutional, biographical, constrained, global).

% Bears the costs of military action (casualties, fiscal expenditure, blowback risk) without formal democratic authorization. Public opinion can build resistance to prolonged wars, but the constraint operates to reduce the requirement for ex-ante consent and substitute appropriations-as-ratification. Once military deployment begins, stopping it requires overcoming executive inertia and the appearance of weakness — a higher bar than preventing initiation would have been.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, american_public, payer,
    powerless, biographical, trapped, national).

% Are subject to unilateral force deployment without the friction that would attend a public congressional debate. They have no seat in the constraint structure but are affected by the speed and decisiveness the reading enables. The constraint suppresses adversary voice in the authorization process itself.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, foreign_adversaries_and_allies, excluded,
    powerful, biographical, trapped, global).

% Reviews war-powers challenges and has historically applied deference (political-question doctrine, standing barriers) to executive war-making. The Court's refusal to enforce congressional war-powers claims strengthens the inherent-executive reading by removing judicial constraint.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, supreme_court, observer,
    institutional, generational, analytical, national).

% Contests the reading's fidelity to original constitutional text and structure. Scholars holding the congressional-primacy or functional-accommodation readings argue the inherent-executive framing misreads the Framers' design; inherent-executive advocates argue Congress delegated operational flexibility by granting Commander-in-Chief power without explicit limitations.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rapid military response to threats that move faster than full congressional deliberation — the coordination function is crisis-speed decision-making in moments where the legislative process (debate, deliberation, voting across 535 members) cannot meet the operational timeline.
% TRANSFER_FUNCTION: Transfers war-authorization power from a shared (executive-legislative) structure to unilateral executive authority. Congress loses ex-ante gate control; it retains funding authority but the constraint structure makes funding refusal politically harder once force is deployed. The public transfers authority to choose combat from democratic deliberation to executive judgment; they retain the fiscal cost and casualty burden.
% ABSENT_VOICES: Congressional legislators who would argue for the constitutional necessity of authorization (the congressional-primacy reading) are seated in the constraint but their formal power is suppressed by the inherent-executive interpretation. Foreign adversaries and the affected public have no seat in the authorization process itself — their voice is entirely absent by design of the constraint.
% DISAPPEARANCE_RATIONALE: If the inherent-executive reading collapsed and were replaced by the congressional-primacy requirement, every military deployment would require explicit legislative authorization before initiation. The tempo, scale, and frequency of force deployment would change — some actions the executive now takes unilaterally would not occur; others would be delayed pending authorization; still others would require the executive to build a legislative coalition. The constitutional structure of separation of powers would functionally shift.
% FOUNDING_PROBLEM: The Framers granted the president Commander-in-Chief authority to conduct military operations, but the Declare War clause and power of the purse to Congress. The founding problem this reading solves is the latency and uncertainty of legislative authorization under threat: can a president wait for Congress to debate and vote when an attack is imminent or a strategic window closing?
% FOUNDING_PROBLEM_CORROBORATION: The executive branch and its constitutional defenders (Department of Justice Office of Legal Counsel, executive-friendly scholars, hawks in Congress) attest the founding problem is live: threats move at speeds Congress cannot match. The congressional-primacy reading and its defenders (legislative legal counsel, constitutional scholars focused on structural limits, civil-rights and anti-war organizations) attest the founding problem was solved at the Framers' intention: the Framers KNEW military threats were urgent and still gave Congress the declaration power — that choice reflects deliberate constraint on executive speed, not oversight. No neutral observer outside the benefiting/constraining institutions has corroborated the urgency claim in a way that settles which reading captures the Framers' intent.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 by 2026 because the reading has enabled sustained unilateral force deployment (Korea, Vietnam, Gulf Wars, Iraq, Afghanistan, Syria, drone operations) without explicit declarations of war, and Congress has lost practical leverage to prevent initiation — defunding after deployment is politically harder than preventing it beforehand. The measurement series shows extraction rising from 0.38 in 1945 (when the reading was nascent and Congress still exercised real constraint) through 1964 (Gulf of Tonkin marked the shift), stabilizing at 0.68 after 2001 (War on Terror normalized the reading). Suppression is moderate (0.42) because Congress is not prevented from debating or voting — its votes simply carry less weight ex-ante and more weight ex-post. Theater is moderate (0.38) because the constraint maintains a performance of constitutional deliberation (war authorizations like AUMF 2001) while the real authorization authority is executive. The separation of powers is enacted as ritual; the substantive gate has moved. Accessibility collapse is moderate (0.51) because alternatives to the reading exist (congressional primacy is still textually defensible) and resistance is high (0.72) because the legislative branch and anti-war constituencies mount continuous challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the executive seat, the reading is necessary operational authority protecting national interests against threats moving faster than legislative deliberation — genuine coordination function granting speed. From the legislative seat, the reading is extraction of constitutional authority through interpretive fait accompli — Congress is coordinated into the constraint (it funds the wars) but the real authorization gate is suppressed. From the public seat, it is a loss of democratic ex-ante authorization, substituting post-hoc appropriations for prior consent. The engine will compute these as different directionalities: the executive gets low d (beneficiary, arbitrage exit), Congress gets high d (target, constrained exit), the public gets high d (trapped, powerless). The claimed type is tangled_rope because the reading does coordinate rapid response (genuine function) while extracting authorization authority from Congress (asymmetric) and requires active suppression of congressional constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive's d is near the beneficiary end: it sets the agenda, claims the authority, and faces no binding prior constraint. Congress's d is high (near full target): it retains formal power but the reading's interpretation subordinates that power to executive fait accompli — Congress pays the political cost of constraint without the power to prevent initiation. The public's d is at full target end (1.0): it bears military costs without ex-ante authorization mechanism; it is trapped and powerless. The legislative branch is listed as 'payer' because its formal constitutional authority is the thing extracted and subordinated by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to balance speed against democratic authorization) remains live but the reading has shifted from a tangled_rope (coordination + extraction) toward the snare end. The coordination function (rapid response) is real but increasingly theater: sustained campaigns (Iraq 18 years, Afghanistan 20 years) do not require the speed the founding problem justified. The extraction (authority subordination) has accumulated — Congress has normalized appropriations-as-consent rather than prior authorization, ratifying the reading through repeated funding votes rather than challenging the underlying authority claim. The constraint persists partly through genuine speed-necessity and partly through theatrical performance of deliberation that does not actually gate action. This is not yet a piton (the tension between the reading and congressional primacy remains hot, and periodic authorization votes maintain the performance), but the gap between founding justification and actual operation has widened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    imminence_threshold_ambiguity,
    'What counts as an ''imminent'' threat sufficient to justify unilateral executive action without congressional authorization? How do we distinguish threats that require speed from threats that can await deliberation?',
    'Doctrine crystallization through Supreme Court rulings on the ''imminence'' standard, or legislative definition through War Powers Resolution amendments. Observe whether presidents claim broader imminence thresholds than courts would uphold.',
    'A narrow imminence standard (truly seconds-to-minutes threats) would constrain the reading''s scope; a broad one (strategic windows, regional instability, intelligence assessments of hostile intent) expands it to cover most force deployment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminence_threshold_ambiguity, conceptual, 'The boundary between threats that justify unilateral speed and threats requiring democratic authorization.').

omega_variable(
    factual_vs_legal_imminence,
    'Is imminence a factual property of threats (measurable time-to-impact) or a legal conclusion the executive claims (the president''s assessment of strategic necessity)?',
    'Analyze how imminence determinations are made in practice: do presidents present evidence to Congress, or declare threats imminent and deploy unilaterally? Observe whether Congress retroactively authorizes based on the president''s threat assessment.',
    'If imminence is legal conclusion claimed by the executor, the reading becomes self-justifying — the executive''s assessment of necessity becomes the authorization. If it is a factual threshold, it is externally verifiable and constraining.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(factual_vs_legal_imminence, empirical, 'Whether imminence is determined by verifiable facts or executive assertion.').

omega_variable(
    reading_foreclosure_via_structural_practice,
    'Has the normalization of unilateral deployment through successive administrations, combined with the Court''s deference doctrine, functionally foreclosed the congressional-primacy reading as a live legal position?',
    'Observe whether any president has actually deferred to Congress for a force deployment deemed strategically necessary. Examine whether courts would enjoin a deployment the executive deemed critical. Monitor whether legislatures can withdraw from the constraint.',
    'If congressional primacy is foreclosed as operational law (even if live in academic theory), the sibling readings do not genuinely coexist — the inherent-executive reading has become the only live structural option. This would shift the relation from ''coexists_with'' to ''forecloses''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_via_structural_practice, empirical, 'Whether the inherent-executive reading has foreclosed the congressional-primacy reading through successful normalization.').

omega_variable(
    democratic_authorization_substitution,
    'Does the appropriations-as-ratification mechanism provide genuinely democratic authorization, or is it a post-hoc rubber-stamp that serves as theater?',
    'Analyze whether Congress can and does refuse appropriations for ongoing operations; observe whether appropriations votes are genuine authorizations or procedural formalities; examine whether public opinion on the military action correlates with appropriations voting.',
    'If appropriations are genuine authorization (Congress votes ''no'' and the operation stops), the constraint carries distributed democratic input. If they are theater (Congress votes ''yes'' under pressure, and ''no'' votes are politically impossible mid-operation), the reading extracts authorization more fully than the metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_authorization_substitution, empirical, 'Whether post-hoc appropriations provide genuine democratic constraint or serve as theater substituting for prior authorization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_powers_allocation__inherent_executive_reading, theater_ratio, 1945, 0.22).
narrative_ontology:measurement_basis(war__tr_t1945, observed).
narrative_ontology:measurement(war__tr_t1964, war_powers_allocation__inherent_executive_reading, theater_ratio, 1964, 0.28).
narrative_ontology:measurement_basis(war__tr_t1964, observed).
narrative_ontology:measurement(war__tr_t1980, war_powers_allocation__inherent_executive_reading, theater_ratio, 1980, 0.33).
narrative_ontology:measurement_basis(war__tr_t1980, observed).
narrative_ontology:measurement(war__tr_t2001, war_powers_allocation__inherent_executive_reading, theater_ratio, 2001, 0.37).
narrative_ontology:measurement_basis(war__tr_t2001, observed).
narrative_ontology:measurement(war__tr_t2013, war_powers_allocation__inherent_executive_reading, theater_ratio, 2013, 0.38).
narrative_ontology:measurement_basis(war__tr_t2013, observed).
narrative_ontology:measurement(war__tr_t2026, war_powers_allocation__inherent_executive_reading, theater_ratio, 2026, 0.38).
narrative_ontology:measurement_basis(war__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1945, 0.38).
narrative_ontology:measurement_basis(war__be_t1945, observed).
narrative_ontology:measurement(war__be_t1964, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1964, 0.52).
narrative_ontology:measurement_basis(war__be_t1964, observed).
narrative_ontology:measurement(war__be_t1980, war_powers_allocation__inherent_executive_reading, base_extractiveness, 1980, 0.61).
narrative_ontology:measurement_basis(war__be_t1980, observed).
narrative_ontology:measurement(war__be_t2001, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement_basis(war__be_t2001, observed).
narrative_ontology:measurement(war__be_t2013, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2013, 0.68).
narrative_ontology:measurement_basis(war__be_t2013, observed).
narrative_ontology:measurement(war__be_t2026, war_powers_allocation__inherent_executive_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(war__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1945, 0.31).
narrative_ontology:measurement_basis(war__su_t1945, observed).
narrative_ontology:measurement(war__su_t1964, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1964, 0.38).
narrative_ontology:measurement_basis(war__su_t1964, observed).
narrative_ontology:measurement(war__su_t1980, war_powers_allocation__inherent_executive_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement_basis(war__su_t1980, observed).
narrative_ontology:measurement(war__su_t2001, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement_basis(war__su_t2001, observed).
narrative_ontology:measurement(war__su_t2013, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2013, 0.42).
narrative_ontology:measurement_basis(war__su_t2013, observed).
narrative_ontology:measurement(war__su_t2026, war_powers_allocation__inherent_executive_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(war__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(war_powers_allocation__inherent_executive_reading, 0.12).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, war_powers_allocation__functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested war-powers-allocation kernel. It instantiates the inherent-executive interpretation: the president's Commander-in-Chief power grants unilateral authority to deploy force without prior congressional authorization. The sibling readings (congressional_primacy and functional_accommodation) instantiate different interpretations of the same constitutional texts and are structured as separate constraints. The three readings are networked via mutual affects_constraints declarations; each reading's omegas document the alternative readings and what would change if they displaced this one. The kernel itself is fixed (the constitutional texts), but the three readings assign different beneficiaries, different victims, and different metrics from those texts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_powers_allocation__inherent_executive_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
