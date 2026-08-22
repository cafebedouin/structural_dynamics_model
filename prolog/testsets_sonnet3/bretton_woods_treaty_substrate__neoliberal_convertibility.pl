% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__neoliberal_convertibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__neoliberal_convertibility, []).

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
 *   constraint_id: bretton_woods_treaty_substrate__neoliberal_convertibility
 *   human_readable: Bretton Woods as Convertibility Discipline on Government Intervention
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This story instantiates the neoliberal_convertibility reading of the
 *   Bretton Woods kernel: the treaty substrate is read as a mechanism whose
 *   real function is to discipline government intervention in favor of free
 *   capital mobility. Under this reading, national policy autonomy — capital
 *   controls, directed credit, counter-cyclical intervention — is the thing
 *   constrained (the victim set), while internationally mobile finance and
 *   creditor exporters are the beneficiaries of a norm that treats their
 *   freedom of movement as the baseline and any government's attempt to
 *   manage it as a deviation requiring justification. This is a distinct
 *   constraint from the sibling readings: the keynesian_embedded_liberalism
 *   reading treats the SAME treaty text as protecting domestic policy space
 *   FROM capital, and the sovereignty_defense reading treats it as protecting
 *   monetary sovereignty from external discipline. All three share a kernel
 *   (the Bretton Woods Articles of Agreement) but diverge on what is
 *   constrained and who benefits — per the ε-invariance principle these are
 *   authored as three separate constraint stories, linked by network edges,
 *   not as one story with a measurement parameter.
 *
 * KEY AGENTS:
 *   - international_finance_capital: primary beneficiary (institutional/arbitrage) — gains structural veto via exit threat
 *   - reserve_currency_issuer: agenda_setter (institutional/arbitrage) — administers norm, retains more discretion than it grants others
 *   - developing_state_governments: primary target (moderate/constrained) — policy tools delegitimized under conditionality
 *   - domestic_full_employment_constituencies: diffuse victim (powerless/trapped) — bears adjustment cost with no exit
 *   - monetary_sovereignty_advocates: excluded analytical/political voice arguing original text does not support this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.62).
domain_priors:suppression_score(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.58).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, extractiveness, 0.62).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__neoliberal_convertibility, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__neoliberal_convertibility, tangled_rope).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__neoliberal_convertibility, "Bretton Woods as Convertibility Discipline on Government Intervention").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__neoliberal_convertibility, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__neoliberal_convertibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__neoliberal_convertibility, '390df8e6-13df-4c04-b779-31e8210d3d85').
narrative_ontology:cs_kernel_codification('390df8e6-13df-4c04-b779-31e8210d3d85', formalized).
narrative_ontology:cs_authority_grounding('390df8e6-13df-4c04-b779-31e8210d3d85', extraction).
narrative_ontology:cs_interpretation_layer_present('390df8e6-13df-4c04-b779-31e8210d3d85').
narrative_ontology:cs_reading_relation('390df8e6-13df-4c04-b779-31e8210d3d85', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('390df8e6-13df-4c04-b779-31e8210d3d85', bretton_woods_treaty_substrate__sovereignty_defense, coexists_with).
narrative_ontology:cs_axiom('390df8e6-13df-4c04-b779-31e8210d3d85', foundational, capital_mobility_is_default_liberty).
narrative_ontology:cs_axiom_status(capital_mobility_is_default_liberty, holdable).
narrative_ontology:cs_axiom_grounding('390df8e6-13df-4c04-b779-31e8210d3d85', capital_mobility_is_default_liberty, conventional).
narrative_ontology:cs_axiom('390df8e6-13df-4c04-b779-31e8210d3d85', foundational, government_intervention_is_deviation_requiring_justification).
narrative_ontology:cs_axiom_status(government_intervention_is_deviation_requiring_justification, holdable).
narrative_ontology:cs_axiom_grounding('390df8e6-13df-4c04-b779-31e8210d3d85', government_intervention_is_deviation_requiring_justification, instrumental).
narrative_ontology:cs_reference_frame('390df8e6-13df-4c04-b779-31e8210d3d85', bretton_woods_1944_articles_of_agreement).
narrative_ontology:cs_drift_state('390df8e6-13df-4c04-b779-31e8210d3d85', post_1997_asian_financial_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('390df8e6-13df-4c04-b779-31e8210d3d85', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__neoliberal_convertibility, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, creditor_nation_exporters).
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_issuer).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_state_governments).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_full_employment_constituencies).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_control_administering_ministries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Private banks and later institutional investors gain a treaty-legitimated norm that currency convertibility and capital mobility are the baseline against which government intervention is judged a deviation. They move capital across borders seeking yield and can exit any single jurisdiction whose government tightens controls, disciplining policy from outside the ballot box.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, international_finance_capital, beneficiary,
    institutional, generational, arbitrage, global).

% Firms and states running persistent surpluses benefit from a convertibility regime that lets them accumulate reserves and invest abroad without symmetric adjustment pressure; they helped write and continue to defend the convertibility norms at the IMF and in bilateral surveillance.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, creditor_nation_exporters, beneficiary,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, creditor_nation_exporters, agenda_setter).

% The state whose currency anchors the system administers the convertibility norm through the IMF Articles of Agreement and bilateral leverage, treating capital account liberalization as the mark of a well-governed economy while retaining more room to run its own deficits than the norm nominally allows others.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, reserve_currency_issuer, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Face IMF Article VIII pressure and conditionality that treats capital controls, credit allocation, and directed industrial policy as distortions to be phased out. Attempting these tools invites capital flight, credit-rating penalties, and conditional-lending leverage; genuine exit from the convertibility norm risks isolation from trade finance and reserve access.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, developing_state_governments, payer,
    moderate, biographical, constrained, national).

% Workers and unions who benefited from postwar domestic policy autonomy (capital controls enabling counter-cyclical spending, wage bargaining insulated from currency speculation) find that governments increasingly defer to capital-mobility discipline over full-employment mandates, since intervention now risks a currency or balance-of-payments crisis engineered by mobile capital's exit.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, domestic_full_employment_constituencies, payer,
    powerless, biographical, trapped, national).

% Finance ministries and central banks that historically administered exchange controls under Bretton Woods' original Article VI find their toolkit steadily delegitimized; each control they retain is treated as a compliance exception requiring justification to the Fund rather than a sovereign prerogative.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_control_administering_ministries, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_control_administering_ministries, excluded).

% Administers Article IV consultations and conditionality frameworks that operationalize the convertibility norm, converting the treaty's original narrow current-account rules into an expansive brief for capital account liberalization across member states.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_surveillance_apparatus, agenda_setter,
    institutional, generational, analytical, global).

% Economists, sovereigntist politicians, and Global South delegations who argue the original Bretton Woods design (per Keynes and White's own current-account-only convertibility) never mandated capital account liberalization are largely absent from the surveillance and conditionality rooms where the expanded norm is applied.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__neoliberal_convertibility, monetary_sovereignty_advocates, excluded,
    moderate, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__neoliberal_convertibility, diffuse).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__neoliberal_convertibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared expectation that currencies will be convertible for current-account transactions and, in this reading's expansive application, that capital should move freely across borders — solving the genuine coordination problem of a world without a common medium for settling trade balances.
% TRANSFER_FUNCTION: Moves policy discretion from national governments (particularly over capital allocation, exchange controls, and counter-cyclical spending) to internationally mobile capital and the institutions administering convertibility surveillance; moves adjustment costs disproportionately onto weaker-currency states and domestic labor constituencies rather than surplus creditors.
% ABSENT_VOICES: Global South finance ministries and domestic labor constituencies who would object that the treaty's original text (Bretton Woods Articles, especially the current-account-only convertibility contemplated by Keynes) never authorized this expansive capital-mobility reading; they are structurally outside the IMF Executive Board's weighted-voting rooms where Article IV interpretation is set.
% DISAPPEARANCE_RATIONALE: If the convertibility discipline vanished overnight, states would rapidly reinstate capital controls, credit allocation, and exchange management as ordinary policy tools; international finance would lose its structural veto over domestic macroeconomic choices, and the balance of adjustment burden between surplus and deficit states would become an open political question rather than a settled compliance matter.
% FOUNDING_PROBLEM: The interwar collapse of the gold standard, competitive devaluations, and beggar-thy-neighbor exchange controls had fragmented international trade; a stable, rules-based currency convertibility regime was built to solve the coordination failure of uncoordinated national exchange policy.
% FOUNDING_PROBLEM_CORROBORATION: International finance and IMF officials attest the founding problem (currency chaos undermining trade) remains live and justifies continued convertibility discipline. Independent economic historians (outside both the finance and sovereignty-advocacy camps) and Global South monetary economists attest the original problem was narrowly about current-account convertibility and was long since solved by the 1960s; the subsequent extension to capital-account liberalization addresses a different, later-invented problem (attracting and retaining mobile investment) not present in the founding text.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__neoliberal_convertibility, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__neoliberal_convertibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__neoliberal_convertibility, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__neoliberal_convertibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__neoliberal_convertibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 (substantial, not extreme) because the coordination function — a shared convertibility standard solving real settlement-coordination problems — is genuine even under this critical reading; the extraction lies in the asymmetric distribution of adjustment burden and the delegitimization of tools once considered ordinary sovereign prerogatives. Suppression at 0.58 reflects that capital controls remain technically legal exceptions under IMF Article VI but are treated as compliance failures requiring justification — a real but not absolute suppression of alternatives. Theater ratio is moderate-low (0.28) because IMF surveillance does perform real monitoring functions alongside its disciplinary function. All three temporal metrics share the 1944-2024 grid; extractiveness and suppression both show accumulation after the 1971 Nixon shock and accelerate through the 1980s Washington Consensus era before plateauing post-2008, consistent with the historical record of capital account liberalization being formalized only gradually.
 *
 * DIRECTIONALITY LOGIC:
 *   International finance capital and creditor-nation exporters are structural beneficiaries: their mobility is the baseline the norm protects, and they can exit any jurisdiction that deviates, which the engine's directionality derivation should read as low d (near-beneficiary). Developing state governments and domestic full-employment constituencies are structural targets: their policy tools are constrained, their exit from the convertibility norm is costly (isolation from trade finance, credit downgrades), which the derivation should read as high d (near-target), especially for the powerless/trapped full-employment constituencies who have no institutional voice at all. The reserve currency issuer occupies an asymmetric position — it administers the norm but is not bound by it symmetrically, which is why it carries an override below.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (interwar currency chaos) was substantially solved by the late 1950s when current-account convertibility was restored among major currencies (1958 European convertibility). This reading holds that the subsequent expansion into capital-account liberalization addresses a DIFFERENT problem — attracting and retaining mobile capital — not present in the founding text, making the founding_problem_status contested rather than cleanly live or dead: finance sector actors treat the original justification as still active, while the mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges signals a live capture/extension dynamic worth flagging rather than a straightforward mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_text_scope_ambiguity,
    'Did the original 1944 Bretton Woods Articles of Agreement (which explicitly permitted capital controls under Article VI while mandating only current-account convertibility under Article VIII) ever license the capital-account liberalization this reading treats as the norm''s core content, or is that an extension grafted on during the 1980s-90s IMF Article IV reinterpretation?',
    'Textual and negotiating-history analysis of the 1944 Articles and subsequent amendments (particularly the failed 1997 attempt to formally amend the Articles to make capital account liberalization a purpose of the Fund, which was abandoned after the Asian Financial Crisis).',
    'If the extension is textually ungrounded, this reading describes a drift/capture dynamic (informal reinterpretation exceeding the founding mandate) rather than a straightforward application of the original design — strengthening the tangled_rope classification''s asymmetric-extraction component over a pure coordination story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_text_scope_ambiguity, conceptual, 'Whether capital-account liberalization was part of the original kernel or a later reinterpretation.').

omega_variable(
    reserve_issuer_symmetry_ambiguity,
    'Does the reserve currency issuer actually bear the same convertibility discipline it exports to other states, or does its exorbitant privilege (ability to run deficits financed by reserve demand for its own currency) exempt it from the adjustment pressure imposed on developing states?',
    'Comparative balance-of-payments adjustment analysis across reserve-issuing and non-reserve-issuing states facing similar deficit levels.',
    'If asymmetric, this substantially strengthens the case that the reserve issuer functions as an agenda_setter/beneficiary hybrid rather than a peer bound by its own rule — supporting the directionality override applied below.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reserve_issuer_symmetry_ambiguity, empirical, 'Whether the reserve currency issuer is bound by the convertibility discipline it administers for others.').

omega_variable(
    sibling_reading_kernel_disagreement_location,
    'Where exactly do the three kernel readings (neoliberal_convertibility, keynesian_embedded_liberalism, sovereignty_defense) locate their disagreement — is it about what the treaty text says, what its drafters intended, or what its subsequent institutional practice became?',
    'Comparative analysis of the three readings'' foundational axioms: this reading holds capital_mobility_is_default_liberty as foundational; the embedded_liberalism reading would hold domestic_policy_space_is_protected_interest; the sovereignty_defense reading would hold monetary_sovereignty_is_inviolable. The disagreement is located in which value the treaty substrate is read as protecting, not in disputed facts about the text.',
    'Clarifies that this is a genuine ε-invariance case requiring three separate stories (already implemented) rather than one story with a hidden observer parameter — confirms the decomposition was correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_disagreement_location, conceptual, 'Locating the structural disagreement among the three kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__neoliberal_convertibility, 1944, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bret_tr_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1944, 0.1).
narrative_ontology:measurement(bret_tr_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(bret_tr_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1971, 0.18).
narrative_ontology:measurement(bret_tr_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(bret_tr_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 1997, 0.26).
narrative_ontology:measurement(bret_tr_t2010, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2010, 0.27).
narrative_ontology:measurement(bret_tr_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(bret_be_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1944, 0.22).
narrative_ontology:measurement(bret_be_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1958, 0.3).
narrative_ontology:measurement(bret_be_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1971, 0.4).
narrative_ontology:measurement(bret_be_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(bret_be_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 1997, 0.6).
narrative_ontology:measurement(bret_be_t2010, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(bret_be_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(bret_su_t1944, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1944, 0.2).
narrative_ontology:measurement(bret_su_t1958, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1958, 0.28).
narrative_ontology:measurement(bret_su_t1971, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1971, 0.38).
narrative_ontology:measurement(bret_su_t1985, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(bret_su_t1997, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 1997, 0.55).
narrative_ontology:measurement(bret_su_t2010, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2010, 0.57).
narrative_ontology:measurement(bret_su_t2024, bretton_woods_treaty_substrate__neoliberal_convertibility, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__neoliberal_convertibility, resource_allocation).
narrative_ontology:boltzmann_floor_override(bretton_woods_treaty_substrate__neoliberal_convertibility, 0.12).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, imf_conditionality_regime).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, capital_account_liberalization_norm).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__neoliberal_convertibility, sovereignty_defense).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the bretton_woods_treaty_substrate kernel (network family). keynesian_embedded_liberalism reads the same treaty as protecting domestic policy space from capital (international finance is the constrained party, not the beneficiary — inverted victim/beneficiary structure from this story). sovereignty_defense reads it as protecting national monetary sovereignty from external discipline (the IMF/reserve-issuer apparatus is the constrained party). All three share the same treaty text as their kernel but diverge in beneficiary/victim structure, ε, and classification; they must not be merged into one story with an observable parameter per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bretton_woods_treaty_substrate__neoliberal_convertibility, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
