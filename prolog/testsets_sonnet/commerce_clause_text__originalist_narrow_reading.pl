% ============================================================================
% CONSTRAINT STORY: commerce_clause_text__originalist_narrow_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commerce_clause_text__originalist_narrow_reading, []).

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
 *   constraint_id: commerce_clause_text__originalist_narrow_reading
 *   human_readable: Commerce Clause — Originalist Narrow (Border-Crossing) Reading
 *   domain: constitutional_law/federalism
 *
 * SUMMARY:
 *   This constraint instantiates the originalist narrow reading of the
 *   Commerce Clause kernel: federal authority under the Commerce Clause
 *   reaches only transactions that literally cross state lines and the
 *   instrumentalities (channels, vehicles, persons) engaged in that crossing,
 *   not intrastate activity regardless of its aggregate economic effects.
 *   This reading was doctrinally dominant before 1937, was substantially
 *   eclipsed during the New Deal / Wickard era (1937-1995), and has been
 *   partially revived since United States v. Lopez (1995) and United States
 *   v. Morrison (2000), which reintroduced meaningful limits on the
 *   substantial-effects doctrine without fully returning to the
 *   border-crossing standard. This story models the narrow reading as a
 *   discrete constraint with its own beneficiary/victim structure and its own
 *   extraction profile — it is NOT the same constraint as the
 *   expansive_federal_reading or the substantial_effects_limited_reading
 *   siblings, which have different ε values, different beneficiaries, and
 *   different victims. Per Rule 1, this story does not describe the contest
 *   between readings; it models only the structural consequences of this
 *   reading holding.
 *
 * KEY AGENTS:
 *   - state_governments: beneficiary/agenda_setter (institutional/arbitrage) — gain regulatory autonomy and litigate to preserve the boundary
 *   - intrastate_industry_incumbents: beneficiary (powerful/mobile) — escape federal standards by framing operations as intrastate
 *   - cross_border_pollution_victims: payer (powerless/trapped) — bear externalities the doctrine places outside federal reach
 *   - low_wage_intrastate_workers: payer (powerless/constrained) — lose federal labor protections applicable under broader readings
 *   - constitutional_historians: observer (analytical) — assess original-meaning fit independent of outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commerce_clause_text__originalist_narrow_reading, 0.42).
domain_priors:suppression_score(commerce_clause_text__originalist_narrow_reading, 0.38).
domain_priors:theater_ratio(commerce_clause_text__originalist_narrow_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(commerce_clause_text__originalist_narrow_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commerce_clause_text__originalist_narrow_reading, tangled_rope).
narrative_ontology:human_readable(commerce_clause_text__originalist_narrow_reading, "Commerce Clause — Originalist Narrow (Border-Crossing) Reading").
narrative_ontology:topic_domain(commerce_clause_text__originalist_narrow_reading, "constitutional_law/federalism").

domain_priors:requires_active_enforcement(commerce_clause_text__originalist_narrow_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commerce_clause_text__originalist_narrow_reading, '88a7536e-9854-4a35-bd5d-731271a09a65').
narrative_ontology:cs_kernel_codification('88a7536e-9854-4a35-bd5d-731271a09a65', fixed_text).
narrative_ontology:cs_authority_grounding('88a7536e-9854-4a35-bd5d-731271a09a65', lineage).
narrative_ontology:cs_interpretation_layer_present('88a7536e-9854-4a35-bd5d-731271a09a65').
narrative_ontology:cs_reading_relation('88a7536e-9854-4a35-bd5d-731271a09a65', commerce_clause_text__expansive_federal_reading, forecloses).
narrative_ontology:cs_reading_relation('88a7536e-9854-4a35-bd5d-731271a09a65', commerce_clause_text__substantial_effects_limited_reading, influences).
narrative_ontology:cs_axiom('88a7536e-9854-4a35-bd5d-731271a09a65', foundational, commerce_power_confined_to_border_crossing_transactions).
narrative_ontology:cs_axiom_status(commerce_power_confined_to_border_crossing_transactions, holdable).
narrative_ontology:cs_axiom_grounding('88a7536e-9854-4a35-bd5d-731271a09a65', commerce_power_confined_to_border_crossing_transactions, conventional).
narrative_ontology:cs_axiom('88a7536e-9854-4a35-bd5d-731271a09a65', secondary, residual_police_power_belongs_exclusively_to_states).
narrative_ontology:cs_axiom_status(residual_police_power_belongs_exclusively_to_states, holdable).
narrative_ontology:cs_axiom_grounding('88a7536e-9854-4a35-bd5d-731271a09a65', residual_police_power_belongs_exclusively_to_states, deontological).
narrative_ontology:cs_reference_frame('88a7536e-9854-4a35-bd5d-731271a09a65', founding_era_border_crossing_trade_barriers).
narrative_ontology:cs_drift_state('88a7536e-9854-4a35-bd5d-731271a09a65', post_lopez_revival_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('88a7536e-9854-4a35-bd5d-731271a09a65', '').
narrative_ontology:cs_kernel_id(commerce_clause_text__originalist_narrow_reading, commerce_clause_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, state_governments).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates).
narrative_ontology:constraint_beneficiary(commerce_clause_text__originalist_narrow_reading, intrastate_industry_incumbents).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, cross_border_pollution_victims).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, low_wage_intrastate_workers).
narrative_ontology:constraint_victim(commerce_clause_text__originalist_narrow_reading, national_market_uniformity_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain police-power authority over labor, environmental, and business regulation within their borders under this reading, and litigate aggressively (via attorneys general and amicus coalitions) to keep federal statutes confined to literal border-crossing transactions. They gain regulatory autonomy and can compete for business by offering laxer standards than neighboring states.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, state_governments, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(commerce_clause_text__originalist_narrow_reading, state_governments, agenda_setter).

% Political and legal movements committed to a limited-national-government vision fund litigation and judicial appointments to entrench this reading. They collect ideological and institutional wins each time a federal statute is struck down as exceeding the border-crossing definition, independent of the statute's practical merits.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, anti_federal_consolidation_advocates, beneficiary,
    organized, civilizational, arbitrage, national).

% Businesses whose operations are framed as purely intrastate escape federal labor, environmental, or safety standards that would otherwise reach them under a broader reading. They can also forum-shop by incorporating or operating primarily in states with favorable police-power regimes, gaining a cost advantage over competitors constrained by federal rules.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, intrastate_industry_incumbents, beneficiary,
    powerful, biographical, mobile, regional).

% Communities downstream or downwind of intrastate industrial activity bear externalities (pollution, contaminated water, degraded air) that this reading treats as outside federal commerce authority because the harmful activity itself does not cross a state line, even though its effects do. They have no exit — the harm arrives at their homes regardless of their preferences, and no federal mechanism reaches the source under this reading.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, cross_border_pollution_victims, payer,
    powerless, biographical, trapped, regional).

% Workers in industries characterized as intrastate lose access to federal minimum-wage, workplace-safety, and collective-bargaining protections that a broader reading would extend to them. Their only recourse is relocating to a state with stronger protections, which requires resources many do not have, or organizing for state-level reform against incumbents with a structural incentive to resist it.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, low_wage_intrastate_workers, payer,
    powerless, biographical, constrained, regional).

% Multi-state businesses, consumer-protection organizations, and national trade associations that want single federal standards instead of fifty divergent state regimes bear compliance costs from regulatory fragmentation. They can lobby Congress for narrowly-drawn border-crossing statutes that satisfy this reading, but cannot obtain the uniform national floor a broader commerce power would permit.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, national_market_uniformity_seekers, payer,
    organized, generational, constrained, national).

% Agencies such as environmental and labor regulators would extend oversight to intrastate activity with interstate spillovers under a broader reading, but this reading confines their jurisdictional hook to literal border-crossing transactions and instrumentalities of movement. They are not parties to the litigation that sets the doctrine and must operate within whatever boundary the courts draw.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, federal_regulatory_agencies, excluded,
    institutional, generational, constrained, national).

% Study the founding-era understanding of 'commerce among the several states,' the drafting history, and ratification debates to assess whether the border-crossing reading matches original public meaning, without a direct stake in the outcome.
narrative_ontology:constraint_stakeholder(commerce_clause_text__originalist_narrow_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(commerce_clause_text__originalist_narrow_reading, diffuse).
narrative_ontology:fixing_cost_class(commerce_clause_text__originalist_narrow_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a clear, judicially administrable line between federal and state regulatory authority, giving states predictable domains of exclusive police power and preventing open-ended expansion of federal jurisdiction into any activity with some downstream economic effect.
% TRANSFER_FUNCTION: Moves regulatory authority and its associated costs from the federal government to the states, and moves the cost of unregulated externalities and substandard labor conditions from intrastate industry onto workers and neighboring communities who have no seat in the state's political process that authorizes the activity.
% ABSENT_VOICES: Cross-border pollution victims and low-wage workers in industries deemed intrastate would object that harms and effects do not respect the line this reading draws, but they are not parties before the courts that adjudicate the doctrine — the litigants are typically states, industry groups, and the federal government, not the diffuse populations bearing the externalities.
% DISAPPEARANCE_RATIONALE: If this narrow reading were abandoned entirely (rather than merely one contested reading among several), the jurisdictional boundary between state police power and federal commerce authority would shift substantially toward federal reach, state regulatory autonomy over nominally intrastate activity would contract, and litigation currently won by states and intrastate incumbents under this doctrine would instead be lost — a real reallocation of governing authority, not a cosmetic one.
% FOUNDING_PROBLEM: The founding-era problem was interstate trade barriers and retaliatory tariffs among states under the Articles of Confederation, which the Commerce Clause was drafted to eliminate by giving Congress power over commerce 'among the several states.' The narrow reading holds that this original problem was specifically about barriers to cross-border trade, not a general grant of power over economic activity with indirect effects.
% FOUNDING_PROBLEM_CORROBORATION: Originalist legal scholars and some economic historians attest the founding problem was narrowly about interstate trade barriers, supporting this reading. Legal historians outside the originalist movement, and the bulk of post-1937 Supreme Court jurisprudence itself, attest the founding generation also intended commerce power to reach activities substantially affecting interstate trade even without a literal border crossing — the corroboration is genuinely split along the same lines as the underlying doctrinal dispute, not resolved by any source outside the contest.
narrative_ontology:disappearance_verdict(commerce_clause_text__originalist_narrow_reading, world_rearranges).
narrative_ontology:founding_problem_status(commerce_clause_text__originalist_narrow_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(commerce_clause_text__originalist_narrow_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(commerce_clause_text__originalist_narrow_reading, 'none', 1).
narrative_ontology:epsilon_provenance(commerce_clause_text__originalist_narrow_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commerce_clause_text__originalist_narrow_reading_tests).
:- end_tests(commerce_clause_text__originalist_narrow_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects that the doctrine does real coordination work — a clear jurisdictional line reduces litigation uncertainty and preserves a genuine zone of state autonomy — but that coordination function rides alongside a real transfer: externalities and substandard labor conditions in nominally intrastate activity are pushed onto powerless populations who have no federal recourse and often no effective state-level recourse either, because the same incumbents that benefit from the doctrine typically have outsized influence in state politics. Suppression (0.38) is moderate: the doctrine's persistence has depended on continuous, contested judicial enforcement (Lopez, Morrison, and their progeny required active doctrinal maintenance against the substantial-effects trend), not on settled consensus. Theater ratio (0.22) is modest because the doctrinal line-drawing is a real constraint on litigated outcomes, not merely performative — cases are actually won and lost on this ground. Resistance (0.68) is high because this reading is perpetually contested in the courts and academy, unlike a settled mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and anti-federal-consolidation advocates sit near the beneficiary end: they collect autonomy, ideological wins, and regulatory competition advantage from the border-crossing line, and can exit into litigation or legislative advocacy to defend it (arbitrage). Intrastate industry incumbents are also beneficiaries with mobile exit — they can relocate operations to favorable jurisdictions. Cross-border pollution victims and low-wage workers are targets: trapped or constrained exit, no political voice in the jurisdiction whose choices harm them (pollution victims), or limited relocation resources (workers). National market uniformity seekers are organized but constrained — they can lobby but cannot obtain the outcome a broader reading would deliver directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored as contested rather than dead: the original problem (interstate trade barriers) genuinely no longer resembles the modern economy's interdependence, which argues for mandatrophy under this reading's own terms — a doctrine built to stop 1780s tariff wars now operates in an economy where 'purely intrastate' activity routinely has substantial interstate spillovers. But the doctrine is not merely inertial: state governments and industry incumbents actively benefit from and defend it, which is why this story classifies as tangled_rope rather than piton — a piton has no concentrated beneficiary, and this constraint plainly does. The coordination function (predictable federal/state boundary) is real and distinct from the extraction (externalized costs onto powerless payers), which is exactly the tangled_rope signature: both functions must be present, and both are.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    originalist_meaning_of_commerce_among_the_states,
    'Did the founding generation understand ''commerce among the several states'' to mean literally cross-border trade transactions, or did it also encompass activity with substantial interstate economic effects, as some founding-era usage and post-ratification practice suggests?',
    'Corpus linguistics analysis of founding-era usage of ''commerce,'' comparison with state ratification debates, and examination of early congressional commerce legislation (e.g., embargo acts, early transportation regulation) for evidence of whether Congress itself understood its power as border-crossing-only.',
    'If founding-era usage was broader than literal border-crossing, this reading''s core originalist claim weakens and the substantial_effects_limited_reading gains textual/historical support; if usage was genuinely narrow, this reading''s foundational premise strengthens against both siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_meaning_of_commerce_among_the_states, empirical, 'Whether founding-era usage of ''commerce among the several states'' supports the border-crossing-only reading.').

omega_variable(
    which_reading_is_the_disagreement_located_in,
    'Where exactly do the three sibling readings disagree — is it about the meaning of ''commerce,'' the meaning of ''among the several states,'' or the proper judicial test for jurisdictional nexus?',
    'Doctrinal decomposition of the disagreement into its component textual and structural elements, mapping each sibling reading''s position on each element.',
    'Locating the disagreement precisely determines whether the readings are genuinely incompatible (forecloses) or capable of coexisting as different judicial tests applied in different eras or contexts (coexists_with/influences) — this shapes the reading_relations declared in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(which_reading_is_the_disagreement_located_in, conceptual, 'Precise location of the interpretive disagreement among the three kernel readings.').

omega_variable(
    state_capture_of_intrastate_police_power,
    'When state governments exercise the police power this reading preserves, are they genuinely representing constituents'' interests, or are they substantially captured by the same intrastate industry incumbents who benefit from federal non-reach?',
    'Empirical study of state regulatory outcomes in industries characterized as intrastate, comparing regulatory stringency to industry campaign contributions and lobbying expenditure at the state level, and comparing outcomes for affected communities with and without effective political voice.',
    'If state police power is substantially captured, the doctrine''s claimed coordination benefit (state autonomy serving local preferences) is largely illusory and the constraint functions closer to pure extraction (snare) than tangled_rope; if state regulation is genuinely responsive, the coordination function is more substantial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capture_of_intrastate_police_power, empirical, 'Whether state police power under this reading serves genuine local preferences or is captured by the same beneficiaries the reading protects from federal reach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commerce_clause_text__originalist_narrow_reading, 1789, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1789, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement_basis(comm_tr_t1789, observed).
narrative_ontology:measurement(comm_tr_t1900, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1900, 0.15).
narrative_ontology:measurement_basis(comm_tr_t1900, observed).
narrative_ontology:measurement(comm_tr_t1937, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1937, 0.3).
narrative_ontology:measurement_basis(comm_tr_t1937, observed).
narrative_ontology:measurement(comm_tr_t1995, commerce_clause_text__originalist_narrow_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement_basis(comm_tr_t1995, observed).
narrative_ontology:measurement(comm_tr_t2012, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2012, 0.2).
narrative_ontology:measurement_basis(comm_tr_t2012, observed).
narrative_ontology:measurement(comm_tr_t2025, commerce_clause_text__originalist_narrow_reading, theater_ratio, 2025, 0.22).
narrative_ontology:measurement_basis(comm_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(comm_be_t1789, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1789, 0.2).
narrative_ontology:measurement_basis(comm_be_t1789, observed).
narrative_ontology:measurement(comm_be_t1900, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1900, 0.28).
narrative_ontology:measurement_basis(comm_be_t1900, observed).
narrative_ontology:measurement(comm_be_t1937, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1937, 0.15).
narrative_ontology:measurement_basis(comm_be_t1937, observed).
narrative_ontology:measurement(comm_be_t1995, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement_basis(comm_be_t1995, observed).
narrative_ontology:measurement(comm_be_t2012, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2012, 0.4).
narrative_ontology:measurement_basis(comm_be_t2012, observed).
narrative_ontology:measurement(comm_be_t2025, commerce_clause_text__originalist_narrow_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(comm_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1789, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1789, 0.25).
narrative_ontology:measurement_basis(comm_su_t1789, observed).
narrative_ontology:measurement(comm_su_t1900, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1900, 0.3).
narrative_ontology:measurement_basis(comm_su_t1900, observed).
narrative_ontology:measurement(comm_su_t1937, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1937, 0.1).
narrative_ontology:measurement_basis(comm_su_t1937, observed).
narrative_ontology:measurement(comm_su_t1995, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 1995, 0.32).
narrative_ontology:measurement_basis(comm_su_t1995, observed).
narrative_ontology:measurement(comm_su_t2012, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2012, 0.36).
narrative_ontology:measurement_basis(comm_su_t2012, observed).
narrative_ontology:measurement(comm_su_t2025, commerce_clause_text__originalist_narrow_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(comm_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commerce_clause_text__originalist_narrow_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(commerce_clause_text__originalist_narrow_reading, 0.12).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, expansive_federal_reading).
narrative_ontology:affects_constraint(commerce_clause_text__originalist_narrow_reading, substantial_effects_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the commerce_clause_text kernel. expansive_federal_reading claims federal commerce power reaches all economic activity with substantial aggregate national effects — its core premise is logically incompatible with this reading's border-crossing confinement (declared as forecloses: a court cannot simultaneously hold that intrastate non-economic activity is beyond federal reach and that aggregate effects alone suffice). substantial_effects_limited_reading occupies a middle position requiring jurisdictional nexus and non-pretextual regulation; this reading influences it by supplying the textual/historical pressure that motivated post-1995 doctrinal tightening (Lopez, Morrison) without fully foreclosing the substantial-effects framework, since that framework can incorporate nexus requirements without adopting literal border-crossing. Each sibling has its own ε, beneficiaries, and victims and should be read as a separate file, not as a parameter of this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
