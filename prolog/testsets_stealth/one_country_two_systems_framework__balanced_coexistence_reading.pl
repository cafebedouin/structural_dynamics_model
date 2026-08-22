% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__balanced_coexistence_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems — Balanced Coexistence Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The One Country, Two Systems settlement binds a socialist sovereign state
 *   and a common-law capitalist territory into a single constitutional order
 *   with a negotiated division of function. This story authors the
 *   balanced_coexistence_reading of that settlement: a regime in which
 *   neither sovereignty nor autonomy is absolute, boundary disputes are
 *   settled politically rather than by legal supremacy, and each crisis round
 *   re-prices the division. The reading predicts a medium-extraction regime
 *   with periodic renegotiation, and the temporal record matches: extraction
 *   dips where civil-society leverage wins (2003) and ratchets where the
 *   center prevails (2014, 2020-22). Family note: the colloquial label
 *   decomposes into three structurally distinct claims (see
 *   network.dual_formulation_note); this file is the middle reading, linked
 *   to both siblings. The epsilon referent is the standing settlement as this
 *   reading assesses it — not the arrangement any sibling reading would
 *   install. KEY AGENTS (by structural relationship): -
 *   prc_central_authorities: primary agenda-setter (institutional power,
 *   arbitrage-grade exit via interpretive control) — collects the
 *   sovereignty-side gains of every settlement -
 *   hongkong_business_establishment: primary beneficiary (powerful, arbitrage
 *   exit) — collects stability and market-access gains -
 *   prodemocracy_political_actors: primary target (organized, trapped) —
 *   their demands are the currency settled away in each crisis round -
 *   hongkong_civil_liberties_advocates: secondary target (moderate, trapped)
 *   — protected civic space narrows at each settlement - hongkong_judiciary:
 *   boundary administrator drifting toward target (institutional,
 *   identity_locked) — adjudicates the boundary and absorbs each override -
 *   hongkong_general_residents: diffuse dual-positioned public (powerless,
 *   constrained) — receive coordination goods, bear settlement costs -
 *   united_kingdom_treaty_cosignatory: excluded guarantor (institutional,
 *   mobile) — monitors without a seat - comparative_constitutional_scholars:
 *   analytical observer — sees the full structure from outside
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.62).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.68).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems — Balanced Coexistence Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, '228f7aca-b296-4236-a11f-255535f3e35d').
narrative_ontology:cs_kernel_codification('228f7aca-b296-4236-a11f-255535f3e35d', fixed_text).
narrative_ontology:cs_authority_grounding('228f7aca-b296-4236-a11f-255535f3e35d', practice).
narrative_ontology:cs_interpretation_layer_present('228f7aca-b296-4236-a11f-255535f3e35d').
narrative_ontology:cs_reading_relation('228f7aca-b296-4236-a11f-255535f3e35d', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('228f7aca-b296-4236-a11f-255535f3e35d', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('228f7aca-b296-4236-a11f-255535f3e35d', foundational, neither_sovereignty_nor_autonomy_absolute).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('228f7aca-b296-4236-a11f-255535f3e35d', neither_sovereignty_nor_autonomy_absolute, conventional).
narrative_ontology:cs_axiom('228f7aca-b296-4236-a11f-255535f3e35d', foundational, boundary_disputes_resolved_by_accommodation_not_supremacy).
narrative_ontology:cs_axiom_status(boundary_disputes_resolved_by_accommodation_not_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('228f7aca-b296-4236-a11f-255535f3e35d', boundary_disputes_resolved_by_accommodation_not_supremacy, instrumental).
narrative_ontology:cs_reference_frame('228f7aca-b296-4236-a11f-255535f3e35d', joint_declaration_negotiated_balance).
narrative_ontology:cs_drift_state('228f7aca-b296-4236-a11f-255535f3e35d', contemporary_post_nsl_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('228f7aca-b296-4236-a11f-255535f3e35d', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authorities).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_business_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, prodemocracy_political_actors).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_civil_liberties_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_general_residents).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hksar_government).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_general_residents).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds sovereign authority over Hong Kong and sets the terms under which boundary questions are reopened, chiefly through standing-committee interpretations of the Basic Law and policy directives to the territory. Gains continued territorial control alongside working access to Hong Kong's courts, currency, and markets. Bears restraint costs when accommodation requires leaving local arrangements untouched, and reputational costs abroad when settlements draw criticism.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authorities, agenda_setter,
    institutional, civilizational, arbitrage, continental).

% Administers the territory day to day under the Basic Law and carries central directives into local law. Its leadership is selected through a vetted committee process rather than universal suffrage. Loses local trust when read as transmitting central demands, loses central confidence when read as indulging local ones; carries both sides' dissatisfaction away from each settlement.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hksar_government, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hksar_government, payer).

% Holds the arrangement's most liquid position: common-law courts, an open capital account, and preferential access to mainland markets all depend on the dual-system settlement continuing. Assets and residencies abroad give it credible walk-away capacity, which it converts into advisory influence through the election committee and consultative bodies. Publicly defends the settlement while quietly maintaining exits.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_business_establishment, beneficiary,
    powerful, generational, arbitrage, global).

% Contest district councils and the legislature under electoral rules the center can revise. Their platform demands — universal suffrage timetables, independent inquiries, releases for detained protesters — are the items settled away in each crisis round. Since 2020 many sit disqualified, prosecuted, or in exile; organizational space has narrowed toward closure.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prodemocracy_political_actors, payer,
    organized, biographical, trapped, regional).

% Run the press outlets, unions, churches, and NGOs whose operating space the settlement nominally protects. Each crisis round ends with less protected space than the last: security-law offenses, society deregistrations, newsroom closures. Leaving means abandoning the constituencies they serve, so most stay and absorb the narrowing.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_civil_liberties_advocates, payer,
    moderate, biographical, trapped, regional).

% Receive the settlement's everyday goods — functioning courts, an open economy, visa-free travel — and bear its diffuse costs: housing priced by capital inflows, political voice exercisable mainly through occasional mass marches, and emigration as the costly exit of last resort. Their preferences reach the negotiating table only filtered through mobilization.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_general_residents, payer,
    powerless, immediate, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_general_residents, beneficiary).

% Operates the common law courts that adjudicate Basic Law disputes. Judges are recruited from and socialized into the common law world; professional identity is bound up with independence from direction. Final interpretation authority sits elsewhere, so every boundary ruling risks being overridden, and since 2020 designated judges hear security cases without juries. The bench accommodates while recording its reservations in reasoning.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary, payer).

% Co-signed the 1984 Joint Declaration and publishes six-monthly monitoring reports. It holds no seat inside the arrangement: its instruments are diplomatic protest, coordinated statements, and visa schemes for departing residents. Were it admitted to the conversation it would condition recognition on verified treaty performance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, united_kingdom_treaty_cosignatory, excluded,
    institutional, generational, mobile, global).

% Track the settlement against federal, associated-state, and treaty-succession models worldwide, documenting the widening gap between the texts' guarantees and operational practice. Publishes the analyses other seats cite; holds no position inside the arrangement itself.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authorities).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__balanced_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the interface between two incompatible systems — common law versus socialist legality, capitalist markets versus state-directed economy — inside a single sovereign state, so that each side's core functions operate without requiring the other to convert.
% TRANSFER_FUNCTION: Moves political concession upward: in each boundary crisis, autonomy-side claims (suffrage timetables, investigative mandates, press space) are the currency settled to preserve the framework. Moves systemic tolerance, market access, and infrastructural privilege downward to the territory.
% ABSENT_VOICES: The UK as treaty co-signatory holds monitoring duties but no seat; ordinary residents' preferences enter only through episodic mass mobilization rather than any standing institutional channel; Taiwan and the international financial community observe but cannot speak inside the arrangement.
% DISAPPEARANCE_RATIONALE: Overnight removal would force an immediate choice between full integration and open constitutional rupture: capital flight, emergency legislation, and either the absorption of the territory's legal system or an ungovernable enclave — every institution named in this story reorganizes around whichever successor rule emerges.
% FOUNDING_PROBLEM: Reintegrate a treaty-acquired capitalist enclave into a socialist sovereign state without destroying the enclave's economic function or triggering capital flight and constitutional conflict.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the UK Foreign Office's six-monthly reports attest the settlement's continuing operation and recurring strain; UN treaty-body reviews of ICCPR application to the territory document unresolved boundary disputes; comparative-law literature across jurisdictions treats the dual-system management problem as unsolved. No attesting source can compel performance — corroboration without an enforcement seat.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__balanced_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.62: the settlement delivers real interface coordination (parallel legal systems, open capital account, managed border) while every major boundary crisis since 2003 has ended with autonomy-side claims conceding ground — the 2003 security-legislation withdrawal being the clearest exception proving bargaining was once real. Suppression 0.68 measures enforcement build-up rather than everyday coercion: a dedicated security statute, a new enforcement organ, oath vetting, and newsroom closures constitute machinery that did not exist at handover. Theater 0.36 tracks the fate of consultation: responsive in the early years, increasingly predetermined after 2014. Accessibility_collapse 0.45: exits exist (emigration, capital mobility, international pressure, litigation) but none reaches the boundary question itself, which only the center can reopen. Resistance 0.60 is high for a constitutional arrangement — half-million-strong marches in 2003, seventy days of occupation in 2014, the 2019 uprising — though post-2020 enforcement has raised the cost of visible resistance. The three series share one eight-point grid (1997-2026) and trace the crisis cycle this reading predicts: tension accumulates, a crisis forces settlement, extraction steps up or dips depending on who wins, calm follows, accumulation resumes. The 2003 dip and the 2014/2022 ratchet are the cycle's visible hinges; the oscillation is not noise but the renegotiation mechanism itself, and the post-2022 flattening records consolidation rather than resolution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats compute a coordination-first picture: from the center and the territorial government, the settlement is an achievement that has kept two incompatible systems running inside one sovereignty for a generation. The payer seats compute extraction-first: from the democratic and civic seats, each negotiation is a round in which their claims are the item sold. The judiciary seat computes something between — an institution whose professional identity depends on an independence it cannot finally secure. Same texts, same institutions, divergent computed types per seat; the divergence is the data, and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map the center and the business establishment toward the subsidized end: the settlement preserves sovereignty and systemic privilege for both. Victim declarations map the democratic and civic seats toward the target end: they supply the concessions each settlement transfers. Residents carry paired declarations and derive an intermediate position — genuine coordination goods received, diffuse settlement costs borne. The judiciary mixes agenda-setting with cost-bearing, placing it mid-range, with identity lock amplifying its exposure because exit would dissolve the professional self the bench is built on. The UK seat is excluded rather than coordinated: outside the boundary conversation entirely, its monitoring alters nothing inside. No directionality overrides were needed: the beneficiary/victim declarations plus the exit-option spread already differentiate the seats, and the residents' intermediate position follows from their paired declarations rather than from any correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reintegrating a capitalist enclave without destroying its economic function — remains live, so nothing here is resolved mandatrophy. The tangled-rope classification prevents misreading in both directions: calling this pure extraction would erase the genuine interface coordination all seats still consume (courts, markets, border management); calling it pure coordination would erase the repeated pattern of settlements resolving toward the center at autonomy-side expense. Theater is rising but the coordinating function has not atrophied — a degraded-inertial verdict would require the negotiation mechanism to die while its forms persist, and the 2019-2020 round contradicts that: outcomes still move when the center decides to move them. Watch-item: if a future round produces no autonomy-side gain at all and consultation becomes wholly ceremonial, theater crosses the atrophy threshold and the classification should be revisited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the one_country_two_systems_framework kernel; which reading''s structure does operative practice actually instantiate?',
    'Observe crisis-resolution patterns across coming renegotiation rounds: settlements resolving by mutual concession instantiate this reading; settlements resolving by unilateral central decision instantiate sovereignty_primacy_reading; externally enforced guarantees would instantiate autonomy_primacy_reading.',
    'If sovereignty_primacy becomes operative practice, effective extraction on autonomy-bearing seats rises sharply and this reading''s medium-epsilon profile is revealed as transitional; if autonomy_primacy acquires enforcement, extraction falls toward coordination cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which sibling reading of the kernel matches operative practice.').

omega_variable(
    accommodation_symmetry_question,
    'Is the political accommodation this reading names substantively bilateral, or does the underlying power asymmetry reduce it to ratification of decisions already made?',
    'Code each historical boundary dispute (right of abode 1999, security legislation 2003, electoral reform 2014-15, extradition bill 2019, security law 2020) for whether autonomy-side inputs altered outcomes or merely timed them.',
    'Genuine bilateralism keeps epsilon at the authored medium level; a ratification pattern pushes epsilon toward the pure-extraction boundary and recasts this reading as cover for the sovereignty_primacy structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_symmetry_question, empirical, 'Whether the negotiation mechanism is substantive or ceremonial.').

omega_variable(
    civil_society_leverage_durability,
    'Do civil society and the business establishment retain real bargaining power through economic and international leverage, as this reading''s structure assumes?',
    'Test at the next crisis cycle: do mass mobilization, capital signals, and coordinated international response still move settlement terms, or has the post-2020 enforcement apparatus decoupled outcomes from mobilization?',
    'If leverage is exhausted, the reading''s coordination claim weakens, measured resistance falls in future series, and the arrangement drifts toward extraction maintained by enforcement alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_leverage_durability, empirical, 'Durability of the bargaining-power premise under hardened enforcement.').

omega_variable(
    implicit_sunset_2047,
    'Does the Basic Law''s fifty-year term make this arrangement transitional by design rather than steady-state coexistence?',
    'Observe the pre-2047 renegotiation: a renewed dual-system settlement confirms steady-state character; lapse into a single-system arrangement converts the structure to the sovereignty_primacy form; open-ended extension without renewal indicates inertial persistence of a lapsed mandate.',
    'A renewed settlement validates this reading''s framing; a lapse reclassifies the arrangement into the sibling structure; extension without renewal pushes toward degraded inertial maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implicit_sunset_2047, conceptual, 'Transitional versus steady-state character of the fifty-year horizon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 1997, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(octs_balanced_tr_t1997, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 1997, 0.14).
narrative_ontology:measurement_basis(octs_balanced_tr_t1997, observed).
narrative_ontology:measurement(octs_balanced_tr_t2001, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement_basis(octs_balanced_tr_t2001, observed).
narrative_ontology:measurement(octs_balanced_tr_t2003, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2003, 0.13).
narrative_ontology:measurement_basis(octs_balanced_tr_t2003, observed).
narrative_ontology:measurement(octs_balanced_tr_t2008, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2008, 0.19).
narrative_ontology:measurement_basis(octs_balanced_tr_t2008, observed).
narrative_ontology:measurement(octs_balanced_tr_t2014, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement_basis(octs_balanced_tr_t2014, observed).
narrative_ontology:measurement(octs_balanced_tr_t2019, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2019, 0.3).
narrative_ontology:measurement_basis(octs_balanced_tr_t2019, observed).
narrative_ontology:measurement(octs_balanced_tr_t2022, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2022, 0.37).
narrative_ontology:measurement_basis(octs_balanced_tr_t2022, observed).
narrative_ontology:measurement(octs_balanced_tr_t2026, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 2026, 0.36).
narrative_ontology:measurement_basis(octs_balanced_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(octs_balanced_be_t1997, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 1997, 0.36).
narrative_ontology:measurement_basis(octs_balanced_be_t1997, observed).
narrative_ontology:measurement(octs_balanced_be_t2001, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2001, 0.34).
narrative_ontology:measurement_basis(octs_balanced_be_t2001, observed).
narrative_ontology:measurement(octs_balanced_be_t2003, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2003, 0.33).
narrative_ontology:measurement_basis(octs_balanced_be_t2003, observed).
narrative_ontology:measurement(octs_balanced_be_t2008, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2008, 0.41).
narrative_ontology:measurement_basis(octs_balanced_be_t2008, observed).
narrative_ontology:measurement(octs_balanced_be_t2014, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2014, 0.52).
narrative_ontology:measurement_basis(octs_balanced_be_t2014, observed).
narrative_ontology:measurement(octs_balanced_be_t2019, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement_basis(octs_balanced_be_t2019, observed).
narrative_ontology:measurement(octs_balanced_be_t2022, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement_basis(octs_balanced_be_t2022, observed).
narrative_ontology:measurement(octs_balanced_be_t2026, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 2026, 0.62).
narrative_ontology:measurement_basis(octs_balanced_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(octs_balanced_su_t1997, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 1997, 0.28).
narrative_ontology:measurement_basis(octs_balanced_su_t1997, observed).
narrative_ontology:measurement(octs_balanced_su_t2001, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2001, 0.29).
narrative_ontology:measurement_basis(octs_balanced_su_t2001, observed).
narrative_ontology:measurement(octs_balanced_su_t2003, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2003, 0.27).
narrative_ontology:measurement_basis(octs_balanced_su_t2003, observed).
narrative_ontology:measurement(octs_balanced_su_t2008, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2008, 0.34).
narrative_ontology:measurement_basis(octs_balanced_su_t2008, observed).
narrative_ontology:measurement(octs_balanced_su_t2014, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2014, 0.46).
narrative_ontology:measurement_basis(octs_balanced_su_t2014, observed).
narrative_ontology:measurement(octs_balanced_su_t2019, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2019, 0.56).
narrative_ontology:measurement_basis(octs_balanced_su_t2019, observed).
narrative_ontology:measurement(octs_balanced_su_t2022, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2022, 0.71).
narrative_ontology:measurement_basis(octs_balanced_su_t2022, observed).
narrative_ontology:measurement(octs_balanced_su_t2026, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 2026, 0.68).
narrative_ontology:measurement_basis(octs_balanced_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'One Country, Two Systems' covers three structurally distinct claims about where boundary-resolution authority sits. This file instantiates the balanced_coexistence_reading (medium epsilon, accommodation-resolved). The upstream texts (Joint Declaration, Basic Law) are cited as evidence by all three readings; sovereignty_primacy_reading derives higher effective extraction on autonomy claims (delegation model), autonomy_primacy_reading derives near-zero tolerated extraction (guarantee model). Linked per the epsilon-invariance decomposition rule: one label, three constraints, three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
