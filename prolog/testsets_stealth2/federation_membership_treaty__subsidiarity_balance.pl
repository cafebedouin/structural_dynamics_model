% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Free Movement Under Proportionality Balance (Subsidiarity Reading)
 *   domain: political economy/federalism/migration policy
 *
 * SUMMARY:
 *   Within the federation_membership_treaty kernel, this story instantiates
 *   the subsidiarity_balance reading: free movement is real and enforceable,
 *   but legitimate national interests — labor-market protection,
 *   welfare-system sustainability, public order — legitimately constrain it,
 *   calibrated through proportionality review. The colloquial label 'free
 *   movement in the federation' covers three structurally distinct claims;
 *   per the ε-invariance principle this file authors only the balance
 *   reading, with the integration_primary and sovereignty_primary siblings as
 *   separate linked constraints. The claim and the metrics are independent
 *   authored facts: the reading is claimed as tangled_rope because it
 *   structurally possesses both a genuine coordination function
 *   (bidirectional suppression of both extremes, standing adjudication,
 *   persistent alternatives) and asymmetric extraction (identifiable mover
 *   classes bear upheld restrictions while receiving states collect the
 *   savings), while the metric values are descriptive estimates of actual
 *   operation. Extraction concentrates in the welfare-access and
 *   transitional-control domains; core movement and work rights survive
 *   review in the large majority of cases.
 *
 * KEY AGENTS:
 *   - eu_court_of justice: adjudicative agenda-setter (institutional/constrained) — decides which restrictions survive proportionality review
 *   - european_commission: enforcement agenda-setter (institutional/constrained) — infringement proceedings and transitional-control timetables
 *   - receiving_state_governments: agenda-setting beneficiary (institutional/constrained) — authors restrictions, collects budget savings and labor-market shielding
 *   - sending_state_governments: ambivalent payer (institutional/constrained) — nationals restricted abroad, brain-drain relief at home
 *   - mobile_eu_citizens: net beneficiary (organized/constrained) — retains the enforceable mobility floor
 *   - accession_state_workers: primary target (moderate/constrained) — transitional controls deferred their earnings
 *   - low_income_mobile_claimants: concentrated target (powerless/trapped) — welfare-access denials end their moves entirely
 *   - domestic_labor_sectors: incidental beneficiary (organized/constrained) — shielded from labor competition where restrictions hold
 *   - third_country_residents: excluded voice (powerless/trapped) — outside the citizenship-based framework entirely
 *   - mobility_rights_advocates: analytical observer (analytical/analytical) — sees the full structure across cases and states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.46).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.52).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.46).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Free Movement Under Proportionality Balance (Subsidiarity Reading)").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political economy/federalism/migration policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '98f7bd2a-ec81-437d-a661-d88ff9a172a8').
narrative_ontology:cs_kernel_codification('98f7bd2a-ec81-437d-a661-d88ff9a172a8', fixed_text).
narrative_ontology:cs_authority_grounding('98f7bd2a-ec81-437d-a661-d88ff9a172a8', lineage).
narrative_ontology:cs_interpretation_layer_present('98f7bd2a-ec81-437d-a661-d88ff9a172a8').
narrative_ontology:cs_reading_relation('98f7bd2a-ec81-437d-a661-d88ff9a172a8', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('98f7bd2a-ec81-437d-a661-d88ff9a172a8', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('98f7bd2a-ec81-437d-a661-d88ff9a172a8', foundational, legitimate_national_interest_is_valid_restriction_ground).
narrative_ontology:cs_axiom_status(legitimate_national_interest_is_valid_restriction_ground, holdable).
narrative_ontology:cs_axiom_grounding('98f7bd2a-ec81-437d-a661-d88ff9a172a8', legitimate_national_interest_is_valid_restriction_ground, conventional).
narrative_ontology:cs_axiom('98f7bd2a-ec81-437d-a661-d88ff9a172a8', foundational, mobility_rights_constrainable_but_never_eliminated).
narrative_ontology:cs_axiom_status(mobility_rights_constrainable_but_never_eliminated, holdable).
narrative_ontology:cs_axiom_grounding('98f7bd2a-ec81-437d-a661-d88ff9a172a8', mobility_rights_constrainable_but_never_eliminated, deontological).
narrative_ontology:cs_reference_frame('98f7bd2a-ec81-437d-a661-d88ff9a172a8', proportionality_calibrated_mobility_regime).
narrative_ontology:cs_drift_state('98f7bd2a-ec81-437d-a661-d88ff9a172a8', post_enlargement_politicization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('98f7bd2a-ec81-437d-a661-d88ff9a172a8', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_eu_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, receiving_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, domestic_labor_sectors).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, accession_state_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, low_income_mobile_claimants).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, sending_state_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, sending_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, proportionality_principle).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, subsidiarity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates challenges to national restrictions on cross-border movement, deciding through proportionality review which restrictions stand and which fall. Its rulings define the operative content of the balance. It cannot decline jurisdiction or relocate its authority, and its standing depends on being seen to weigh both sides rather than defer automatically to either.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, eu_court_of_justice, agenda_setter,
    institutional, generational, constrained, continental).

% Monitors member-state implementation, opens infringement proceedings against restrictions it judges excessive, and administered the timetable of transitional labor-market controls after each enlargement. Its institutional weight grows with the reach of the framework it polices; its practical leverage depends on member states' willingness to comply.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, european_commission, agenda_setter,
    institutional, generational, constrained, continental).

% Author and defend national restrictions — transitional labor-market controls, welfare-access conditions, public-order exclusions — and argue their legitimacy before the court. Restrictions that survive review convert denied access into budget savings and shielded labor markets. Leaving the framework entirely means leaving the union, as one member demonstrated at severe cost.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, receiving_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, receiving_state_governments, beneficiary).

% Governments of newer member states whose nationals face transitional controls and welfare gating abroad. They formally objected to transitional clauses at accession but accepted them as the price of membership, and quietly gain when emigration slows and skills stay home. Their leverage inside the framework is limited by late entry and dependence on structural funds.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sending_state_governments, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, sending_state_governments, beneficiary).

% Hold treaty-guaranteed rights to move, work, and reside across member states, enforceable through courts they can access directly. They retain a floor of enforceable mobility even where specific claims fail; their practical reach depends on resources, language, and recognition of qualifications.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_eu_citizens, beneficiary,
    organized, biographical, constrained, continental).

% Workers from newly admitted member states who encountered temporary national controls on taking employment abroad for up to seven years after accession. Many relocated anyway through informal channels or third-country detours; those who waited bore years of deferred earnings and stalled career progression.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, accession_state_workers, payer,
    moderate, biographical, constrained, continental).

% Movers whose claims on host-state welfare systems are tested against residence and resource conditions that courts have upheld. With little income and no fallback, a rejected claim ends the planned move entirely; they have the least capacity to absorb denial and the fewest alternatives anywhere else.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, low_income_mobile_claimants, payer,
    powerless, immediate, trapped, continental).

% Workers and firms in receiving-state industries exposed to incoming labor competition. Where restrictions on access survive review they face less wage pressure; where movement opens, adjustment costs land on them first and they lobby for renewed controls.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, domestic_labor_sectors, beneficiary,
    organized, biographical, constrained, national).

% Non-citizen residents whose movement between member states runs through separate, narrower regimes. They are not parties to the citizenship-based framework and have no seat in its adjudication, though its boundaries determine which doors are open to them at all.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, third_country_residents, excluded,
    powerless, immediate, trapped, continental).

% Legal clinics, NGOs, and scholars who litigate test cases, document denied claims, and publish analyses of how the balance operates across cases and states. They see the full structure without collecting from it or paying into it.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobility_rights_advocates, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__subsidiarity_balance, receiving_state_governments).
narrative_ontology:fixing_cost_class(federation_membership_treaty__subsidiarity_balance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, adjudicable standard for when national restrictions on cross-border movement are acceptable, so that a multi-state labor and residence area can operate without either unilateral national closure or unconditional open access. Every enlargement, welfare reform, and public-order episode poses the same collective problem — protecting host institutions while keeping the mobility commitment credible — and proportionality review is the standing procedure for resolving it case by case.
% TRANSFER_FUNCTION: Moves access and discretion between seats case by case: a restriction that survives review transfers opportunity from the affected movers to the restricting state's budget and labor market; a restriction struck down returns mobility to the mover at the state's expense. Across the interval the largest steady transfers ran from accession-state workers (years of deferred earnings under transitional controls) and from low-income claimants (denied welfare access) to receiving-state budgets and sheltered domestic sectors.
% ABSENT_VOICES: Third-country residents are structurally outside the framework and would object that its protections attach to citizenship rather than personhood. Future accession populations inherited transitional restrictions negotiated by their governments before they held any seat. Individual movers whose claims failed review appear only as case statistics — no mechanism returns their testimony to the setting of the standard that denied them.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, the arrangement would collapse to one of its poles: member states would reimpose blanket national controls within months, or the commission and court would enforce unconditional movement against them. Transitional-control timetables, welfare-access gating, the adjudication pipeline, and the bilateral workaround agreements built around them all presuppose the framework; cross-border labor flows would reorganize around whichever pole filled the vacuum.
% FOUNDING_PROBLEM: Reconcile a credible commitment to cross-border movement within a common market with member states' retained authority over their labor markets, welfare systems, and public order — without either abolishing national self-government or making the mobility promise revocable at will.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: acceding states accepted transitional-control clauses in their accession treaties before they held any beneficiary position (Act of Accession annexes, 2003 and 2005), attesting that the tension was real enough to price into membership; the court's own case law acknowledges overriding imperative requirements as legitimate grounds for restriction; and comparative federalism scholarship documents the same recurring problem in every enlargement round. Receiving-state governments also attest the problem, but they are beneficiaries, so the independent attestations carry the provenance.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.46, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).
:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.46: real and concentrated — transitional controls deferred accession-state earnings for up to seven years per enlargement round, and welfare-access rulings deny identifiable claimants outright — but bounded, since core movement and work rights survive review in the large majority of cases. Suppression 0.52 is bidirectional: the framework actively suppresses blanket national bans (striking them down) and suppresses unrestricted-movement claims (upholding calibrated restrictions), eliminating neither extreme. Theater_ratio 0.28: adjudication is mostly functional, but deferential scrutiny of public-policy and public-security justifications contributes a performative share. Accessibility_collapse 0.42: alternatives persist — protocol opt-outs, association formats, costly-but-real exit — so understanding the framework does not foreclose every alternative. Resistance 0.5: states delay compliance and contest rulings, movers litigate and win, and the exit-referendum era confronted the framework openly. Inter-institutionally, receiving and sending governments share the institutional power atom but sit at opposite directionalities; the court and commission enforce rather than pay. All three tracked series share one six-point grid (1993–2023 mapped to 0–30), and each series' endpoint equals its base scalar.
 *
 * PERSPECTIVAL GAP:
 *   From the accession-state worker's seat, an upheld transitional control is seven lost years with no compensating service received — the arrangement computes as extraction wearing a coordination face. From the receiving government's seat, the same upheld control is legitimate self-government doing exactly what the framework permits. From the court's seat it is doctrine functioning as designed. The engine derives these divergent per-seat classifications from the structural data (role, power, exit); the authored claim does not adjudicate between them. The claimant seat deserves special note: each denied claim is isolated, with no organizing surface, so the seat's numbers never convert into coalition power despite its size.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for mobile_eu_citizens (the enforceable rights floor damps their d despite indirect costs), domestic_labor_sectors, and receiving_state_governments. Victim declarations drive high directionality for accession_state_workers and, at the extreme, low_income_mobile_claimants, whose trapped exit and negligible power place them nearest the full-target end. Sending_state_governments are the known derivation limitation: declared victims because their nationals are restricted, but genuinely ambivalent — slower emigration relieves brain-drain pressure — so their true d sits near 0.55, well below the victim-derived value. No directionality override is authored because overrides key on the power atom, and receiving-state governments share the institutional atom; correcting one would corrupt the other. The ambivalence is recorded here and in the stakeholder's secondary role instead.
 *
 * MANDATROPHY ANALYSIS:
 *   Reading the arrangement as pure rope would erase the identifiable losers — welfare-denied claimants and transition-delayed workers — whose losses are the mechanism's operating cost, not incidental friction. Reading it as a snare would erase the genuine bidirectional constraint: blanket bans really do fall, unrestricted-movement claims really do fail, and alternatives outside the framework persist. The mandatrophy question — is the founding problem dead while the arrangement persists? — answers no: every enlargement and every fiscal squeeze revives the founding tension, so founding_problem_status 'live' paired with disappearance_verdict 'world_rearranges' produces no zombie flag. The live drift risk runs the other way: if transitional controls lapse permanently and welfare rulings converge mover-ward, the extraction half atrophies and the arrangement decays toward rope; the measurement series tracks exactly this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_operativity,
    'Is the proportionality-balance reading the operative constraint, or does one of the sibling readings (integration_primary, sovereignty_primary) govern actual outcomes with proportionality language as cover?',
    'Comparative prediction test: code CJEU free-movement rulings 1993-2023 against each reading''s predicted outcome distribution; the reading whose predictions best fit the upheld-versus-struck-down rates is operative.',
    'If integration_primary is operative, ε falls well below 0.46 (restrictions rarely survive) and the arrangement trends rope; if sovereignty_primary is operative, ε rises sharply, the mover floor-rights axiom fails in practice, and the victim set expands toward all movers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_operativity, conceptual, 'Which kernel reading governs actual adjudication outcomes versus which reading the doctrine narrates.').

omega_variable(
    domain_indexed_extraction,
    'Extraction varies sharply by policy domain (welfare access versus worker mobility versus establishment and services): is ε one stable property of the whole arrangement, or is the arrangement domain-indexed enough to require decomposition into separate constraint stories?',
    'Domain-stratified outcome coding: if the welfare-access domain''s upheld-restriction rate implies an ε differing from the worker-mobility domain''s by a wide margin, decompose per the ε-invariance principle.',
    'Decomposition would yield a high-ε welfare-access sub-constraint (snare-leaning, concentrated victims) and a low-ε core-mobility sub-constraint (rope-leaning), linked by network edges; the unified story''s 0.46 would be retired as a conflation artifact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_indexed_extraction, empirical, 'Whether the arrangement is one constraint or a domain-decomposed constraint family.').

omega_variable(
    third_country_floor_exclusion,
    'Does the proportionality floor extend to third-country nationals and asylum seekers, or does the measured ε understate total extraction by counting only citizen movers?',
    'Extend the outcome coding to the third-country regimes (long-term residents directive, intra-EU transfer procedures): if restriction-uphold rates against non-citizens vastly exceed citizen rates, the citizen-only measure understates the arrangement''s extraction.',
    'If included, ε rises materially and the victim set expands; the reading''s mobility-floor axiom would hold only for citizens, pushing the operative structure toward the sovereignty_primary sibling''s shape for everyone else.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(third_country_floor_exclusion, empirical, 'Citizen-only versus all-mover accounting of the arrangement''s costs.').

omega_variable(
    enforcement_politicization_trajectory,
    'Will rising member-state defiance (conditionality disputes, delayed compliance with rulings) shift the operative constraint from adjudicated balance toward de facto consent-based operation?',
    'Track infringement-proceeding completion rates and ruling-compliance latency from 2023 forward; sustained decay indicates the enforcement layer is failing while adjudication continues.',
    'Enforcement decay would push operational behavior toward the sovereignty_primary sibling, with theater_ratio climbing as review continues without compliance — a tangled_rope decaying toward piton-by-performance on the enforcement side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_politicization_trajectory, empirical, 'Whether enforcement-layer decay is shifting the operative regime''s center of gravity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(fede_tr_t0, observed).
narrative_ontology:measurement(fede_tr_t6, federation_membership_treaty__subsidiarity_balance, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(fede_tr_t6, observed).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__subsidiarity_balance, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(fede_tr_t12, observed).
narrative_ontology:measurement(fede_tr_t18, federation_membership_treaty__subsidiarity_balance, theater_ratio, 18, 0.3).
narrative_ontology:measurement_basis(fede_tr_t18, observed).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.29).
narrative_ontology:measurement_basis(fede_tr_t24, observed).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__subsidiarity_balance, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(fede_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(fede_be_t0, observed).
narrative_ontology:measurement(fede_be_t6, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 6, 0.38).
narrative_ontology:measurement_basis(fede_be_t6, observed).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(fede_be_t12, observed).
narrative_ontology:measurement(fede_be_t18, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 18, 0.52).
narrative_ontology:measurement_basis(fede_be_t18, observed).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.47).
narrative_ontology:measurement_basis(fede_be_t24, observed).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 30, 0.46).
narrative_ontology:measurement_basis(fede_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(fede_su_t0, observed).
narrative_ontology:measurement(fede_su_t6, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 6, 0.44).
narrative_ontology:measurement_basis(fede_su_t6, observed).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(fede_su_t12, observed).
narrative_ontology:measurement(fede_su_t18, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(fede_su_t18, observed).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.54).
narrative_ontology:measurement_basis(fede_su_t24, observed).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(fede_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'free movement in the federation' decomposes into three readings of one treaty kernel. integration_primary is the upstream constitutive-market claim (highest confidence in the market's existence, lowest standalone extraction); sovereignty_primary is the consent-conditioned counterclaim; subsidiarity_balance (this file) mediates, inheriting citations from both — integration_primary's market logic supplies the mobility floor this reading preserves, and sovereignty_primary's legitimate-interest inventory supplies the restriction grounds this reading legitimizes. Each family member carries its own ε, beneficiary set, and victim set; the edges declared here are family links, not contamination claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
