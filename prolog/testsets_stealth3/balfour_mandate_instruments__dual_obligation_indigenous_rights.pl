% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__dual_obligation_indigenous_rights
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__dual_obligation_indigenous_rights, []).

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
 *   constraint_id: balfour_mandate_instruments__dual_obligation_indigenous_rights
 *   human_readable: Mandate Dual-Obligation Protective Regime — Indigenous Rights Reading
 *   domain: international law/colonial administration/state formation
 *
 * SUMMARY:
 *   Under this instantiation, the Mandate's protective articles are the
 *   operative law of the territory: the administration owes the existing
 *   population civil and political rights and secure land tenure on equal or
 *   better terms than any facility owed to newcomers, and the national-home
 *   facility operates only inside those limits. The working machinery is
 *   concrete — land registry refusals in protected zones, tenant-protection
 *   rules on transferred tracts, immigration certificates issued against an
 *   economic absorptive-capacity schedule, and custodial duties binding the
 *   administration — all executed by British officials under League
 *   supervision and audited annually in Geneva. The claim/metric split is
 *   deliberate: the claimed type is tangled_rope because the structure
 *   carries a genuine protection function for the majority population while
 *   imposing heavy asymmetric costs on the settlement program, the seller
 *   class, the blocked migrants, and the administering power itself; the
 *   metrics are authored from the observed operation of the machinery, not
 *   tuned to the claim. Assumptions stated: the interval maps to the Mandate
 *   years 1922-1948; metric values are assessed from the enforcement-record
 *   phases of the interval; the end-state figures match the terminal
 *   measurement row.
 *
 * KEY AGENTS:
 *   - - palestinian_arab_peasant_tenantry: Protected beneficiary (powerless/trapped) — tenure and village continuity secured by registry controls; bears no new burden
 *   - - palestinian_arab_national_leadership: Political beneficiary (organized/identity_locked) — majority standing and the representative-government claim are anchored by the protective articles
 *   - - arab_absentee_land_sellers: Willing sellers turned cost-bearers (powerful/constrained) — premium sales blocked at the registry
 *   - - zionist_settlement_institutions: Primary target (organized/identity_locked) — the two channels their program runs through are throttled at registry and permit desk
 *   - - european_jewish_quota_blocked_refugees: Collateral target (powerless/trapped) — the certificate ceiling closes a route with no substitute
 *   - - british_mandatory_administration: Administering cost-bearer (institutional/constrained) — runs the enforcement machinery and absorbs the diplomatic price of holding it
 *   - - permanent_mandates_commission: Supervisory observer (institutional/analytical) — audits the annual record against the obligations' terms
 *   - - arab_constitutionalists: Excluded constituency (moderate/constrained) — the promised representative institutions whose indefinite deferral silences the trajectory the protections defer to
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68).
domain_priors:suppression_score(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.55).
domain_priors:theater_ratio(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, extractiveness, 0.68).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(balfour_mandate_instruments__dual_obligation_indigenous_rights, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__dual_obligation_indigenous_rights, tangled_rope).
narrative_ontology:human_readable(balfour_mandate_instruments__dual_obligation_indigenous_rights, "Mandate Dual-Obligation Protective Regime — Indigenous Rights Reading").
narrative_ontology:topic_domain(balfour_mandate_instruments__dual_obligation_indigenous_rights, "international law/colonial administration/state formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__dual_obligation_indigenous_rights).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__dual_obligation_indigenous_rights, '1179f329-73bf-4377-8029-395733bd4279').
narrative_ontology:cs_kernel_codification('1179f329-73bf-4377-8029-395733bd4279', fixed_text).
narrative_ontology:cs_authority_grounding('1179f329-73bf-4377-8029-395733bd4279', lineage).
narrative_ontology:cs_interpretation_layer_present('1179f329-73bf-4377-8029-395733bd4279').
narrative_ontology:cs_reading_relation('1179f329-73bf-4377-8029-395733bd4279', balfour_mandate_instruments__jewish_national_home_primacy, forecloses).
narrative_ontology:cs_reading_relation('1179f329-73bf-4377-8029-395733bd4279', balfour_mandate_instruments__mandatory_interpretive_discretion, influences).
narrative_ontology:cs_axiom('1179f329-73bf-4377-8029-395733bd4279', foundational, protective_obligation_equal_or_superior).
narrative_ontology:cs_axiom_status(protective_obligation_equal_or_superior, holdable).
narrative_ontology:cs_axiom_grounding('1179f329-73bf-4377-8029-395733bd4279', protective_obligation_equal_or_superior, deontological).
narrative_ontology:cs_axiom('1179f329-73bf-4377-8029-395733bd4279', secondary, majority_wishes_condition_self_government).
narrative_ontology:cs_axiom_status(majority_wishes_condition_self_government, holdable).
narrative_ontology:cs_axiom_grounding('1179f329-73bf-4377-8029-395733bd4279', majority_wishes_condition_self_government, conventional).
narrative_ontology:cs_reference_frame('1179f329-73bf-4377-8029-395733bd4279', sacred_trust_equal_obligation_frame).
narrative_ontology:cs_drift_state('1179f329-73bf-4377-8029-395733bd4279', macdonald_letter_aftermath, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1179f329-73bf-4377-8029-395733bd4279', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__dual_obligation_indigenous_rights, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_peasant_tenantry).
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_national_leadership).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_settlement_institutions).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, european_jewish_quota_blocked_refugees).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, arab_absentee_land_sellers).
narrative_ontology:constraint_victim(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, league_covenant_sacred_trust_doctrine).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, minority_protection_treaty_norms).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, indigenous_tenure_security_principle).
narrative_ontology:constraint_vindicates(balfour_mandate_instruments__dual_obligation_indigenous_rights, economic_absorptive_capacity_calibration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cultivate and occupy land across the hill villages and plains. Transfer controls keep registered holdings in their hands: transactions out of protected zones are refused at the registry, and cultivators on transferred tracts cannot be displaced. What reaches them is continuity of tenure and village life; what they pay beyond existing taxation is nothing. Leaving the land was never an option — it is their subsistence.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_peasant_tenantry, beneficiary,
    powerless, generational, trapped, regional).

% Urban notable families and religious-office holders who speak for the majority population through petitions to Geneva, delegations to London, and the Supreme Muslim Council's institutions. The protective articles anchor their standing claim that the majority's civil and political rights precede any immigrant facility and that representative institutions are the agreed destination. Their position is fused with the majority-standing claim itself; abandoning it would dissolve who they are.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_national_leadership, beneficiary,
    organized, generational, identity_locked, regional).

% Wealthy urban proprietors, often resident in Beirut or Damascus, holding tracts worked by tenants. Before transfer restrictions they sold to settlement purchasers at premiums far above local value; registry refusals now block those transactions in protected zones. Their capital is movable but the estates are not, and the remaining domestic market pays much less. They press the administration through the same notable networks that otherwise defend the protections.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, arab_absentee_land_sellers, payer,
    powerful, biographical, constrained, regional).

% The Jewish Agency, the Jewish National Fund, and affiliated settlement and labor organizations financed by worldwide diaspora fundraising. Their program runs through two channels the administration throttles: registered land purchase and authorized immigration. Certificate ceilings and registry refusals strike the load-bearing axis of the enterprise; they respond with political mobilization in London, intermediary purchase structures, and unauthorized entry networks. They cannot exit the project — the land program is what the institutions are.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, zionist_settlement_institutions, payer,
    organized, civilizational, identity_locked, global).

% Jews in Europe seeking admission under the immigration certificates the administration issues. After 1933 the certificate schedule is the difference between a visa and none: doors across the continent are closing and the monthly schedule admits a fraction of applicants. They hold no alternative refuge and no seat in any forum where the schedule is set.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, european_jewish_quota_blocked_refugees, payer,
    powerless, immediate, trapped, continental).

% The Colonial Office, the high commissioner, district commissioners, land registrars, and immigration departments running Palestine. They administer the protective articles: refusing registrations, issuing certificates to schedule, answering commissions of inquiry, and filing annual reports to Geneva. Every tightening buys quiet on one side and costs cooperation on the other; every loosening reverses the trade. They pay in diplomatic capital, enforcement manpower, and managed contradiction, and collect no revenue particular to the protective side of the ledger.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(balfour_mandate_instruments__dual_obligation_indigenous_rights, british_mandatory_administration, payer).

% The League body in Geneva that receives the annual report, hears petitions from Arab delegations, questions visiting officials, and minutes its reservations. It holds audit authority without command authority: its questions determine what London must explain, and its sessions are the one venue where both communities' claims meet a neutral transcript.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, permanent_mandates_commission, observer,
    institutional, generational, analytical, global).

% Moderate Arab politicians and municipal figures who accepted participation in a legislative council and gradual constitutional development as the agreed path. The council proposed in 1922 never convened — boycott and scheduling disputes buried it — and successive schemes lapsed. They would object that the protective articles defer the very institutions they were told would follow, and they have no forum in which the deferral is decided.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__dual_obligation_indigenous_rights, arab_constitutionalists, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(balfour_mandate_instruments__dual_obligation_indigenous_rights, palestinian_arab_peasant_tenantry).
narrative_ontology:fixing_cost_class(balfour_mandate_instruments__dual_obligation_indigenous_rights, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages two incompatible settlement programs under a single administering authority without open civil war: registry controls prevent a market-driven dispossession spiral, absorptive-capacity calibration prevents demographic shock, and both populations receive a predictable rule-bound environment instead of a race for accomplished facts.
% TRANSFER_FUNCTION: Moves land-market access and admission slots away from settlement purchasers and intending immigrants toward incumbent cultivators; moves enforcement effort and diplomatic expenditure from the administering power; moves political standing toward the majority population's representative claim.
% ABSENT_VOICES: The blocked migrant pool had no seat anywhere — not in setting the certificate schedule, not at the refugee conferences of the late 1930s, not in Mandate governance. Settlement negotiators were shut out of the Passfield deliberations and walked out of the 1939 round table when asked to sit with Arab delegates. Village-level cultivators affected by specific transactions were decided for rather than consulted. The constitutionalist constituency was silenced by the non-convening of the promised council.
% DISAPPEARANCE_RATIONALE: If the protective articles and their enforcement vanished overnight, the registry opens to premium-priced purchases backed by diaspora capital, the certificate ceiling disappears, the demographic and tenure trajectories bend within a decade, the majority-standing politics built on those protections loses its anchor, and the administering power's governing formula collapses into open revolt — the entire arrangement of the territory reorganizes.
% FOUNDING_PROBLEM: Operationalize contradictory wartime and postwar pledges at once: support for a Jewish national home in Palestine, the Hussein-McMahon correspondence promising Arab political futures, and the Covenant of the League's requirement that Mandates serve as a sacred trust protecting societies not yet able to stand alone. The instruments had to let a transformative settlement proceed without destroying the existing society.
% FOUNDING_PROBLEM_CORROBORATION: Permanent Mandates Commission session minutes record commissioners pressing the administering power year after year on the balance of its obligations; the Shaw Commission and the Hope Simpson Report were commissioned by the British government itself and document the dispossession pressure the protections answer; the King-Crane report, an American inquiry predating the Mandate, independently registered the incompatibility the arrangement manages. Corroboration therefore comes from supervisory and inquiry bodies outside both national movements; no essential attestation rests solely on the protected communities.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__dual_obligation_indigenous_rights, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__dual_obligation_indigenous_rights, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(balfour_mandate_instruments__dual_obligation_indigenous_rights, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__dual_obligation_indigenous_rights, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__dual_obligation_indigenous_rights, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__dual_obligation_indigenous_rights_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.68: whenever the protections bind, they take heavily from governed parties — settlement institutions lose the acquisition and admission channels their program depends on, blocked migrants lose a route with no substitute, willing sellers lose the premium market, and the administration pays enforcement and diplomatic costs while collecting nothing particular to the protective side. Suppression 0.55 is authored as a raw structural property and is deliberately left unscaled: permit regimes, registry refusals, and interdiction of unauthorized entry are real coercive force, but circumvention channels persisted, so the figure stops short of closure. Theater 0.42 at interval end: the registry-and-permit core stayed functional, but a growing share of activity migrated to commissions, committees, conferences, and annual reporting — inquiry as substitute for decision. Accessibility collapse 0.55: knowing how the machinery worked did not close alternatives (front-company purchases, unauthorized entry, political reversal in London), so alternatives persisted at moderate strength. Resistance 0.60: sustained counter-pressure flipped the 1930 policy within months, illegal entry networks ran against the 1939 ceiling for years, and the seller class worked the administration through its own channels. The series traces one full oscillation: build (1922-31), lapse under London pressure (1931-36), ratchet under crisis (1939), decay of enforcement capacity (1944-48). The cycle's driver is alternating external pressure — cabinet sensitivity to mobilized diaspora opinion produces lapses; on-ground crises and revolt produce ratchets — and the intermittency itself functions as reinforcement, keeping both movements dependent on British arbitration rather than forcing them toward exits. Receipt surface: the material substance of the protections — retained tenure, prevented displacements — accrues demonstrably to the cultivating class, with leadership gains derivative and political; fixing cost for the administering power is prohibitive because full removal meant Covenant censure in Geneva, revolt risk on the ground, and imperial-legitimacy loss outweighing the diplomatic relief it bought.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the tenancy seat the apparatus arrives as pure subsidy — protection without payment. From the settlement-institution seat it lands across the program's load-bearing axis and reads as a wall. From the administering seat it is a duty whose costs arrive from every direction at once. From the supervisory seat it is an audit object, real only insofar as the annual record substantiates it. Same-power lateral divergence appears inside Arab society itself: cultivator and proprietor face the same articles from opposite sides of the transfer channel, so equal nominal standing yields opposite positions. The engine derives these divergences from the structural declarations; nothing here adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the two Arab seats toward the subsidized end: tenantry (trapped, powerless) sits nearest full beneficiary, leadership (identity_locked) somewhat inward of that because its gains are political and contingent on delivery. Victim declarations drive the payers toward the target end: settlement institutions (identity_locked) sit near full target — exit from the land program would dissolve the institutions themselves — and blocked migrants (trapped, powerless) sit at the extreme target end despite holding no power at all, which is precisely the amplification the exit modulation exists to capture. The seller seat is the lateral case: ethnically contiguous with the beneficiaries yet structurally a payer, differentiated by relationship to the transfer channel rather than group membership. The administering power is listed among victims because it pays without receiving: it holds the agenda but the derivation correctly reads its cost-bearing position, and its secondary payer role encodes that dual position without an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in both directions. Calling the arrangement pure coordination hides the blocked-program costs — a national project throttled at its load-bearing axis, a refugee pool locked out of a route with no substitute, a seller class taxed by prohibition — which is exactly the asymmetric-extraction half the hybrid category requires. Calling it pure extraction ignores that the protections solved a real collective-action problem for the majority population and that alternatives were never fully closed. Active enforcement is what holds the shape: registry refusals, certificate rationing, and interdiction are the load-bearing machinery, and their decay is the observable failure mode. The founding problem remains contested-live, so no mandatrophy resolution is declared; the watch condition is theater crossing 0.5 — inquiry replacing decision while the instruments persist — at which point the structure drifts toward the inertial category with nobody positioned to fix it cheaply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates only the indigenous-rights reading of the balfour_mandate_instruments kernel; what would the sibling readings change in the structural data?',
    'Generate and compare the sibling stories against this one: beneficiary and victim sets invert under jewish_national_home_primacy; substantive obligations are replaced by adjudicative discretion itself under mandatory_interpretive_discretion.',
    'Under the primacy reading the same instruments yield a facilitation regime with Arab communities as targets and settlement bodies as beneficiaries, with epsilon reassessed over that referent; under the discretion reading neither substantive obligation binds and the operative constraint is who may decide.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Kernel-membership omega: which obligation the Mandate instruments impose is the contested kernel; this file authors one reading of it.').

omega_variable(
    clause_ordering_disagreement_site,
    'Where exactly is the interpretive disagreement located — in the status ordering of the national-home clause against the protective articles?',
    'Textual analysis of the Declaration wording, the 1922 White Paper gloss, and Permanent Mandates Commission questioning of visiting officials: whether the instruments fix an ordering between facility and safeguard or leave it open to circumstance.',
    'If the instruments fix no ordering, this reading''s foundational axiom loses its textual anchor and the discretion reading inherits the field; if the ordering is fixed as authored here, the primacy ordering is excluded from any single coherent interpretive framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clause_ordering_disagreement_site, conceptual, 'Location of the interpretive disagreement: fixed clause-ordering versus open-endedness.').

omega_variable(
    enforcement_continuity_gap,
    'How much of the measured extraction belongs to this arrangement as such, versus to its intermittent application — were the protections enforceable against settlement pressure at all?',
    'Compare binding and enforcement levels across enforcement windows (1930-31, 1939-45) against lapse windows (1931-36, post-war demobilization): if the same structural costs appear whenever the protections bind, the arrangement is enforceable; if protections operated only when politically convenient, the arrangement trends toward ceremony.',
    'Demonstrated enforceability sustains the hybrid classification with genuine protective function; constitutive unenforceability pushes the structure toward theatrical maintenance and eventual inertial character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_continuity_gap, empirical, 'Delivery gap between protective obligation and protective practice across the interval.').

omega_variable(
    absorptive_capacity_objectivity,
    'Was economic absorptive capacity an objective demographic-economic limit or a discretionary instrument presented as technique?',
    'Retrospective comparison of certified immigration schedules against independent reconstructions of labor-market and infrastructural absorption in the same periods.',
    'A discretionary standard raises the effective burden on blocked migrants and settlement bodies (cap levels tracking politics rather than capacity) and strengthens the discretion reading; a defensible objective standard strengthens the coordination content of the calibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(absorptive_capacity_objectivity, empirical, 'Status of the calibration standard underlying the immigration ceilings.').

omega_variable(
    arab_seat_heterogeneity,
    'Do the protective articles serve the Arab population as a unit, or a coalition of protected cultivators and national leadership at the cost of willing sellers?',
    'Decompose Arab-seat position by relationship to the transfer channel: cultivators retaining tenure, leadership converting protection into standing, proprietors losing premium-priced sales to settlement purchasers.',
    'If seller losses are material, the beneficiary declarations overstate Arab-side net position and the Arab seats split between subsidized and taxed positions; if negligible, the unitary-protected-population picture holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_seat_heterogeneity, empirical, 'Coalitional structure of the protected population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__dual_obligation_indigenous_rights, 1922, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1922, 0.25).
narrative_ontology:measurement_basis(balf_tr_t1922, observed).
narrative_ontology:measurement(balf_tr_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1925, 0.29).
narrative_ontology:measurement_basis(balf_tr_t1925, observed).
narrative_ontology:measurement(balf_tr_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1929, 0.34).
narrative_ontology:measurement_basis(balf_tr_t1929, observed).
narrative_ontology:measurement(balf_tr_t1931, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1931, 0.27).
narrative_ontology:measurement_basis(balf_tr_t1931, observed).
narrative_ontology:measurement(balf_tr_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1936, 0.4).
narrative_ontology:measurement_basis(balf_tr_t1936, observed).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1939, 0.3).
narrative_ontology:measurement_basis(balf_tr_t1939, observed).
narrative_ontology:measurement(balf_tr_t1944, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1944, 0.37).
narrative_ontology:measurement_basis(balf_tr_t1944, observed).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, theater_ratio, 1948, 0.42).
narrative_ontology:measurement_basis(balf_tr_t1948, observed).

% Extraction over time
narrative_ontology:measurement(balf_be_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1922, 0.42).
narrative_ontology:measurement_basis(balf_be_t1922, observed).
narrative_ontology:measurement(balf_be_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1925, 0.46).
narrative_ontology:measurement_basis(balf_be_t1925, observed).
narrative_ontology:measurement(balf_be_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1929, 0.51).
narrative_ontology:measurement_basis(balf_be_t1929, observed).
narrative_ontology:measurement(balf_be_t1931, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1931, 0.63).
narrative_ontology:measurement_basis(balf_be_t1931, observed).
narrative_ontology:measurement(balf_be_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1936, 0.57).
narrative_ontology:measurement_basis(balf_be_t1936, observed).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1939, 0.76).
narrative_ontology:measurement_basis(balf_be_t1939, observed).
narrative_ontology:measurement(balf_be_t1944, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1944, 0.72).
narrative_ontology:measurement_basis(balf_be_t1944, observed).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement_basis(balf_be_t1948, observed).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1922, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1922, 0.4).
narrative_ontology:measurement_basis(balf_su_t1922, observed).
narrative_ontology:measurement(balf_su_t1925, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1925, 0.43).
narrative_ontology:measurement_basis(balf_su_t1925, observed).
narrative_ontology:measurement(balf_su_t1929, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1929, 0.49).
narrative_ontology:measurement_basis(balf_su_t1929, observed).
narrative_ontology:measurement(balf_su_t1931, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1931, 0.61).
narrative_ontology:measurement_basis(balf_su_t1931, observed).
narrative_ontology:measurement(balf_su_t1936, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1936, 0.53).
narrative_ontology:measurement_basis(balf_su_t1936, observed).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1939, 0.74).
narrative_ontology:measurement_basis(balf_su_t1939, observed).
narrative_ontology:measurement(balf_su_t1944, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1944, 0.66).
narrative_ontology:measurement_basis(balf_su_t1944, observed).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__dual_obligation_indigenous_rights, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement_basis(balf_su_t1948, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__dual_obligation_indigenous_rights, resource_allocation).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__dual_obligation_indigenous_rights, mandatory_interpretive_discretion).

% DUAL FORMULATION NOTE:
% The colloquial label Mandate obligations in Palestine decomposes into three structurally distinct constraints with different epsilon and different victim sets: the indigenous-rights protective regime (this story), the national-home facilitation regime (jewish_national_home_primacy), and the adjudicative-discretion regime (mandatory_interpretive_discretion). The discretion regime sits upstream of both substantive regimes — which substantive reading bites in any given year is determined by how the discretionary authority exercises itself — so this story links both siblings. The two substantive readings are logical contraries on the clause-ordering question and cannot share a single interpretive framework; each is authored as its own file with its own beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
