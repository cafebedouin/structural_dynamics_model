% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__balanced_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: one_country_two_systems_framework__balanced_coexistence_reading
 *   human_readable: One Country, Two Systems — Negotiated Coexistence Regime (Balanced Coexistence Reading)
 *   domain: constitutional/political/sovereignty
 *
 * SUMMARY:
 *   This story instantiates the balanced coexistence reading of the One
 *   Country, Two Systems settlement: a constitutional arrangement in which
 *   sovereignty and autonomy are both bounded, powers are divided
 *   functionally (defense, foreign affairs, and national security to the
 *   center; domestic law, courts, economy, and daily administration to the
 *   territory), and boundary disputes are settled through political
 *   accommodation rather than by either legal system simply prevailing. On
 *   this reading the arrangement is a working negotiated order with a real
 *   coordination achievement — two incompatible systems have been held
 *   together under one flag for a generation — that nonetheless carries a
 *   persistent asymmetric tilt: when accommodation fails, the center holds
 *   the interpretive lever, the security apparatus, and the appointment
 *   power, so the costs of failed negotiation land disproportionately on the
 *   territory's autonomous politics. The ε referent is the standing
 *   arrangement as this reading assesses it: real and rising extraction
 *   concentrated in the political-security domain, bounded by surviving
 *   channels of negotiation, judicial continuity, and civil-society leverage.
 *   The claim/metric gap is deliberate: the reading CLAIMS a tangled
 *   coordination-extraction hybrid; the authored metrics describe a regime
 *   whose extraction and enforcement have hardened markedly over the interval
 *   — the engine measures that divergence; nothing here reconciles claim to
 *   metrics.
 *
 * KEY AGENTS:
 *   - prc_central_authorities: agenda-setting institutional seat (institutional/arbitrage) — controls interpretation, appointments, and security policy; collects jurisdictional ground in every settled dispute
 *   - hongkong_business_elites: primary beneficiary seat (powerful/arbitrage) — collects market access and legal continuity; exit-leveraged, least exposed
 *   - hongkong_pro_beijing_estaffment_placeholder: beneficiary/administrator seat (organized/identity_locked) — staffs the arrangement locally; status inseparable from loyalty
 *   - hongkong_pandemocracy_movement: primary payer seat (organized/identity_locked) — bears concentrated costs of every failed accommodation cycle
 *   - ordinary_hongkong_residents: mixed payer/beneficiary seat (moderate/constrained) — retains daily autonomy, bears contested-domain costs diffusely
 *   - hongkong_judiciary: dual-positioned institutional seat (institutional/constrained) — administers the retained system under eroding interpretive finality
 *   - united_kingdom_government: excluded co-signatory (institutional/analytical) — attests and reports, cannot enforce
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) — sees the full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__balanced_coexistence_reading, 0.68).
domain_priors:suppression_score(one_country_two_systems_framework__balanced_coexistence_reading, 0.8).
domain_priors:theater_ratio(one_country_two_systems_framework__balanced_coexistence_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(one_country_two_systems_framework__balanced_coexistence_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__balanced_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__balanced_coexistence_reading, "One Country, Two Systems — Negotiated Coexistence Regime (Balanced Coexistence Reading)").
narrative_ontology:topic_domain(one_country_two_systems_framework__balanced_coexistence_reading, "constitutional/political/sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__balanced_coexistence_reading).
narrative_ontology:has_sunset_clause(one_country_two_systems_framework__balanced_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__balanced_coexistence_reading, 'b03cc973-2f34-4292-9a97-88f70ab16cf0').
narrative_ontology:cs_kernel_codification('b03cc973-2f34-4292-9a97-88f70ab16cf0', fixed_text).
narrative_ontology:cs_authority_grounding('b03cc973-2f34-4292-9a97-88f70ab16cf0', practice).
narrative_ontology:cs_interpretation_layer_present('b03cc973-2f34-4292-9a97-88f70ab16cf0').
narrative_ontology:cs_reading_relation('b03cc973-2f34-4292-9a97-88f70ab16cf0', one_country_two_systems_framework__sovereignty_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('b03cc973-2f34-4292-9a97-88f70ab16cf0', one_country_two_systems_framework__autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('b03cc973-2f34-4292-9a97-88f70ab16cf0', foundational, neither_sovereignty_nor_autonomy_absolute).
narrative_ontology:cs_axiom_status(neither_sovereignty_nor_autonomy_absolute, holdable).
narrative_ontology:cs_axiom_grounding('b03cc973-2f34-4292-9a97-88f70ab16cf0', neither_sovereignty_nor_autonomy_absolute, instrumental).
narrative_ontology:cs_axiom('b03cc973-2f34-4292-9a97-88f70ab16cf0', foundational, boundary_disputes_resolved_through_accommodation).
narrative_ontology:cs_axiom_status(boundary_disputes_resolved_through_accommodation, holdable).
narrative_ontology:cs_axiom_grounding('b03cc973-2f34-4292-9a97-88f70ab16cf0', boundary_disputes_resolved_through_accommodation, conventional).
narrative_ontology:cs_reference_frame('b03cc973-2f34-4292-9a97-88f70ab16cf0', joint_declaration_negotiated_equilibrium).
narrative_ontology:cs_drift_state('b03cc973-2f34-4292-9a97-88f70ab16cf0', post_national_security_law_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b03cc973-2f34-4292-9a97-88f70ab16cf0', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__balanced_coexistence_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authorities).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_business_elites).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_pro_beijing_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_pandemocracy_movement).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, ordinary_hongkong_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_pro_beijing_estaffment_placeholder).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, ordinary_hongkong_residents).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Directs defense, foreign affairs, and national security policy for the territory, appoints the chief executive, and issues authoritative interpretations of the Basic Law through its standing committee. Sets the pace and ceiling of political development and decides when a boundary dispute is settled by negotiation and when by directive. Can restructure the terms of the arrangement unilaterally when it judges the coexistence framework to threaten sovereign control.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, prc_central_authorities, agenda_setter,
    institutional, generational, arbitrage, continental).

% Hold property portfolios, trading houses, and professional franchises that depend on the territory's distinct legal and customs regime. Collect preferential access to mainland markets, low taxation, and common-law commercial courts. Hold foreign residency and mobile capital, and cultivate relationships with both the central authorities and local government.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_business_elites, beneficiary,
    powerful, biographical, arbitrage, global).

% Staff the territorial government, its executive council, and the legislative majority produced under the restructured electoral rules. Their careers, honors, and social standing are bound up with demonstrating loyalty to the center while administering local affairs. Leaving the arrangement would mean forfeiting the offices and status the arrangement alone confers.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_pro_beijing_estaffment_placeholder, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_pro_beijing_estaffment_placeholder, agenda_setter).

% Organized mass mobilization, electoral campaigns, and civil society networks demanding universal suffrage and preserved civil liberties. Across successive boundary disputes its leaders absorbed prosecutions, legislative disqualifications, media closures, and exile. Its members' political identity is constituted by the demand the arrangement currently refuses; exit means prison, departure, or political silence.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_pandemocracy_movement, payer,
    organized, biographical, identity_locked, regional).

% Live under the retained common-law courts, passport-free travel, press pluralism (narrowing), and the capitalist economy the arrangement preserves. Bear the costs in contested domains: a narrowed choice of candidates, security-law exposure for speech and association, and housing and inequality outcomes shaped by the elite-dominated political structure. Emigration pathways exist but are costly and disruptive.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, ordinary_hongkong_residents, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, ordinary_hongkong_residents, beneficiary).

% Administers the common-law system, hears constitutional challenges, and retains international professional prestige. Its final interpretive authority is qualified by standing-committee interpretations that can override its judgments, and recent security legislation assigns it politically sensitive dockets. Judges cannot resign the system's problems; some senior figures have departed, others continue under narrowed room.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary, beneficiary,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__balanced_coexistence_reading, hongkong_judiciary, payer).

% Co-signatory of the joint declaration establishing the arrangement. Publishes six-monthly reports to Parliament and offers citizenship pathways to some residents, but holds no seat in the arrangement's decision loop and no enforcement mechanism beyond diplomatic statement. Its objections register as record, not as leverage.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__balanced_coexistence_reading, united_kingdom_government, excluded,
    institutional, generational, analytical, global).

% Track the arrangement's operation across successive crises, publishing analyses of interpretation practice, electoral restructuring, and the shifting boundary between the two legal systems. Hold no stake in outcomes beyond the epistemic; their assessments feed international opinion and the historical record.
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
% COORDINATION_FUNCTION: Manages the coexistence of two legal, economic, and administrative systems within a single sovereign state: defense and foreign affairs are exercised centrally while the territory keeps its common-law courts, separate customs territory, currency, and domestic administration. Solves once, at the constitutional level, the problem that would otherwise be relitigated in every individual dispute between the center and the territory.
% TRANSFER_FUNCTION: Moves decision authority over contested domains — national security, electoral design, constitutional interpretation — from territorial institutions to central authorities during crises, and moves economic access, international financial connectivity, and legitimating 'high autonomy' recognition in the opposite direction.
% ABSENT_VOICES: No elected representatives of the territory's population sat at the founding negotiations; the 1980s talks were conducted between London and Beijing over the inhabitants' heads. Today the imprisoned, disqualified, and exiled opposition, and the younger cohorts who never consented to any version of the settlement, are outside the conversation. Their objection: every boundary settlement has been negotiated by the two centers and the loyalist establishment, never with the people governed by it.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the territory's courts, currency board, customs territory, and immigration regime would lose their constitutional foundation; the center would face either immediate absorption of a hostile legal system or construction of a successor framework, and capital and residents would repriced both options within days. Nothing about the territory's position is self-sustaining apart from this settlement.
% FOUNDING_PROBLEM: How to resume sovereignty over a treaty-acquired capitalist enclave without destroying its economic system, its international financial role, or its population's willingness to stay — and, secondarily, how to demonstrate to Taiwan that reunification need not mean assimilation.
% FOUNDING_PROBLEM_CORROBORATION: British Foreign Office archives and parliamentary records from the negotiation period document the design aims (preserving the territory's capitalism and confidence, stabilizing the transfer); academic historiography of the handover corroborates the two-systems management problem as the settlement's core; Taiwanese official statements repeatedly cite the arrangement's operation as precedent-bearing for cross-strait talks. None of these sources belongs to the arrangement's benefiting parties. The central authorities attest a different genealogy (ending national humiliation, sovereign restoration) — a benefiting party's account — and no living external guarantor holds enforcement power over the founding text.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__balanced_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__balanced_coexistence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__balanced_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__balanced_coexistence_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.68 (interval end): the arrangement's costs concentrate in the political-security domain — interpretation overrides, electoral restructuring, security legislation with extraterritorial reach — while the commercial and daily-life layers remain substantively autonomous, yielding a high-but-bounded figure consistent with the reading's medium-epsilon expectation. Suppression is higher (0.80) because persistence since 2020 has depended on actively built enforcement machinery (a dedicated security police unit, prosecution of the opposition's leadership, rewritten electoral rules), not on participant preference. Theater_ratio (0.55) reflects the growing share of performative activity: consultations with predetermined outcomes, elections without meaningful contest, 'high degree of autonomy' rhetoric maintained while practice narrows it. Accessibility_collapse is moderate (0.50): understanding the arrangement does not reveal a workable alternative — secession is unspeakable, full assimilation is the counterfactual regime, emigration is real but costly — so alternatives narrow without vanishing. Resistance is substantial (0.65): a quarter-century of mass mobilization (2003, 2014, 2019) forced real retreats (the 2003 security bill was withdrawn), though post-2020 attenuation is visible in the flattening tail of the series. The three temporal series share one grid (t = 0, 6, 12, 17, 22, 23, 25, 27, mapping 1997–2024) so every metric is authored at every examined point; the extractiveness plateau at t=6–12 records the genuine post-2003 accommodation truce, after which the trajectory is a crisis-driven ratchet rather than an oscillation — each boundary crisis (2014, 2019–20) settled by unilateral instruments, never rolled back. Coalition power among the payer seats was real (the 2019 mobilization was the largest in the territory's history) and is precisely what the post-2020 enforcement build-up was engineered to dismantle; the suppression series traces that dismantling.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different arrangements from identical constitutional text. From the center's seat the settlement is a sovereignty-restoration instrument it generously qualifies; from the business elites' seat it is a serviceable umbrella they can arbitrage around and rarely touch; from the establishment's seat it is a career structure; from the democracy movement's seat it is a promise serially narrowed and enforced by prosecution; from the residents' seat it is a livable but narrowing inheritance; from the judiciary's seat it is a functioning court system whose final word is not final. Same-nominal-level actors diverge on exit: business elites (global capital mobility) versus residents (costly emigration) versus establishment figures (no exit without self-liquidation) — power differences within the same city are driven almost entirely by asset mobility and loyalty positioning, not formal status. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to low directionality: the central authorities sit near the beneficiary pole (they collect jurisdictional ground and face no counterparty able to impose costs on them — arbitrage-grade exit), and the business elites sit nearest zero (net collectors with full capital mobility). Declared victims map toward the target pole: the democracy movement's identity-locked exit pushes it toward the full-target end — it cannot leave the struggle without self-annihilation, so the arrangement's costs bind it completely. Ordinary residents are the deliberate complication: they appear in the victims array, and a naive derivation from victim-plus-constrained-exit would push their directionality near the full-target end, but their lived position is genuinely mixed — retained liberties, courts, and economy subsidize them even as contested-domain costs fall on them. A directionality override sets the moderate-power seat to d = 0.55 to encode that mixed position; only one stakeholder occupies the moderate atom, so the override lands on that seat alone. The judiciary's dual beneficiary/payer position is left to structural derivation: its institutional power and constrained exit place it between the elite and resident seats, which is where its situation actually sits.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical errors. Reading the arrangement as pure extraction would erase its genuine founding coordination function — the handover problem was real, the two-systems management problem is real, and the 1997–2003 accommodation era delivered measurable value to both centers and territory. Reading it as pure coordination would excuse the post-2014 ratchet, in which each crisis has been settled by unilateral instrument and the accommodation channels themselves have become partly theatrical. The founding problem (managing two systems under one sovereignty) remains live, so this is not yet a mandatrophy case; but the rising theater_ratio marks the characteristic drift path — if the negotiation layer hollows out entirely while the ceremonial layer persists, the arrangement converts toward inertial performance maintained by enforcement alone, and the corpus should expect a tangled_rope-to-piton or tangled_rope-to-snare transition signal in later measurements. The 2047 terminal provision is declared as a sunset clause because the Basic Law genuinely fixes one; this reading treats it as a renegotiable horizon rather than a dissolution date, which is precisely what distinguishes a durable negotiated order from mere scaffolding — the justification is the steady-state coexistence, not the transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (balanced_coexistence_reading) of the kernel one_country_two_systems_framework; what would the sibling readings change structurally, and where exactly does the disagreement bind?',
    'Compare the three instantiated stories'' epsilon, victim sets, and computed types: sovereignty_primacy_reading authors the arrangement as revocable delegation (higher epsilon for autonomy claims, victims = autonomy holders); autonomy_primacy_reading authors it as breached treaty guarantee (highest epsilon, victims = the territory''s population as rights-holders). The disagreement binds at the boundary-resolution mechanism: accommodation versus sovereign supremacy versus entrenched guarantee.',
    'If the sovereignty-primacy reading prevails institutionally, this reading''s accommodation channels close and its epsilon understates the arrangement; if the autonomy-primacy reading prevailed, epsilon falls toward the coordination floor. The three stories must be read as a family, never averaged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one kernel, three readings, disagreement located in the boundary-resolution mechanism.').

omega_variable(
    accommodation_channel_viability,
    'Does political accommodation remain a live resolution mechanism for boundary disputes, or did the 2020–2024 consolidation close it in favor of unilateral instruments?',
    'Observe the next boundary crisis (electoral reform, a major security case, a judicial appointment): does a negotiation phase occur with visible two-way movement, or does the center settle by directive? Count directive-to-negotiation ratios across the coming cycle.',
    'If accommodation is closed, the balanced reading''s descriptive core fails, effective extraction rises sharply, and the story trends toward snare-flavored classification; if channels reopen, the tangled_rope claim is confirmed and the post-2020 spike reads as cyclical hardening rather than regime change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accommodation_channel_viability, empirical, 'Whether the reading''s constitutive mechanism (negotiated settlement) still operates.').

omega_variable(
    civil_society_leverage_realism,
    'Does civil society retain real bargaining power through economic and international leverage, as this reading holds, or is that leverage residual memory?',
    'Test whether mass mobilization or international economic pressure still shifts policy outcomes: compare pre-2020 retreat episodes (2003 bill withdrawal) against post-2020 responses to equivalent-scale pressure. Track emigration flows, capital relocation, and sanction effects against policy movement.',
    'If leverage is gone, the reading''s balance thesis is descriptive of a past regime, the measured epsilon understates current extraction, and the payer seats'' effective directionality moves toward the full-target end; if leverage persists latently, the current suppression-heavy equilibrium is unstable and reversible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_society_leverage_realism, empirical, 'Whether the bargaining-power assumption anchoring the balanced reading is currently true.').

omega_variable(
    terminal_date_negotiability,
    'Is the 2047 terminal provision a negotiable horizon within a durable negotiated order, or a forcing date that converts the arrangement into transitional scaffolding toward a predetermined end state?',
    'Observe whether renewal discussions begin as genuine negotiation (terms contestable, both centers at the table) or as announcement of a predetermined successor arrangement; track preparatory legal work product appearing before 2040.',
    'If the date forces integration, the arrangement is scaffolding whose justification was always the transition, and the sunset clause dominates classification; if negotiable, the steady-state coexistence reading holds and the tangled_rope claim stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(terminal_date_negotiability, conceptual, 'Status of the Basic Law''s 50-year term as horizon versus deadline.').

omega_variable(
    regime_decomposition_threshold,
    'Has the arrangement changed enough across the interval (pre-2014 accommodation regime versus post-2020 enforcement regime) that the standing arrangement under assessment is actually two structurally distinct constraints wearing one label?',
    'Apply the epsilon-invariance test: if measuring extraction against the joint-declaration baseline versus the post-national-security-law baseline yields materially different epsilon values that no single referent reconciles, decompose into two linked stories (accommodation-era regime and enforcement-era regime) joined by network.affects_constraints, with the earlier story upstream.',
    'Decomposition would give each regime its own stable epsilon, its own victim set, and its own lifecycle position (the later regime plausibly a snare-trending tangled_rope); refusal to decompose would leave this story carrying a blended epsilon belonging to neither regime precisely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_decomposition_threshold, conceptual, 'Whether one story or a two-story family correctly captures the arrangement''s identity across the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__balanced_coexistence_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(one__tr_t6, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(one__tr_t12, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement(one__tr_t17, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 17, 0.33).
narrative_ontology:measurement(one__tr_t22, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 22, 0.41).
narrative_ontology:measurement(one__tr_t23, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 23, 0.48).
narrative_ontology:measurement(one__tr_t25, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(one__tr_t27, one_country_two_systems_framework__balanced_coexistence_reading, theater_ratio, 27, 0.55).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(one__be_t6, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement(one__be_t12, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(one__be_t17, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 17, 0.55).
narrative_ontology:measurement(one__be_t22, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 22, 0.6).
narrative_ontology:measurement(one__be_t23, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 23, 0.66).
narrative_ontology:measurement(one__be_t25, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(one__be_t27, one_country_two_systems_framework__balanced_coexistence_reading, base_extractiveness, 27, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(one__su_t6, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 6, 0.34).
narrative_ontology:measurement(one__su_t12, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(one__su_t17, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 17, 0.55).
narrative_ontology:measurement(one__su_t22, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 22, 0.68).
narrative_ontology:measurement(one__su_t23, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 23, 0.76).
narrative_ontology:measurement(one__su_t25, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 25, 0.79).
narrative_ontology:measurement(one__su_t27, one_country_two_systems_framework__balanced_coexistence_reading, suppression_requirement, 27, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__balanced_coexistence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__balanced_coexistence_reading, autonomy_primacy_reading).

% DUAL FORMULATION NOTE:
% Kernel family decomposition: the colloquial label 'One Country, Two Systems' covers three structurally distinct constraints — the balanced coexistence reading (this file), the sovereignty primacy reading, and the autonomy primacy reading. Each instantiates a different boundary-resolution mechanism (accommodation / sovereign supremacy / entrenched guarantee), hence a different epsilon, victim set, and failure mode. This reading sits structurally between its siblings: it inherits the coordination function the sovereignty reading acknowledges and the autonomy claims the autonomy reading defends, and its epsilon is authored only over its own referent. Family members are linked via affects_constraints; contamination propagates across the family when any reading's institutional fortunes shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(one_country_two_systems_framework__balanced_coexistence_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
