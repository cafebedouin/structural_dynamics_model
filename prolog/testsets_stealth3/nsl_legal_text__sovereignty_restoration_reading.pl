% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law — Sovereignty Restoration Reading
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   This file instantiates the sovereignty_restoration_reading of the
 *   National Security Law: the law as a legitimate sovereign security
 *   instrument that restored constitutional order after the 2019 unrest. On
 *   this reading the arrangement solves a real governance problem — a
 *   quarter-century failure to enact Basic Law Article 23, capped by months
 *   of escalating unrest that the local institutions proved unable to
 *   terminate — and its costs fall on a bounded class (movement participants,
 *   activists, opposition politicians, independent media) whom the reading
 *   processes as security risks rather than as a general-population tax. The
 *   reading therefore authors moderate extraction, high-but-normalizing
 *   suppression, and a genuine coordination function alongside an enforced
 *   transfer. Per the epsilon-invariance principle this is one member of a
 *   three-file constraint family: the democratic_enclosure and
 *   jurisdictional_capture siblings are separate stories with their own
 *   epsilon values, victim sets, and classifications, linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - cpg_authority: agenda-setter (institutional/arbitrage) — drafted and imposed the law via the Annex III mechanism, sets enforcement priorities and interpretations, collects the arrangement's principal gains
 *   - - hksar_government: beneficiary with secondary agenda-setting role (institutional/constrained) — administers enforcement locally, collects restored governability, bears sanction and autonomy costs
 *   - - stability_seeking_residents: beneficiary (moderate/constrained) — recovered usable streets and predictable daily life; bear diffuse costs in narrowed speech and schooling norms
 *   - - hong_kong_business_community: beneficiary (powerful/arbitrage) — collected the stability premium; capital hedged abroad lets it keep the gains without the political exposure
 *   - - protest_movement_participants: payer (powerless/trapped) — processed as security risks; facing prosecution, detention, surrendered travel documents
 *   - - pro_democracy_activists: payer (moderate/identity_locked) — civic identity fused with the cause; choose prison, disqualification, or exile advocacy over silence
 *   - - opposition_politicians: payer (moderate/constrained) — disqualified by oath vetting or detained; institutional access eliminated, arena gone
 *   - - independent_journalists: payer (moderate/constrained) — newsroom closures and sedition cases; operating under self-censorship equilibria
 *   - - unconsulted_hk_public: excluded (organized/constrained) — the law's subjects were never in the drafting room; no local vote, consultation, or authorization
 *   - - joint_declaration_signatories: observer (institutional/analytical) — monitor, grade, and sanction from outside; no enforcement seat inside the arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.58).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.72).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law — Sovereignty Restoration Reading").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional/political").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, '81a65d6b-34ea-4e84-a973-d85b396ed539').
narrative_ontology:cs_kernel_codification('81a65d6b-34ea-4e84-a973-d85b396ed539', fixed_text).
narrative_ontology:cs_authority_grounding('81a65d6b-34ea-4e84-a973-d85b396ed539', lineage).
narrative_ontology:cs_interpretation_layer_present('81a65d6b-34ea-4e84-a973-d85b396ed539').
narrative_ontology:cs_reading_relation('81a65d6b-34ea-4e84-a973-d85b396ed539', nsl_legal_text__democratic_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('81a65d6b-34ea-4e84-a973-d85b396ed539', nsl_legal_text__jurisdictional_capture_reading, forecloses).
narrative_ontology:cs_axiom('81a65d6b-34ea-4e84-a973-d85b396ed539', foundational, target_class_limited_to_genuine_security_threats).
narrative_ontology:cs_axiom_status(target_class_limited_to_genuine_security_threats, holdable).
narrative_ontology:cs_axiom_grounding('81a65d6b-34ea-4e84-a973-d85b396ed539', target_class_limited_to_genuine_security_threats, empirically_contingent).
narrative_ontology:cs_axiom('81a65d6b-34ea-4e84-a973-d85b396ed539', foundational, nsl_completes_basic_law_design_via_annex_iii).
narrative_ontology:cs_axiom_status(nsl_completes_basic_law_design_via_annex_iii, holdable).
narrative_ontology:cs_axiom_grounding('81a65d6b-34ea-4e84-a973-d85b396ed539', nsl_completes_basic_law_design_via_annex_iii, conventional).
narrative_ontology:cs_axiom('81a65d6b-34ea-4e84-a973-d85b396ed539', secondary, restored_order_serves_collective_welfare).
narrative_ontology:cs_axiom_status(restored_order_serves_collective_welfare, holdable).
narrative_ontology:cs_axiom_grounding('81a65d6b-34ea-4e84-a973-d85b396ed539', restored_order_serves_collective_welfare, instrumental).
narrative_ontology:cs_reference_frame('81a65d6b-34ea-4e84-a973-d85b396ed539', annex_iii_constitutional_continuity).
narrative_ontology:cs_drift_state('81a65d6b-34ea-4e84-a973-d85b396ed539', contemporary_post_local_article23_legislation, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('81a65d6b-34ea-4e84-a973-d85b396ed539', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, cpg_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hksar_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, stability_seeking_residents).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hong_kong_business_community).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, protest_movement_participants).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, opposition_politicians).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, independent_journalists).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, comprehensive_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__sovereignty_restoration_reading, sovereign_security_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and imposed the law by decision inserted through the Basic Law Annex III mechanism after judging local legislation would never arrive; chairs the Committee for Safeguarding National Security through an appointed advisor; sets enforcement priorities, issues interpretations, and directs cases of concern. Collects the arrangement's principal gains: closure of a decades-long security gap, elimination of an organized challenge to sovereignty, and demonstrated command over the territory's political space. It authored the instrument, faces no constraint it did not write, and extended the architecture with the 2024 local Article 23 legislation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, cpg_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Administers enforcement locally through the National Security Department of the police, designated judges, and the Secretary for Justice; collects restored governability, street calm, and renewed administrative authority after 2019. Also bears costs a purely beneficiary reading would miss: officials sanctioned abroad, diminished international standing, and autonomy questions raised by each directive received from above. It executes the center's priorities and cannot decline them without replacement, so its exit is constrained despite its enforcement seat.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hksar_government, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__sovereignty_restoration_reading, hksar_government, agenda_setter).

% Residents wearied by 2019's disruptions — blocked transport, vandalized districts, a year of confrontation — regained usable streets, functioning transit, and predictable daily life. They bear diffuse indirect costs: narrowed speech norms, patriotic curriculum for their children, and a political voice most did not exercise and now cannot. Emigration was a real exit and many took it; those who remain are held by family, property, and livelihood, making their effective position constrained.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, stability_seeking_residents, beneficiary,
    moderate, biographical, constrained, regional).

% Financial and professional services gained the end of disruption and a restored predictability premium; exchanges, banks, and property interests publicly welcomed the return of order. Capital is mobile and many firms hedge operations to Singapore while keeping Hong Kong books — an arbitrage position that lets them collect the stability dividend without bearing the political exposure the arrangement generates.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hong_kong_business_community, beneficiary,
    powerful, biographical, arbitrage, global).

% Those who marched, staffed roadblocks, or organized in 2019-2020 now face liability under the new definitions: unauthorized-assembly prosecutions, charges brought for slogans and flags, long pre-trial detention. Under this reading they are processed as security risks rather than negotiated counterparts. Their exit is trapped — some surrendered travel documents as bail conditions, trials are pending, and the window for leaving before arrest has closed for anyone already charged.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, protest_movement_participants, payer,
    powerless, biographical, trapped, regional).

% Organizers, district councillors, and movement veterans whose civic identity is constituted by the cause: for them exit means exile or silence, and many chose prison, disqualification, or overseas advocacy over either. Several are subjects of overseas bounty notices. The identity fusion is the binding mechanism — the arrangement costs them most precisely because self-concept and cause coincide, so they keep bearing costs that peers without that fusion fled.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    moderate, biographical, identity_locked, regional).

% Legislators and candidates disqualified through oath vetting or detained in the '47 democrats' case; parties dissolved or hollowed out. Their institutional access is gone and the electoral rules that replaced them exclude their platform. Remaining options are quietism, retirement, or exile-based commentary — resources and mobility some retain, but no arena in which to use them.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, opposition_politicians, payer,
    moderate, biographical, constrained, regional).

% Newsroom closures — the largest independent paper raided and forced to cease within a year of enactment — sedition charges against reporters and commentators, and visa and licensing pressure shrank the independent press. Remaining journalists operate under self-censorship equilibria; exit is emigration or career change, both costly. They carry the arrangement's information-narrowing costs, which this reading accounts as collusion-enforcement rather than as harm.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, independent_journalists, payer,
    moderate, biographical, constrained, regional).

% The law was drafted in Beijing and promulgated by insertion into Annex III without local legislative deliberation, consultation, or any vote the public's representatives could cast. The people who live under it indefinitely were structurally absent from the drafting table; their preferences entered only afterward, through surveys and emigration flows, never through authorization. They would have objected to the imposition method even where many supported the outcome.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, unconsulted_hk_public, excluded,
    organized, biographical, constrained, regional).

% Foreign governments — notably the United Kingdom as co-signatory of the 1984 Joint Declaration, and states engaging the ICCPR review cycle — monitor the law's operation, publish assessments, impose visa and sanction measures, and lodge formal objections. They observe and grade the arrangement but hold no enforcement seat inside it; their leverage is reputational and reciprocal, exercised from outside the legal order they are assessing.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, joint_declaration_signatories, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, cpg_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a governance gap left open for twenty-three years: provides unified statutory definitions of secession, subversion, terrorism, and collusion with foreign forces where none existed locally, restores the public-order capacity that 2019 exhausted, and gives businesses and residents a single authoritative security framework instead of improvised ad-hoc policing.
% TRANSFER_FUNCTION: Moves political freedom of action — speech, assembly, organization, press — from pro-democracy actors and opposition institutions to the central state's security apparatus; moves enforcement discretion to the national security police and designated judges; moves predictability and order to residents and business.
% ABSENT_VOICES: The unconsulted_hk_public seat: the law's subjects were never in the room — no local consultation, no legislative vote, no ratification; the pan-democratic representatives who would have objected were excluded from the process by design, and detained defendants awaiting trial have no effective voice in the arrangement's ongoing refinement. Treaty bodies and foreign signatories comment from outside without standing inside.
% DISAPPEARANCE_RATIONALE: Overnight repeal would reopen the contest it froze: surviving networks and exile cohorts would rebuild organized opposition, in-flight prosecutions would lose their legal basis, the 2024 local Article 23 legislation would stand orphaned, and the center would face again the sovereignty-without-security-control problem it concluded in 2020 it could no longer tolerate. Business stability premiums and residential calm would be repriced against renewed-unrest risk within weeks.
% FOUNDING_PROBLEM: After 2019's months of escalating unrest — airport shutdowns, petrol bombs, a stabbed legislator, open 'mutual destruction' rhetoric — overlaid on twenty-three years of unenacted Basic Law Article 23, Beijing judged that it held sovereignty over Hong Kong without security control of it, and that local institutions would never close the gap voluntarily.
% FOUNDING_PROBLEM_CORROBORATION: The historical problem is corroborated from outside the benefiting parties: contemporaneous international media documentation of the unrest's scale and violence, insured-loss and retail-sales data showing the economic damage, and foreign-government statements at the time acknowledging its severity. The continuing-liveness claim — persistent foreign interference, latent separatism — is attested mainly by the CPG and HKSAR governments themselves, with partial external support from security analysts and sanctioned-entity listings; critics abroad dispute the threat's nature and scale, so liveness is corroborated thinly and openly contested.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate (0.58 at interval end) because the cost-bearing set is concentrated: the general population experiences the arrangement predominantly as restored order, while severe costs land on identifiable political classes. The series rises from 0.42 as enforcement broadened from the four named crimes into sedition prosecutions, oath vetting, and media closure — accumulation, but bounded. Suppression (0.72) is authored as a raw structural property, unscaled by power or scope; its temporal series is deliberately non-monotonic: a hard early ratchet (mass arrests, the Apple Daily raid and closure, the '47 democrats' case) peaking around month 24, then declining as open resistance collapsed and enforcement shifted to preventive, routine modes — chilling effects doing work the active machinery no longer needs to. Theater rises steadily (0.14 to 0.26) as functional security targets depleted and performative loyalty maintenance (oath renewals, national security education, institutional pledge displays) grew as a share of activity. All three series run on one shared six-point grid so no metric's row is backfilled from another's end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute very different arrangements from the same legal text. From the agenda-setter seat the structure is a completed constitutional repair it authored, with arbitrage-grade control over revision — the coordination side dominates. From the trapped and identity_locked payer seats the same structure operates as open-ended personal jeopardy with no exit priced in their favor. Between the beneficiary seats the divergence runs on exit quality: the business community's arbitrage position converts the arrangement into near-pure subsidy, while constrained residents collect real but partial gains against diffuse costs they did not choose. The engine computes these per-seat classifications from the structural data; this reading's authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: cpg_authority sits near the full-beneficiary end (authored the instrument, collects its principal gains, faces no constraint it did not write); hksar_government derives low d as a declared beneficiary but its constrained exit and dual enforcement role place it short of full subsidy; stability_seeking_residents sit mildly beneficiary-side with diffuse costs; the business community's arbitrage exit pushes it nearest the beneficiary pole of any seat. All four payer groups derive high d, amplified by trapped and identity_locked exit profiles — the activists' identity lock places them at the extreme target end, since exit carries identity-death rather than mere cost. No directionality_overrides are authored: the exit-option differentiation (trapped versus arbitrage versus constrained) gives the derivation chain everything it needs to separate seats that share power atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline cuts both ways for this reading. Declaring the victim set blocks the reading's own temptation to launder the arrangement as pure rope — the transfer is real, enforced, and asymmetric, whatever its justification. Conversely, declaring the coordination function and the beneficiary set blocks the sibling readings' move to erase the genuine order-restoration achievement and read the whole structure as cover. On obsolescence: the founding problem is authored live under this reading, so mandatrophy is not resolved — but the rising theater_ratio is the designated watch item. If the threat recedes while enforcement and loyalty performance persist, the arrangement drifts toward maintained-theater operation, and the mismatch consumer (founding_problem_status x disappearance_verdict) is the tripwire for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the nsl_legal_text kernel — the sovereignty_restoration_reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparison against the sibling files (nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading): the enclosure sibling expands the victim set from security-threat-designated actors to dissent-as-such and raises epsilon sharply; the jurisdictional-capture sibling adds common-law institutional autonomy to the victim set and relocates extraction onto the legal system itself.',
    'The disagreement is located in two structural elements: the extension of the target class (does enforcement track genuine security threats, or does the threat category coincide with political dissent?) and the fate of common law adjudication (preserved venue versus transplanted substance). Resolving either element toward a sibling''s premise dissolves this reading''s distinct epsilon, victim set, and classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this file instantiates one of three live readings of the NSL text; sibling readings are separate constraints, not averaged into this one.').

omega_variable(
    threat_dissent_boundary,
    'Does enforcement under the law in fact track conduct constituting genuine security threats (operational planning, violence, foreign-direction), or has the threat category expanded until it covers ordinary political dissent?',
    'Systematic coding of prosecuted cases against the four statutory crime definitions: proportion of cases involving operational capability or foreign direction versus pure advocacy, slogan-display, or organization-building; trajectory of the sedition caseload relative to the four named crimes.',
    'If the boundary holds, this reading''s moderate epsilon and bounded victim set are stable and the tangled_rope classification stands. If the boundary dissolves, this reading collapses into the democratic_enclosure sibling: epsilon rises sharply, the victim set expands to dissent-as-such, and the computed classification shifts snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_dissent_boundary, conceptual, 'The load-bearing boundary of this reading: where the security-threat category ends and political dissent begins.').

omega_variable(
    extraction_accumulation_trajectory,
    'Is the moderate extraction level stable, or does cost-bearing accumulate as enforcement broadens from the four named crimes into sedition, cultural regulation, elections administration, and economic conduct?',
    'Longitudinal tracking of the widening set of cost-bearing actors: new professional and cultural sectors drawn under enforcement attention, expansion of designated-judge remit, growth of preventive measures (bounties, border alerts, school curricula) relative to case-based enforcement.',
    'Continued accumulation with a widening victim set would date a tangled_rope-to-snare transition and validate the enclosure sibling''s structural delta; a plateau at moderate levels would confirm this reading''s bounded-extraction profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_accumulation_trajectory, empirical, 'Whether the moderate epsilon holds or accumulates as the enforcement perimeter widens.').

omega_variable(
    founding_problem_liveness_corroboration,
    'Is the founding problem (persistent foreign interference and latent separatism requiring the law''s continued full operation) still live, or is liveness asserted mainly by the arrangement''s own administrators?',
    'External audit of the continuing-threat claim: independent security analyses distinguishing documented foreign-direction cases from asserted ones; comparison of threat assessments issued by parties with no stake in the law''s continuation against official characterizations.',
    'If liveness fails external audit, the arrangement persists past its mandate and the theater_ratio rise signals onset of piton dynamics; if corroborated, the live founding problem supports the reading''s steady-state justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness_corroboration, empirical, 'Whether the founding problem''s continuing liveness survives corroboration from outside the benefiting parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl_sov_tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(nsl_sov_tr_t0, observed).
narrative_ontology:measurement(nsl_sov_tr_t12, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement_basis(nsl_sov_tr_t12, observed).
narrative_ontology:measurement(nsl_sov_tr_t24, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement_basis(nsl_sov_tr_t24, observed).
narrative_ontology:measurement(nsl_sov_tr_t36, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 36, 0.22).
narrative_ontology:measurement_basis(nsl_sov_tr_t36, observed).
narrative_ontology:measurement(nsl_sov_tr_t48, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 48, 0.24).
narrative_ontology:measurement_basis(nsl_sov_tr_t48, observed).
narrative_ontology:measurement(nsl_sov_tr_t60, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 60, 0.26).
narrative_ontology:measurement_basis(nsl_sov_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(nsl_sov_be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(nsl_sov_be_t0, observed).
narrative_ontology:measurement(nsl_sov_be_t12, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(nsl_sov_be_t12, observed).
narrative_ontology:measurement(nsl_sov_be_t24, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement_basis(nsl_sov_be_t24, observed).
narrative_ontology:measurement(nsl_sov_be_t36, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 36, 0.56).
narrative_ontology:measurement_basis(nsl_sov_be_t36, observed).
narrative_ontology:measurement(nsl_sov_be_t48, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 48, 0.57).
narrative_ontology:measurement_basis(nsl_sov_be_t48, observed).
narrative_ontology:measurement(nsl_sov_be_t60, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(nsl_sov_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl_sov_su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(nsl_sov_su_t0, observed).
narrative_ontology:measurement(nsl_sov_su_t12, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement_basis(nsl_sov_su_t12, observed).
narrative_ontology:measurement(nsl_sov_su_t24, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement_basis(nsl_sov_su_t24, observed).
narrative_ontology:measurement(nsl_sov_su_t36, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 36, 0.77).
narrative_ontology:measurement_basis(nsl_sov_su_t36, observed).
narrative_ontology:measurement(nsl_sov_su_t48, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 48, 0.75).
narrative_ontology:measurement_basis(nsl_sov_su_t48, observed).
narrative_ontology:measurement(nsl_sov_su_t60, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(nsl_sov_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the nsl_legal_text kernel per the epsilon-invariance principle. The colloquial label 'the NSL' conflates three structurally distinct claims: (1) this file — a sovereign security instrument with a genuine coordination function and bounded, opposition-concentrated extraction (tangled_rope profile, moderate epsilon); (2) the democratic_enclosure sibling — the same text read as permanent closure of democratic space, with dissent-as-such in the victim set and sharply higher epsilon; (3) the jurisdictional_capture sibling — the text read as transplantation eroding common law autonomy, with the legal system itself among the cost-bearers. The upstream claim (legitimate restoration) is cited as warrant by the arrangement's defenders; the downstream siblings contest its extension. Each story carries its own epsilon, beneficiaries, victims, and claimed type; this file links both siblings through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
