% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__crown_cession_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__crown_cession_reading, []).

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
 *   constraint_id: treaty_authority_cession__crown_cession_reading
 *   human_readable: Crown Cession Reading of the Treaty of Waitangi (English-Text Sovereignty Doctrine)
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) exists in two texts: the English draft
 *   recording the cession of 'all the rights and powers of sovereignty,' and
 *   te Tiriti o Waitangi in te reo Maori, which grants 'kawanatanga'
 *   (governance) while guaranteeing 'tino rangatiratanga' (full chiefly
 *   authority) over lands and taonga. This story instantiates ONE reading of
 *   that contested instrument — the crown_cession_reading — under which the
 *   English text controls, kawanatanga carries the full sovereignty of the
 *   English 'sovereignty,' and the treaty therefore completed a legal
 *   cession: Maori authority dissolved into Crown subject-hood, and
 *   subsequent land alienation proceeded under valid Crown title. The
 *   standing arrangement under contest — and the epsilon referent — is that
 *   Crown-sovereignty-and-land-title regime as this reading constitutes and
 *   maintains it, assessed as it actually bears on the parties, not as the
 *   sibling readings would replace it. The sibling readings
 *   (rangatiratanga_retention_reading, retrospective_snare_exposure) are
 *   separate constraint files linked through network.affects_constraints.
 *   Claim and metrics are authored independently: the claimed type is my
 *   structural assessment of the arrangement; the metrics describe its
 *   documented operation.
 *
 * KEY AGENTS:
 *   - the_crown: primary beneficiary and agenda-setter (institutional/arbitrage) — collects sovereignty, radical title, and the land fund; reframes the doctrine at will
 *   - maori_iwi_and_hapu: primary paying party (organized/trapped) — bears authority subordination and land alienation; cannot exit the legal order
 *   - colonial_superior_courts: interpretive agenda-setter (institutional/constrained) — fixes the English text as controlling from Wi Parata onward
 *   - native_land_court: conversion administrator (institutional/constrained) — individualises and conveys customary land
 *   - british_settler_community: secondary beneficiary (organized/mobile) — receives title, markets, and self-government
 *   - maori_text_interpreters: excluded voice (powerless/identity_locked) — holds the inadmissible Maori-text reading
 *   - waitangi_tribunal: analytical observer (institutional/analytical) — investigates and reports from outside the doctrine's formation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.68).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.45).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Crown Cession Reading of the Treaty of Waitangi (English-Text Sovereignty Doctrine)").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, '05ba5a89-a12e-429f-9fcd-80e0870dc335').
narrative_ontology:cs_kernel_codification('05ba5a89-a12e-429f-9fcd-80e0870dc335', fixed_text).
narrative_ontology:cs_authority_grounding('05ba5a89-a12e-429f-9fcd-80e0870dc335', lineage).
narrative_ontology:cs_interpretation_layer_present('05ba5a89-a12e-429f-9fcd-80e0870dc335').
narrative_ontology:cs_reading_relation('05ba5a89-a12e-429f-9fcd-80e0870dc335', treaty_authority_cession__rangatiratanga_retention_reading, forecloses).
narrative_ontology:cs_reading_relation('05ba5a89-a12e-429f-9fcd-80e0870dc335', treaty_authority_cession__retrospective_snare_exposure, forecloses).
narrative_ontology:cs_axiom('05ba5a89-a12e-429f-9fcd-80e0870dc335', foundational, english_text_controls_treaty_meaning).
narrative_ontology:cs_axiom_status(english_text_controls_treaty_meaning, holdable).
narrative_ontology:cs_axiom_grounding('05ba5a89-a12e-429f-9fcd-80e0870dc335', english_text_controls_treaty_meaning, conventional).
narrative_ontology:cs_axiom('05ba5a89-a12e-429f-9fcd-80e0870dc335', foundational, kawanatanga_equals_full_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_equals_full_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('05ba5a89-a12e-429f-9fcd-80e0870dc335', kawanatanga_equals_full_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('05ba5a89-a12e-429f-9fcd-80e0870dc335', secondary, cession_completes_radical_title_transfer).
narrative_ontology:cs_axiom_status(cession_completes_radical_title_transfer, holdable).
narrative_ontology:cs_axiom_grounding('05ba5a89-a12e-429f-9fcd-80e0870dc335', cession_completes_radical_title_transfer, conventional).
narrative_ontology:cs_reference_frame('05ba5a89-a12e-429f-9fcd-80e0870dc335', indivisible_crown_sovereignty).
narrative_ontology:cs_drift_state('05ba5a89-a12e-429f-9fcd-80e0870dc335', post_waitangi_tribunal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('05ba5a89-a12e-429f-9fcd-80e0870dc335', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, the_crown).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, british_settler_community).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi_and_hapu).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_text_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, native_land_court).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, indivisible_crown_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, parliamentary_supremacy_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, radical_title_on_cession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts and administers the cession reading: proclaims sovereignty, charters the courts, holds radical title to unalienated land, and draws the land fund that finances colonial government. Adjusts interpretive posture when convenient — from treating the treaty as a nullity in the Wi Parata era to reciting 'Treaty principles' today — while holding the sovereignty core fixed. Exit is effectively unlimited: it can reframe the doctrine by statute or litigation posture without leaving the arrangement.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, the_crown, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, the_crown, beneficiary).

% Decide what the treaty means. From Wi Parata (1877) through the twentieth century they adopt the English text, hold the treaty unenforceable as a contract, and treat ceded sovereignty as beyond inquiry. They gain jurisdiction over Maori affairs and land disputes from the doctrine they articulate; precedent binds them to the line they drew.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, colonial_superior_courts, agenda_setter,
    institutional, biographical, constrained, national).

% Administers the conversion of customary tenure into individualised titles saleable to settlers, taking fees and salaries from a caseload the doctrine generates. Sets the practical agenda of alienation — who may sell, in what ten-owner fractions — while depending on the doctrine for its existence.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, native_land_court, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, native_land_court, beneficiary).

% Receives land title, market access, and self-government under the doctrine. Migration was voluntary and remained open — a settler dissatisfied with the arrangement could leave in a way the tangata whenua could not. Organised politically through provincial and colonial assemblies that shaped land policy.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, british_settler_community, beneficiary,
    organized, biographical, mobile, national).

% Bear the arrangement's costs: authority over lands and taonga subordinated to Crown law, roughly nine-tenths of the 1840 Maori land estate alienated by 1939, and adjudication moved into forums applying the English text. Their land is immovable and their peoplehood tied to it; exit from the jurisdiction's legal order is not available. They resist through the Kingitanga, armed defence in the 1860s wars, petitions, and a century of litigation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi_and_hapu, payer,
    organized, generational, trapped, national).

% Hold and transmit the Maori-text reading — kawanatanga as governorship, tino rangatiratanga retained — through whakapapa, te reo, and tikanga. Their interpretation was ruled inadmissible in the forums deciding the doctrine, leaving them objectors outside the conversation; their interpretive identity is fused with the language and genealogy the reading displaces, so abandoning it is not a live option.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_text_interpreters, payer,
    powerless, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, maori_text_interpreters, excluded).

% Investigates Crown conduct against the treaty's principles since 1975 (retrospectively since 1985) and reports findings — including the 2014 finding that rangatiratanga was retained in 1840. It recommends but does not bind; it observes the doctrine's operation from an institutional seat with analytical distance.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, the_crown).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single governing authority (kawanatanga) over a fragmented frontier: one law for settler-Maori transactions, a monopoly framework for land purchase, protection channels for Maori subjects, and a counterweight to rival imperial designs — problems that before 1840 had no common adjudicator.
% TRANSFER_FUNCTION: Moves legislative authority and customary land out of iwi and hapu hands into Crown title and settler freehold — first through Crown pre-emption and negotiated purchase, then through Native Land Court individualisation and purchase; and moves adjudication of Crown-Maori disputes into Crown-chartered courts applying the English text.
% ABSENT_VOICES: The rangatira who signed te Tiriti and their descendants read the Maori text — kawanatanga as governorship of settlers, tino rangatiratanga retained — but that reading was inadmissible in the courts that fixed the doctrine's meaning from Wi Parata onward; te reo speakers and tikanga authorities stood outside the rooms where the English text was made controlling. Their objections survive in petitions (Kotahitanga, the Maori Council), Tribunal submissions, and protest rather than in the doctrine's formation.
% DISAPPEARANCE_RATIONALE: If the cession reading vanished overnight, parliamentary sovereignty, all derived Crown title, and the property-law settlement of New Zealand would lose their doctrinal foundation: land tenure, local government, and the legitimacy of every statute passed under Crown authority would be thrown open to challenge, and the constitutional question the treaty deferred — who holds authority — would reopen immediately.
% FOUNDING_PROBLEM: The 1830s New Zealand frontier: British subjects arriving without any governing authority, musket-war destabilisation among iwi, humanitarian pressure over land dealings, and rival foreign designs (Baron de Thierry, French interest) — the problem was establishing a workable governing presence able to control settlers and regularise land transactions.
% FOUNDING_PROBLEM_CORROBORATION: The Crown attests the cession framing (sovereignty transferred, problem solved). Outside the benefiting parties: Waitangi Tribunal reports — notably He Whakaputanga me te Tiriti (2014) — find the chiefs consented to governorship, not cession, corroborating that the founding problem was settler governance rather than sovereignty transfer; academic historiography (Orange, Ward, Walker) and the survival of Maori-text petitions from 1840 onward independently attest the contested status. No source outside the benefiting parties attests the full-cession framing as the founders' shared understanding.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__crown_cession_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__crown_cession_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high throughout and peaked during the Native Land Court era (0.87 at t=65): the reading converted a guarantee-bearing instrument into a title-clearing device, and roughly nine-tenths of the 1840 Maori land estate had left Maori ownership by 1939. Suppression tracks the enforcement arc — military in the 1860s wars, judicial foreclosure from Wi Parata (1877), administrative compulsion in the court era — peaking at 0.78 and decaying after 1975 as statutory channels replaced coercion; the current scalar (0.45) reflects a doctrine now maintained mainly by precedent and parliamentary supremacy rather than force. Theater rises nearly monotonically (0.14 to 0.55): protection guarantees were honoured in recital while overridden in substance, and the contemporary 'principles' vocabulary performs partnership while the cession core stands untouched. Accessibility collapse is moderate (0.55): the rival reading never died — it survived in petitions, the Kingitanga, and Maori-language scholarship, and partially re-entered legal discourse after 1975. Resistance is substantial (0.6) and continuous across the whole interval. All three series share one time grid ([0, 37, 65, 90, 135, 145, 175, 185]); the final values match the base_properties scalars. Receipts accrue to the Crown seat (gain_flow: the_crown) — the land fund financed colonial government — and fixing is prohibitive: reversal would require reconstituting the constitutional order itself.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat the arrangement computes as its own legitimate foundation: near-beneficiary directionality, minimal effective burden, a settled constitutional fact. From the iwi and hapu seats the same structure computes as enforced dispossession: near-full-target directionality amplified by trapped exit. The institutional intermediaries (superior courts, land court) sit between — administering the doctrine, collecting salaries and fees, bound by their own precedents. The engine derives these per-seat classifications from the structural data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the_crown (collects sovereignty, title, and revenue; arbitrage-grade exit — it can reframe the doctrine by statute or litigation posture — placing it near the beneficiary end) and british_settler_community (received title and self-government; mobile exit keeps d low). Victims: maori_iwi_and_hapu (bear subordination and alienation; trapped — land immovable, peoplehood tied to it — placing them near the full-target end) and maori_text_interpreters (their reading ruled inadmissible; identity_locked through reo, whakapapa, and tikanga). No neutral third party sits symmetric here; the waitangi_tribunal observes from an analytical seat. Scope is national, which moderately amplifies effective extraction on the paying seats: verifying consent across two languages and a century of practice is hard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — governing an ungovernable settler frontier — was substantially solved by the 1870s, and its solution was absorbed into ordinary state function; what persists past that absorption is the reading's exclusionary work (foreclosing the Maori-text reading) without a coordinating justification unique to it. The tangled-rope classification prevents mislabeling in both directions: a pure-extraction label would erase the genuine governance coordination the instrument performed (which even the Tribunal credits), while a pure-coordination label would erase the extinguishment and alienation the same instrument authorised. The rising theater ratio marks the transition from functional enforcement toward ceremonial maintenance; whether that ends in inertial performance or re-hardening is left open (omega: enforcement_revival_pressure). Mandatrophy is not resolved: the arrangement remains ACTIVE.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the crown_cession_reading of kernel treaty_authority_cession; what structural changes would instantiating the sibling readings (rangatiratanga_retention_reading, retrospective_snare_exposure) produce, and where exactly is the disagreement located?',
    'Comparative compilation of the sibling story files against this one: diff the beneficiary/victim sets, epsilon, and cs_structure axioms. The disagreement is located in the 1840 semantic force of ''kawanatanga'' in te reo Maori and in the legal effect the treaty is held to have completed.',
    'Under the retention reading the same instrument coordinates rather than extinguishes: victims convert to rights-holders, epsilon falls sharply, and the type moves toward rope/scaffold. Under the snare-exposure reading the textual divergence itself is the mechanism and epsilon rises toward pure extraction. Per-seat classifications computed from this file''s structural data are valid only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame indexing: one reading of a contested kernel, with sibling readings as separate constraints.').

omega_variable(
    consent_validity_asymmetry,
    'Could the rangatira who signed te Tiriti o Waitangi have assented, under 1840 semantic conditions, to the full sovereignty cession the English text records?',
    'Philological reconstruction of 1840 te reo usage of kawanatanga and rangatiratanga, missionary drafting correspondence, and oral testimony gathered by the Waitangi Tribunal, contrasted with contemporaneous Crown drafting intent.',
    'If informed assent to full cession is implausible, the reading''s legitimacy premise fails, effective extraction on the trapped seats rises, and the arrangement trends toward pure extraction; if assent holds, part of the measured cost is the price of the agreement itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_validity_asymmetry, empirical, 'Whether the cession premise survives 1840 semantic reconstruction.').

omega_variable(
    extinguishment_degree,
    'Did the cession reading extinguish Maori customary authority outright, or subordinate it beneath Crown authority?',
    'Doctrine-and-practice survey: judicial treatment of tikanga, surviving rangatira functions, and statutory recognition episodes across the interval.',
    'Full extinguishment widens the victim set and raises the burden on trapped seats; mere subordination leaves a residual coordination layer and tempers the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinguishment_degree, conceptual, 'Degree of customary-authority displacement produced by the reading.').

omega_variable(
    enforcement_revival_pressure,
    'Is the long decline in suppression_requirement a permanent decay of the reading''s enforcement, or a trough before revival through statutory reaffirmations of unqualified Crown sovereignty?',
    'Track the legislative and litigation agenda: bills reaffirming indivisible sovereignty, court treatment of tikanga, and uptake rates for Tribunal recommendations.',
    'Revival re-hardens suppression and reverses the theater-driven drift; continued decay pushes the arrangement toward inertia-maintained performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_revival_pressure, empirical, 'Direction of the enforcement trajectory after the post-1975 decline.').

omega_variable(
    ceded_fact_vs_constructed_allocation,
    'Is Crown sovereignty under this reading a settled constitutional fact beyond revision, or a constructed allocation maintained by continuing interpretive choice from which identifiable parties collect?',
    'Counterfactual institutional analysis: whether any court or legislature could revisit the cession premise at acceptable cost, and who would collect from each outcome.',
    'If constructed, the arrangement is a defended allocation with concentrated beneficiaries (false-summit candidate) rather than constitutional bedrock; if settled fact, the measured extraction is historical residue rather than live rent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ceded_fact_vs_constructed_allocation, conceptual, 'Naturalness ambiguity of the cession premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treaty_cession_reading_tr_t0, treaty_authority_cession__crown_cession_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(treaty_cession_reading_tr_t0, observed).
narrative_ontology:measurement(treaty_cession_reading_tr_t37, treaty_authority_cession__crown_cession_reading, theater_ratio, 37, 0.28).
narrative_ontology:measurement_basis(treaty_cession_reading_tr_t37, observed).
narrative_ontology:measurement(treaty_cession_reading_tr_t65, treaty_authority_cession__crown_cession_reading, theater_ratio, 65, 0.34).
narrative_ontology:measurement_basis(treaty_cession_reading_tr_t65, observed).
narrative_ontology:measurement(treaty_cession_reading_tr_t90, treaty_authority_cession__crown_cession_reading, theater_ratio, 90, 0.44).
narrative_ontology:measurement_basis(treaty_cession_reading_tr_t90, observed).
narrative_ontology:measurement(treaty_cession_reading_tr_t135, treaty_authority_cession__crown_cession_reading, theater_ratio, 135, 0.41).
narrative_ontology:measurement_basis(treaty_cession_reading_tr_t135, observed).
narrative_ontology:measurement(treaty_cession_reading_tr_t145, treaty_authority_cession__crown_cession_reading, theater_ratio, 145, 0.47).
narrative_ontology:measurement_basis(treaty_cession_reading_tr_t145, observed).
narrative_ontology:measurement(treaty_cession_reading_tr_t175, treaty_authority_cession__crown_cession_reading, theater_ratio, 175, 0.53).
narrative_ontology:measurement_basis(treaty_cession_reading_tr_t175, observed).
narrative_ontology:measurement(treaty_cession_reading_tr_t185, treaty_authority_cession__crown_cession_reading, theater_ratio, 185, 0.55).
narrative_ontology:measurement_basis(treaty_cession_reading_tr_t185, observed).

% Extraction over time
narrative_ontology:measurement(treaty_cession_reading_be_t0, treaty_authority_cession__crown_cession_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement_basis(treaty_cession_reading_be_t0, observed).
narrative_ontology:measurement(treaty_cession_reading_be_t37, treaty_authority_cession__crown_cession_reading, base_extractiveness, 37, 0.7).
narrative_ontology:measurement_basis(treaty_cession_reading_be_t37, observed).
narrative_ontology:measurement(treaty_cession_reading_be_t65, treaty_authority_cession__crown_cession_reading, base_extractiveness, 65, 0.87).
narrative_ontology:measurement_basis(treaty_cession_reading_be_t65, observed).
narrative_ontology:measurement(treaty_cession_reading_be_t90, treaty_authority_cession__crown_cession_reading, base_extractiveness, 90, 0.84).
narrative_ontology:measurement_basis(treaty_cession_reading_be_t90, observed).
narrative_ontology:measurement(treaty_cession_reading_be_t135, treaty_authority_cession__crown_cession_reading, base_extractiveness, 135, 0.77).
narrative_ontology:measurement_basis(treaty_cession_reading_be_t135, observed).
narrative_ontology:measurement(treaty_cession_reading_be_t145, treaty_authority_cession__crown_cession_reading, base_extractiveness, 145, 0.73).
narrative_ontology:measurement_basis(treaty_cession_reading_be_t145, observed).
narrative_ontology:measurement(treaty_cession_reading_be_t175, treaty_authority_cession__crown_cession_reading, base_extractiveness, 175, 0.69).
narrative_ontology:measurement_basis(treaty_cession_reading_be_t175, observed).
narrative_ontology:measurement(treaty_cession_reading_be_t185, treaty_authority_cession__crown_cession_reading, base_extractiveness, 185, 0.68).
narrative_ontology:measurement_basis(treaty_cession_reading_be_t185, observed).

% Suppression requirement over time
narrative_ontology:measurement(treaty_cession_reading_su_t0, treaty_authority_cession__crown_cession_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(treaty_cession_reading_su_t0, observed).
narrative_ontology:measurement(treaty_cession_reading_su_t37, treaty_authority_cession__crown_cession_reading, suppression_requirement, 37, 0.58).
narrative_ontology:measurement_basis(treaty_cession_reading_su_t37, observed).
narrative_ontology:measurement(treaty_cession_reading_su_t65, treaty_authority_cession__crown_cession_reading, suppression_requirement, 65, 0.78).
narrative_ontology:measurement_basis(treaty_cession_reading_su_t65, observed).
narrative_ontology:measurement(treaty_cession_reading_su_t90, treaty_authority_cession__crown_cession_reading, suppression_requirement, 90, 0.74).
narrative_ontology:measurement_basis(treaty_cession_reading_su_t90, observed).
narrative_ontology:measurement(treaty_cession_reading_su_t135, treaty_authority_cession__crown_cession_reading, suppression_requirement, 135, 0.6).
narrative_ontology:measurement_basis(treaty_cession_reading_su_t135, observed).
narrative_ontology:measurement(treaty_cession_reading_su_t145, treaty_authority_cession__crown_cession_reading, suppression_requirement, 145, 0.52).
narrative_ontology:measurement_basis(treaty_cession_reading_su_t145, observed).
narrative_ontology:measurement(treaty_cession_reading_su_t175, treaty_authority_cession__crown_cession_reading, suppression_requirement, 175, 0.47).
narrative_ontology:measurement_basis(treaty_cession_reading_su_t175, observed).
narrative_ontology:measurement(treaty_cession_reading_su_t185, treaty_authority_cession__crown_cession_reading, suppression_requirement, 185, 0.45).
narrative_ontology:measurement_basis(treaty_cession_reading_su_t185, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, retrospective_snare_exposure).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, native_land_court_individualisation).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Treaty of Waitangi' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle. This file carries the English-text cession claim (epsilon 0.68; referent: the Crown-title regime as constituted by the cession doctrine). rangatiratanga_retention_reading carries the contra-proferentem partnership claim (different beneficiary/victim structure, lower epsilon). retrospective_snare_exposure carries the meta-claim that the textual divergence itself is the mechanism (highest epsilon; victim set includes all descendants of the Maori-text signatories). The upstream story is this one: the cession reading is cited as settled authority foreclosing the other two, so influence edges run from this file to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
