% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Rangatiratanga Reading of the Waitangi Sovereignty Allocation
 *   domain: constitutional/indigenous/post-colonial
 *
 * SUMMARY:
 *   The colloquial label 'what the Treaty of Waitangi allocated' covers three
 *   structurally distinct claims, and per the ε-invariance principle this
 *   file instantiates exactly one of them: the rangatiratanga reading, under
 *   which the Māori text's Article II guaranteed tino rangatiratanga (full
 *   authority) over lands, resources, and taonga, and Article I granted the
 *   Crown only kāwanatanga (governorship) over the settler population. The
 *   standing arrangement this reading assesses is the actual New Zealand
 *   constitutional order as operated since 1840 — Crown assertion of plenary
 *   authority, parliamentary supremacy, and the land-transfer machinery built
 *   on it. Measured through this reading's lights, that standing arrangement
 *   rests on authority the reading holds was never transferred: hence the
 *   high ε over the shared referent. The sibling readings author different ε
 *   over the SAME referent — the crown_sovereignty_reading treats the order
 *   as the legitimate cession baseline (low extraction), the
 *   partnership_reading as a correctable fiduciary deficit (moderate) — and
 *   those differences are documented in their own files, linked through
 *   network.affects_constraints. The ε arc across the interval: extraction
 *   accumulated sharply through preemption, the 1860s wars and raupatu, and
 *   the Native Land Court era (peak ~1875), plateaued through the
 *   assimilation decades, and has partially receded since the 1975 Treaty of
 *   Waitangi Act and the settlements era — while the core sovereignty
 *   question remains formally unresolved.
 *
 * KEY AGENTS:
 *   - iwi_and_hapu: Primary target (organized/trapped) — bore the arrangement's costs across the interval: land, resources, and governing authority flowed away from them through Crown purchase, the Land Court, and confiscation; exit from the territorial state is not a live option
 *   - rangatira_leadership_lines: Secondary target (moderate/identity_locked) — hereditary authority displaced by Crown institutions; the office's identity is fused with the authority the arrangement denies
 *   - settler_colonial_government: Agenda setter (institutional/arbitrage) — administers the allocation, writes and amends the governing statutes, collected land revenue and territorial control, now funds negotiated redress
 *   - settler_descendant_landholders: Primary beneficiary (powerful/mobile) — hold the compounded land wealth the transfer produced; electoral majority sustaining the order
 *   - pastoral_farming_interests: Secondary beneficiary (organized/arbitrage) — farm and monetize the alienated estate; shape land-use and water policy through sector lobbies
 *   - waitangi_tribunal: Analytical observer (institutional/analytical) — investigates breaches against the Treaty texts, recommends remedies it cannot compel
 *   - non_signatory_iwi: Excluded voice (moderate/trapped) — iwi outside Te Tiriti's signatures whose objection to any Crown authority claim never received a forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.58).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.4).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Rangatiratanga Reading of the Waitangi Sovereignty Allocation").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional/indigenous/post-colonial").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '93a78b5f-3f8b-43d0-bb58-4ff197aac8cb').
narrative_ontology:cs_kernel_codification('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', fixed_text).
narrative_ontology:cs_authority_grounding('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', lineage).
narrative_ontology:cs_interpretation_layer_present('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb').
narrative_ontology:cs_reading_relation('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', foundational, tino_rangatiratanga_unceeded).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_unceeded, holdable).
narrative_ontology:cs_axiom_grounding('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', tino_rangatiratanga_unceeded, deontological).
narrative_ontology:cs_axiom('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', foundational, kawanatanga_limited_to_settler_governance).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_settler_governance, holdable).
narrative_ontology:cs_axiom_grounding('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', kawanatanga_limited_to_settler_governance, conventional).
narrative_ontology:cs_axiom('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', secondary, cogovernance_or_independent_structures_required).
narrative_ontology:cs_axiom_status(cogovernance_or_independent_structures_required, holdable).
narrative_ontology:cs_axiom_grounding('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', cogovernance_or_independent_structures_required, instrumental).
narrative_ontology:cs_reference_frame('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', retained_rangatiratanga_order).
narrative_ontology:cs_drift_state('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', contemporary_constitutional_practice, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('93a78b5f-3f8b-43d0-bb58-4ff197aac8cb', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_colonial_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_descendant_landholders).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, pastoral_farming_interests).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, iwi_and_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, rangatira_leadership_lines).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold whakapapa and mana whenua ties to specific rohe. Across the interval most of their land passed out of their hands through Crown-monopsony purchasing, the Native Land Court's conversion of communal title into individually alienable shares, and confiscations following the 1860s wars; fisheries, waterways, and language were governed by statutes they did not consent to. Today they receive settlement redress and co-management roles through Crown-designed processes, and their members live as citizens of the state whose authority they dispute. Leaving the country would not carry their whenua with them, so exit is not a live option.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, iwi_and_hapu, payer,
    organized, generational, trapped, national).

% Hereditary leadership lines whose authority over people and territory predates 1840. Crown institutions replaced their decision-making roles with elected boards, the Māori Land Court, and later settlement governance bodies; some lines now lead post-settlement iwi corporations. Their standing is inseparable from the authority they hold themselves to retain, so stepping outside the dispute would dissolve the office itself.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, rangatira_leadership_lines, payer,
    moderate, generational, identity_locked, national).

% The Crown and its parliamentary successors. It wrote and administers the statutes governing land, resources, and Māori affairs, controls the settlement process, appoints the Tribunal, and can amend or override the rules that bind everyone else. It collected land revenue and territorial control across the interval and now pays negotiated redress from general taxation while retaining ultimate lawmaking power.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_colonial_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Descendants of settlers who acquired land through Crown grants, purchases, and the market the Land Court created. They hold the compounded wealth of that inheritance — freehold estates, urban land, capital — and vote as the electoral majority that sustains the parliamentary order. Selling and relocating domestically or abroad is realistic for many.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_descendant_landholders, beneficiary,
    powerful, biographical, mobile, national).

% Farming and agribusiness operators working much of the former Māori estate. Organized through sector lobbies, they shape land-use and water policy and monetize the productive capacity of the alienated land; converting, subdividing, or trading assets gives them flexible adjustment paths.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, pastoral_farming_interests, beneficiary,
    organized, biographical, arbitrage, national).

% A standing commission of inquiry established in 1975. It hears claims, finds facts about Crown conduct against the Treaty texts, and recommends remedies; successive governments have treated its recommendations as advisory. It investigates but does not legislate, and its mandate depends on the political branches it examines.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Iwi and hapū whose ancestors did not sign Te Tiriti — some because they had affirmed independence in 1835, some because no Crown agent reached them. Constitutional arrangements bind them regardless; their position that no agreement governs them has never been given a forum inside the settlement process, which presumes Treaty coverage.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, non_signatory_iwi, excluded,
    moderate, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_descendant_landholders).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single governing frame for two populations sharing one territory: courts, property registration, infrastructure, defense, and external relations are supplied once, centrally, rather than separately by each iwi and each settler municipality; the Māori text confines the Crown's granted role to governance of the settler population.
% TRANSFER_FUNCTION: Moves land, forests, fisheries, waterways, and day-to-day governing authority from iwi and hapū to the Crown and, through it, to the settler population and their descendants — via Crown preemption of purchases, the Native Land Court's individualization of communal title, confiscation after the 1860s wars, and subsequent legislative overrides — while returning citizenship, services, and, since 1975, negotiated asset redress.
% ABSENT_VOICES: Non-signatory iwi, whose objection to any Crown authority claim predates the arrangement and never entered it; the signing-generation rangatira whose recorded statements exclude cession of internal authority; and future whakapapa generations whose taonga interests were priced into negotiated settlements they could not attend. They sit outside Parliament's structures, entering only through recommendatory channels, litigation, and protest.
% DISAPPEARANCE_RATIONALE: Every freehold title traced through Crown grants, the parliamentary order itself, and the public estate rest on the allocation; overnight removal would force simultaneous renegotiation of land title, governing institutions, and the settlements architecture — the property economy and the constitution would rearrange around whatever allocation the parties next agreed.
% FOUNDING_PROBLEM: 1840: two polities occupy one archipelago — Māori, the demographic majority holding virtually all land under rangatiratanga, and an incoming settler population requiring governance, lawful land transaction, and protection. The instruments sought orderly settlement and Crown oversight of the settlers while the Crown's agents assured rangatira their possessions and authority would remain intact.
% FOUNDING_PROBLEM_CORROBORATION: Attested outside the benefiting parties by: Waitangi Tribunal report findings (state-commissioned but evidentiary, built on iwi testimony); Crown apologies recited in settlement legislation (Waikato-Tainui 1995, Ngāi Tahu 1998) acknowledging conduct contrary to the guarantees; academic historiography (Orange, Walker, Binney) reconstructing the signing-generation understanding; and United Nations treaty-body reviews noting unresolved self-determination questions. No wholly disinterested arbiter exists — the Tribunal operates inside the state it audits — but multiple non-beneficiary sources corroborate both the founding problem and its unresolved status.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time grid: t = years since 1840; t185 = 2025. Extractiveness ends at 0.58 after peaking at 0.88 during the Native Land Court era (t35 ≈ 1875), when communal title conversion drove the bulk of alienation; the post-1975 decline reflects settlements, asset returns, and co-governance instruments, not resolution of the underlying authority question. Suppression_requirement is tracked because enforcement capacity changed qualitatively: military enforcement peaked at the 1860s invasions (0.80 at t23), shifted to administrative and cultural suppression (Native Schools language policy, Tohunga Suppression Act 1907), and settled into a lower juridical register (0.40 today) — protest policing and parliamentary override rather than open force. Theater_ratio rises monotonically from 0.20 to 0.55: the raw-power phase was least theatrical; each subsequent era wrapped the same structure in progressively thicker legitimating rhetoric ('amalgamation', 'the best race relations in the Empire', 'Treaty principles', 'partnership'), and the settlements era pairs real asset transfers with heavy ceremonial and discursive performance that sometimes substitutes for authority transfer. Accessibility_collapse 0.55: secession and parallel-sovereignty alternatives were foreclosed by force, but cultural-political alternatives persisted and revived — Kīngitanga survived Waikato, the Rātana movement, the Māori renaissance, iwi corporations, and international advocacy (UNDRIP) kept alternative frames live. Resistance 0.75: near-continuous armed resistance (1840s-1870s), passive resistance (Parihaka), and political resistance (1975 land march, Bastion Point, foreshore and seabed hīkoi, Ihumātao) across the whole interval. Claim/metric independence: claimed_type tangled_rope is authored from the reading's own structural concession — kāwanatanga over settlers is genuine coordination the reading affirms — while the metrics describe the arrangement's actual operation independently; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience the same statutes differently. From the Crown seat the arrangement is the lawful constitutional order it administers, amends, and funds redress within; from the settler-beneficiary seats it is the ordinary property regime they inherited and improved; from the iwi seats the same Land Court, preemption, and override machinery operated as the instrument of dispossession of authority never ceded; from the Tribunal seat it resolves into a sequence of auditable breaches and recommendable remedies. The engine computes these divergent per-seat classifications from the structural data — trapped victims with generational horizons sit near the full-target end; mobile and arbitrage beneficiaries sit near the subsidized end; the agenda-setting Crown sits near the beneficiary end with slight offset from its settlement-payment obligations.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for settler_descendant_landholders and pastoral_farming_interests (structural subsidy: the land and its productivity flow to them); the Crown derives low d as agenda-setting beneficiary but is nudged off the floor by its redress payments and constitutional exposure — left to the structural derivation, no override needed. Victim declarations drive high d for iwi_and_hapu, amplified by trapped exit: they cannot leave the jurisdiction without abandoning the whenua that constitutes them, and identity_lock pushes rangatira_leadership_lines to the full-target end. Non-signatory iwi, excluded rather than coordinated, register as targets of an arrangement they never joined. National spatial scope modestly amplifies effective extraction: verifying consent across 185 years of statute-making is structurally hard. The Tribunal's analytical seat computes neutrally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating authority between two polities sharing one territory — is live, so this is not a mandatrophy case; the R5 mismatch consumer should find status=live x verdict=world_rearranges consistent, no zombie flag. The danger this classification guards against runs both directions. Labeling the standing order a pure rope (as the crown reading effectively does) erases the asymmetric taking the victim seats document; labeling it a pure snare erases the kāwanatanga function the reading itself concedes and forecloses the negotiated-rebalance pathway the settlements era actually uses. The theater_ratio crossing 0.5 in the settlements era flags the live Goodhart risk: financial and symbolic redress substituting for the authority return this reading requires. If co-governance instruments remain advisory while the rhetoric of partnership thickens, the structure drifts toward performative maintenance; if instruments like Te Awa Tupua's legal personality propagate binding authority, the structure migrates toward genuine rebalanced coordination. The tangled_rope claim keeps both trajectories visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of kernel waitangi_sovereignty_allocation (reading: rangatiratanga_reading); what structurally changes under the sibling readings?',
    'Compare the three reading files'' beneficiary/victim sets and ε over the identical referent; the divergence locates the contest and prevents cross-reading ε averaging.',
    'Under crown_sovereignty_reading the Māori-specific victim set dissolves into ordinary parliamentary politics and ε drops toward the coordination floor; under partnership_reading the victims remain but remedies shift to fiduciary duties, lowering measured extraction without resolving the authority question. This file''s high ε is reading-indexed, not topic-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one-of-three readings; siblings alter the victim set and ε over the same referent.').

omega_variable(
    article_one_translation_dispute,
    'Does kāwanatanga in the Māori Article I denote complete ceded sovereignty or limited governorship over the settler population — the hinge on which this reading stands or collapses?',
    'Philological and historical reconstruction of 1840 understandings: contemporary rangatira speeches, Hobson''s ''he iwi tahi tatou'' framing, missionary translation records, and the absence of any Māori-text term corresponding to cession of internal authority.',
    'If kāwanatanga denotes complete sovereignty, this reading loses its textual anchor and reduces to a moral claim overlapping the partnership reading; if it denotes limited governorship, the standing arrangement rests on authority never transferred and this reading''s high ε over the referent is textually compelled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_one_translation_dispute, empirical, 'Where the kernel disagreement is located: the meaning of the Article I term.').

omega_variable(
    settlement_theater_vs_repair,
    'Do Treaty settlements and co-governance instruments genuinely reduce the arrangement''s extraction, or substitute financial and symbolic redress for the authority return this reading requires?',
    'Track whether co-governance instruments acquire binding legal authority (propagation of the Te Awa Tupua legal-personality precedent) versus remaining advisory; compare cumulative settlement asset values against unextinguished claims (WAI 262 flora and fauna, confiscated-lands returns, foreshore and seabed).',
    'Genuine repair drives ε and theater_ratio down together and dates a migration toward rebalanced coordination; theatrical substitution drives theater_ratio up while ε plateaus, dating drift toward performative maintenance of an unrepaired structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_theater_vs_repair, empirical, 'Whether the settlements era is functional transition or proxy substitution.').

omega_variable(
    iwi_coalition_capacity,
    'Does pan-iwi coordination (for example the National Iwi Chairs Forum) shift the iwi power atom from organized toward institutional, changing the computed effective extraction borne by the trapped seat?',
    'Observe whether joint iwi positions obtain statutory outcomes no single iwi achieved — policy vetoes, co-governance statutes, entrenchment of Māori seats — versus repeated individual-iwi losses.',
    'Effective coalition formation dampens the target-side amplification and moves the computed classification toward a negotiable tangled_rope; persistent coalition failure leaves the trapped-exit profile dominant and the extraction computation unchanged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iwi_coalition_capacity, empirical, 'Whether coalition formation modulates the trapped-seat extraction profile.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsa_rr_tr_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(wsa_rr_tr_t15, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(wsa_rr_tr_t23, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 23, 0.18).
narrative_ontology:measurement(wsa_rr_tr_t35, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 35, 0.3).
narrative_ontology:measurement(wsa_rr_tr_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(wsa_rr_tr_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 90, 0.42).
narrative_ontology:measurement(wsa_rr_tr_t120, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 120, 0.48).
narrative_ontology:measurement(wsa_rr_tr_t135, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 135, 0.52).
narrative_ontology:measurement(wsa_rr_tr_t155, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 155, 0.5).
narrative_ontology:measurement(wsa_rr_tr_t185, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 185, 0.55).

% Extraction over time
narrative_ontology:measurement(wsa_rr_be_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(wsa_rr_be_t15, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(wsa_rr_be_t23, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 23, 0.76).
narrative_ontology:measurement(wsa_rr_be_t35, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 35, 0.88).
narrative_ontology:measurement(wsa_rr_be_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 60, 0.87).
narrative_ontology:measurement(wsa_rr_be_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 90, 0.85).
narrative_ontology:measurement(wsa_rr_be_t120, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 120, 0.82).
narrative_ontology:measurement(wsa_rr_be_t135, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 135, 0.74).
narrative_ontology:measurement(wsa_rr_be_t155, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 155, 0.65).
narrative_ontology:measurement(wsa_rr_be_t185, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 185, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wsa_rr_su_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(wsa_rr_su_t15, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(wsa_rr_su_t23, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 23, 0.8).
narrative_ontology:measurement(wsa_rr_su_t35, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 35, 0.75).
narrative_ontology:measurement(wsa_rr_su_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(wsa_rr_su_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 90, 0.6).
narrative_ontology:measurement(wsa_rr_su_t120, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement(wsa_rr_su_t135, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 135, 0.5).
narrative_ontology:measurement(wsa_rr_su_t155, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 155, 0.42).
narrative_ontology:measurement(wsa_rr_su_t185, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 185, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).

% DUAL FORMULATION NOTE:
% The natural-language concept 'what the Treaty of Waitangi allocated' decomposes per the ε-invariance principle into three structurally distinct constraints sharing one kernel: crown_sovereignty_reading (complete cession; low ε over the standing order), partnership_reading (fiduciary partnership; moderate ε), and this file, rangatiratanga_reading (no cession of internal authority; highest ε over the same referent). Each carries its own beneficiaries, victims, metrics, and classification; the family is linked through affects_constraints. The upstream/downstream gradient runs from the crown reading (state-endorsed, institutionally enforced, cited as settling the question) through the partnership reading (judicially constructed via the principles doctrine) to this reading (iwi-held, textually anchored in the Māori text and He Whakaputanga 1835). The crown reading's institutional dominance is precisely what this reading identifies as the extraction mechanism — the upstream claim is cited as evidence that no further allocation question exists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
