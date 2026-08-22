% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of the Waitangi Sovereignty Allocation (Article I Complete Cession)
 *   domain: constitutional/post-colonial governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Waitangi
 *   sovereignty-allocation kernel: the crown sovereignty reading, under which
 *   the English text's Article I effected a complete cession of sovereignty
 *   to the Crown, grounding Westminster parliamentary supremacy. On this
 *   reading the Crown legislates for all persons and resources without a
 *   Māori consent requirement, allocates land, water, fisheries, and minerals
 *   unilaterally, and Māori interests hold whatever weight parliamentary
 *   majorities concede. The sibling readings (partnership, rangatiratanga)
 *   are separate constraint files, not folded into this one; the contest
 *   between readings is routed to omega variables and kernel_context per the
 *   committer-frame rules. The claim/metric gap is deliberate: the
 *   arrangement is CLAIMED as tangled_rope (a real coordination core wrapped
 *   around asymmetric extraction) while the authored metrics describe a
 *   heavily extractive, actively enforced history — the engine measures the
 *   divergence from the structural data; nothing here reconciles claim to
 *   metrics.
 *
 * KEY AGENTS:
 *   - - crown_in_parliament: agenda-setter and principal beneficiary (institutional/arbitrage) — holds plenary power, collects governing authority, restructures at will
 *   - - settler_landowners: primary material beneficiary (powerful/constrained) — wealth fixed in Crown-derived title chains
 *   - - crown_resource_enterprises: secondary beneficiary (institutional/arbitrage) — exploits statutorily vested assets
 *   - - maori_iwi_and_hapu: primary target (moderate/trapped) — bore land loss, cultural suppression, and subordinated political standing
 *   - - kingitanga_and_kotahitanga_movements: excluded voice (organized/trapped) — asserted retained authority, refused constitutional recognition
 *   - - senior_courts: enforcement seat with analytical view (institutional/analytical) — maintain the justiciability line that keeps the reading operative
 *   - - waitangi_tribunal: analytical observer (institutional/analytical) — documents the record, cannot compel remedy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.57).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.44).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.47).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.47).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of the Waitangi Sovereignty Allocation (Article I Complete Cession)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/post-colonial governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, '84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a').
narrative_ontology:cs_kernel_codification('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', fixed_text).
narrative_ontology:cs_authority_grounding('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', lineage).
narrative_ontology:cs_interpretation_layer_present('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a').
narrative_ontology:cs_reading_relation('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', foundational, complete_sovereignty_ceded_by_article_one).
narrative_ontology:cs_axiom_status(complete_sovereignty_ceded_by_article_one, holdable).
narrative_ontology:cs_axiom_grounding('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', complete_sovereignty_ceded_by_article_one, conventional).
narrative_ontology:cs_axiom('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', secondary, legislation_requires_no_maori_consent).
narrative_ontology:cs_axiom_status(legislation_requires_no_maori_consent, holdable).
narrative_ontology:cs_axiom_grounding('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', legislation_requires_no_maori_consent, conventional).
narrative_ontology:cs_reference_frame('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', valid_cession_westminster_supremacy).
narrative_ontology:cs_drift_state('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', contemporary_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('84a2b3ad-4b97-41d7-89b0-d56fe9a1aa2a', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_in_parliament).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_landowners).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_resource_enterprises).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, treaty_industry_professionals).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_and_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, kingitanga_and_kotahitanga_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds plenary legislative power over all persons, lands, waters, and resources in New Zealand under this reading: it legislates without any Māori consent requirement, vests and revests assets by statute, and can amend or repeal Treaty-reference legislation at will. It collects governing authority itself as the primary gain of the arrangement, while bearing the costs of enforcing compliance (policing, litigation defence) and of international reputational exposure.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_in_parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Acquired the bulk of alienated Māori land through Crown purchases, Native Land Court-derived titles, and post-confiscation grants between the 1840s and the twentieth century. Their asset base, credit, and political weight rest on the indefeasibility of Crown-derived title; selling out does not exit the arrangement because the market value of every successor parcel depends on the same title chain.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_landowners, beneficiary,
    powerful, biographical, constrained, national).

% State-owned enterprises, Crown forestry entities, and licensed extractors hold forests, energy assets, minerals, and seabed rights vested under statutes enacted without Māori consent. Settlements have returned portions with leases and rents continuing to flow to the Crown balance sheet; they can restructure around any single concession because the vesting statutes sit above the concessions.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_resource_enterprises, beneficiary,
    institutional, generational, arbitrage, national).

% Negotiators, counsel, historians, and consultants who earn fees from claims, settlements, and consultation processes. They collect income from the arrangement's ongoing contestation without bearing its underlying costs, and can move between Crown-side and iwi-side engagements freely.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, treaty_industry_professionals, beneficiary,
    moderate, biographical, mobile, national).

% Collectives holding taonga tuku iho (intergenerational treasures) in land, water, fisheries, and language. By 1939 iwi had lost roughly 95% of their land base through pre-emption waivers, Native Land Court individualisation, and raupatu (confiscation). They now exercise only such authority as the Crown concedes through settlements and co-governance arrangements, fund litigation and negotiation from a diminished base, and cannot exit: identity, burial grounds, and livelihood are fixed in the whenua, and leaving the jurisdiction abandons rather than escapes the claim.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_and_hapu, payer,
    moderate, generational, trapped, national).

% Built parallel institutions — the Kīngitanga from 1858, Kotahitanga parliaments in the 1890s, rūnanga and Māori councils — asserting retained authority and petitioning the Crown for recognised constitutional place. Their petitions went substantially unanswered, their institutions were refused legal recognition, and they remain outside formal lawmaking except as consultees. Some iwi, including Tūhoe, never signed the instrument at all and were nonetheless brought inside the asserted allocation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, kingitanga_and_kotahitanga_movements, excluded,
    organized, generational, trapped, national).

% Apply parliamentary supremacy and control the justiciability line that keeps the reading operative: Wi Parata v Bishop of Wellington (1877) treated the instrument as a nullity for courts, Hoani Te Heuheu (1941) held it unenforceable against statutes, and the Lands Case (1987) articulated Treaty principles that guide but cannot veto legislation. They analyse the whole structure from the bench while enforcing the supremacy premise that makes the analysis non-binding.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, senior_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, senior_courts, observer).

% Since 1975 (historical jurisdiction from 1985) it inquires into Crown conduct, compiles the evidentiary record both sides litigate over, and reports findings and recommendations that are advisory only. It can document that the allocation operated without consent but cannot compel remedy, restructure authority, or stay legislation pending report.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_landowners).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single supreme legislative authority for the territory, resolving the jurisdictional conflict between settler self-government and imperial control, and supplying uniform law, courts, land registration, and contract enforcement across both populations.
% TRANSFER_FUNCTION: Moves land, water, fisheries, minerals, and decision-making authority from Māori iwi and hapū to the Crown and onward to settler purchasers and Crown enterprises; moves political authority from rangatira to Parliament, with compensation paid only where the Crown chooses to pay it.
% ABSENT_VOICES: The rangatira who signed te Tiriti Māori text understood kāwanatanga as a limited governorship over settlers, not a cession of their own authority; iwi that never signed were included anyway; the Kīngitanga and Kotahitanga sought constitutional seats and were refused. Their objection — that no full cession occurred, so no consent basis exists for plenary power — is voiced today only through advisory tribunals, protest, and litigation, not inside the lawmaking room.
% DISAPPEARANCE_RATIONALE: If the supremacy allocation vanished overnight, every statutory vesting, Crown-derived title, settlement deed, and co-governance arrangement would lose its legal foundation simultaneously; the entire property system, public service, and courts would require refoundation, and authority over lands and resources would revert to contest among the holders of the original claims.
% FOUNDING_PROBLEM: The 1830s problem: ungoverned settler influx, land-sharking by speculators, and episodic violence (the Elizabeth affair, the Wairau affray), with no single authority able to control British subjects or regularise land transactions. Imperial policy, per Lord Normanby's 1839 instructions, was to establish government over settlers and protect Māori from dispossession.
% FOUNDING_PROBLEM_CORROBORATION: Imperial Colonial Office records — Normanby's instructions and James Stephen's minutes — attest the founding problem from outside the settler-benefit set, and they attest its protective half explicitly. Historians (Claudia Orange, Alan Ward) corroborate both halves. Māori oral accounts and Tribunal findings attest that the protective half was abandoned almost immediately, which is why the status is contested rather than dead: the control-the-settlers half was absorbed into supremacy, the protect-Māori half was not delivered.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.57 at interval end, peaking 0.82 circa 1890) because the transfer of land and authority ran overwhelmingly one way and compensation was discretionary; even assessed by this reading's own lights — which frames much of the transfer as lawful purchase — the net flow and the absence of any consent requirement leave substantial extraction. Suppression (0.44 current) tracks the enforcement machinery: the New Zealand Wars and raupatu, Native Land Court coercion, language and tohunga suppression, policing of occupations (Bastion Point, Raglan), and the Foreshore and Seabed Act 2004 overreach. Theater ratio (0.47) is the fastest-moving series: consultation rituals, settlement ceremonies, and co-governance boards increasingly perform partnership while the supremacy kernel sits untouched beneath them — classic Goodhart drift of proxy ritual over transferred substance. Accessibility collapse is 0.7: once the reading is understood, alternatives (parallel authority, refusal of jurisdiction) have been legally and militarily foreclosed, though Kīngitanga survival and international fora kept partial exits alive. Resistance is 0.6 currently — sustained litigation, hīkoi (including the 2024 hīkoi mō te Tiriti), and occupations — against a historical peak near open war. All three series share one nine-point grid (years since 1840) so temporal analysis samples aligned rows; the extraction curve is a wave, not monotonic drift: mobilisation (1860s–90s) drove extraction up, Māori demographic and political recovery plus the post-1975 claims regime pulled it partway down, and the residual plateau reflects settlements that return assets without returning the underlying allocation. The late-cycle oscillation between concession and retrenchment (settlements, then Foreshore and Seabed, then co-governance, then the 2024 principles codification attempt) functions partly as intermittent reinforcement — each concession resets pressure without altering the kernel.
 *
 * PERSPECTIVAL GAP:
 *   From the crown_in_parliament seat the arrangement computes as functioning governance: a single authority solving jurisdictional conflict, with concessions flowing downward as a matter of grace. From the maori_iwi_and_hapu seat the identical structure computes as enforced dispossession: the coordination shell is real but the load it carries is one-directional. The senior_courts seat straddles the gap — it administers the supremacy premise while observing, from the same bench, that the premise forecloses the relief claimants seek. The engine derives these divergent per-seat classifications from the structural data (roles, power, exit); this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (crown_in_parliament, settler_landowners, crown_resource_enterprises, treaty_industry_professionals) drive those seats toward the beneficiary end of directionality — the constraint subsidises them with authority, title, assets, and fees. Victim declarations (maori_iwi_and_hapu, kingitanga_and_kotahitanga_movements) drive those seats toward the full-target end, amplified by trapped exit: whensua-fixed identity and unavoidable jurisdiction mean no arbitrage-grade exit damps their effective extraction. The asymmetry is structural, not attitudinal: the same statute that vests a forest in a Crown enterprise strips the hapū claim beneath it. Scope is national throughout, so verification difficulty amplifies extraction modestly for every seat; suppression is authored raw and unscaled, per the structural-property rule.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem had two halves: control the settlers, protect the Māori. The first half was genuinely solved and remains solved — that is the live coordination core that blocks a pure-snare reading. The second half was abandoned within a decade of signing; the protective function the imperial government promised is dead, while the arrangement built partly to deliver it persists at full strength. Authoring founding_problem_status as contested (rather than dead) reflects the genuine dispute over whether modern settlements revive the protective half or merely price its absence. The mandatrophy risk runs both ways: reading the arrangement as pure coordination erases the victims; reading it as pure extraction erases the real jurisdictional problem any government would have had to solve in 1840. The tangled_rope claim holds both facts in one structure, and the theater-ratio series marks where the coordination function is decaying into performance while the extraction function persists unchanged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection,
    'Which reading of the waitangi_sovereignty_allocation kernel governs the standing arrangement — complete cession (this file), ongoing partnership, or retained rangatiratanga?',
    'Constitutional settlement: entrenched Treaty provision, codified principles with cross-party agreement, or a sustained judicial shift on justiciability. Sibling-file classifications update when the selection resolves.',
    'If the partnership reading is selected, this constraint''s extraction collapses toward consultation-cost levels and its type migrates toward rope. If the rangatiratanga reading is selected, the beneficiary/victim structure inverts and the standing arrangement recomputes as uncompensated continuation — snare-flavored. If this reading is reaffirmed, the current classification stands.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection, conceptual, 'Committer-frame omega: this story is one reading of a contested kernel; sibling readings are separate constraints with different epsilon and inverted structures.').

omega_variable(
    cession_completeness_and_understanding,
    'Did the rangatira who signed in 1840 understand and effect the complete cession the English text asserts — and what is the allocation''s footing for the iwi that never signed?',
    'Historical-linguistic analysis of the two texts against 1840 Māori political concepts (kāwanatanga, tino rangatiratanga, mana), signature-coverage mapping, and Tribunal-commissioned oral testimony.',
    'If cession was incomplete or misunderstood, this reading loses its conventional-law foundation and the arrangement persists by imposition alone — pushing the computed type toward snare and raising effective extraction for every trapped seat. If cession stands, the reading''s foundation holds and current metrics stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cession_completeness_and_understanding, empirical, 'Factual predicate of the reading: completeness and comprehension of the 1840 cession, including non-signatory iwi.').

omega_variable(
    persistence_driver_ambiguity,
    'Does the arrangement persist because single supreme authority remains necessary for the territory''s legal order, or because reversal costs (refounding every title, statute, and settlement deed) are prohibitive regardless of necessity?',
    'Counterfactual constitutional design analysis: whether a refounded settlement could preserve title security and legal order while reallocating authority; comparative evidence from jurisdictions that reconstituted sovereignty (settled via treaty federalism) without title collapse.',
    'If necessity drives persistence, the coordination core is robust and the tangled_rope reading is stable. If reversal-cost inertia drives it, the arrangement is drifting toward piton — maintained because fixing is prohibitive, with the theater ratio marking the decay of the coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_driver_ambiguity, conceptual, 'Whether persistence reflects live coordination value or sunk-cost lock-in of the supremacy allocation.').

omega_variable(
    cogovernance_substance,
    'Do settlements, co-governance boards, and Treaty-principle references transfer real decision authority, or do they perform transfer while the vesting statutes and supremacy kernel remain untouched?',
    'Track veto incidence and reversibility: whether co-governance bodies can block Crown action or are overridable by ordinary legislation; compare pre- and post-settlement Crown discretion over the same assets.',
    'If substance, the theater-ratio series is overstated and extraction is genuinely declining toward a stable hybrid. If performance, theater continues climbing past 0.5, the mandate-outlived-function flag fires, and the arrangement trends toward piton with extractive residue.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cogovernance_substance, empirical, 'Substance versus performance of the post-1975 concession layer sitting atop the unchanged supremacy kernel.').

omega_variable(
    revival_pressure_trajectory,
    'Will codification attempts to reassert this reading (the 2024–25 Treaty Principles Bill episode) reconsolidate the complete-cession frame, or accelerate its displacement by hardening opposition and clarifying the alternative?',
    'Observe the bill''s fate, subsequent electoral cycles, and whether any codified principles survive with cross-party support; measure resistance-series response.',
    'Successful revival would push drift_state from practice_drift toward revival_pressure consolidating this reading and raising suppression. Failed revival followed by entrenchment of the rival frame would convert this file''s status toward RESOLVED CONTESTED and shift classification weight to the sibling files.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revival_pressure_trajectory, empirical, 'Direction of active reconstruction pressure on the crown-sovereignty reference frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(wait_tr_t25, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 25, 0.12).
narrative_ontology:measurement(wait_tr_t50, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(wait_tr_t75, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement(wait_tr_t100, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 100, 0.22).
narrative_ontology:measurement(wait_tr_t125, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 125, 0.28).
narrative_ontology:measurement(wait_tr_t150, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 150, 0.38).
narrative_ontology:measurement(wait_tr_t175, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 175, 0.44).
narrative_ontology:measurement(wait_tr_t185, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 185, 0.47).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(wait_be_t25, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement(wait_be_t50, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement(wait_be_t75, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 75, 0.8).
narrative_ontology:measurement(wait_be_t100, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 100, 0.76).
narrative_ontology:measurement(wait_be_t125, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 125, 0.74).
narrative_ontology:measurement(wait_be_t150, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 150, 0.62).
narrative_ontology:measurement(wait_be_t175, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 175, 0.58).
narrative_ontology:measurement(wait_be_t185, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 185, 0.57).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(wait_su_t25, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(wait_su_t50, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(wait_su_t75, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement(wait_su_t100, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(wait_su_t125, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 125, 0.58).
narrative_ontology:measurement(wait_su_t150, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 150, 0.45).
narrative_ontology:measurement(wait_su_t175, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 175, 0.42).
narrative_ontology:measurement(wait_su_t185, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 185, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% Constraint family: waitangi_sovereignty_allocation decomposes per the epsilon-invariance principle into three readings — crown_sovereignty_reading (this file), partnership_reading, and rangatiratanga_reading. The colloquial label 'the Treaty' covers three structurally distinct allocations with different epsilon values, different beneficiary/victim structures, and different failure modes; forcing them into one story would make epsilon observable-dependent. This reading is upstream of the siblings in legitimacy terms: the complete-cession premise is cited as the reason consultation duties and retained-authority claims lack legal force, so this file's operation suppresses the siblings' operating environment without resolving the textual dispute. Edges here declare influence on both siblings; the sibling files carry reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
