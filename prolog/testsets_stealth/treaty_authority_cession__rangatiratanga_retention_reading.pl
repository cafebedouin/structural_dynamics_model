% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Treaty Partnership Consent Requirement (Rangatiratanga Retention Reading)
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Treaty of Waitangi's authority
 *   clause: the rangatiratanga_retention_reading. On this reading the Māori
 *   text controls (the Crown drafted both versions, so discrepancies resolve
 *   against the drafter via contra proferentem); kāwanatanga granted only
 *   delegated governance, not sovereignty; tino rangatiratanga was retained;
 *   and the Treaty therefore establishes a partnership in which Crown acts
 *   are legitimate only within ongoing hapū consent. The constraint authored
 *   here is that partnership-consent arrangement itself, with a single stable
 *   ε. The claim and the metrics are independent facts: the type is claimed
 *   from this reading's seat, while the metric series describe the
 *   arrangement's actual operation across 185 years — including the long era
 *   in which the text was invoked ceremonially and its requirements denied.
 *   The analysis of the translation asymmetry as an extraction mechanism is a
 *   DIFFERENT constraint, authored in the sibling story
 *   retrospective_snare_exposure; this file links to it rather than absorbing
 *   it, preserving ε-invariance.
 *
 * KEY AGENTS:
 *   - iwi_and_hapu: Primary protected party (organized/identity_locked) — retain rangatiratanga, grant the consent on which Crown legitimacy rides
 *   - crown_executive: Mandate-holder and administrator (institutional/constrained) — exercises delegated governance under the consent condition; dual-positioned beneficiary and bearer of compliance costs
 *   - paakeha_settler_and_commercial_interests: Cost-bearing class (powerful/mobile) — projects require agreement; historically resisted by lobbying, litigation, and war
 *   - judiciary: Interpretive authority (institutional/analytical) — moved from non-enforcement to policing Crown conduct against the bargain's terms
 *   - waitangi_tribunal: Analytical observer (institutional/analytical) — inquiries and findings that shape settlements without binding
 *   - urban_non_affiliated_maori: Excluded constituency (moderate/identity_locked) — bound by the arrangement's identity ties but outside the consent machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.3).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.38).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Treaty Partnership Consent Requirement (Rangatiratanga Retention Reading)").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '83e613f4-9f63-4bbd-949f-dc299711a029').
narrative_ontology:cs_kernel_codification('83e613f4-9f63-4bbd-949f-dc299711a029', fixed_text).
narrative_ontology:cs_authority_grounding('83e613f4-9f63-4bbd-949f-dc299711a029', lineage).
narrative_ontology:cs_interpretation_layer_present('83e613f4-9f63-4bbd-949f-dc299711a029').
narrative_ontology:cs_reading_relation('83e613f4-9f63-4bbd-949f-dc299711a029', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('83e613f4-9f63-4bbd-949f-dc299711a029', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_reading_relation('83e613f4-9f63-4bbd-949f-dc299711a029', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('83e613f4-9f63-4bbd-949f-dc299711a029', foundational, maori_text_controls_interpretation).
narrative_ontology:cs_axiom_status(maori_text_controls_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('83e613f4-9f63-4bbd-949f-dc299711a029', maori_text_controls_interpretation, conventional).
narrative_ontology:cs_axiom('83e613f4-9f63-4bbd-949f-dc299711a029', foundational, tino_rangatiratanga_unextinguished).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_unextinguished, holdable).
narrative_ontology:cs_axiom_grounding('83e613f4-9f63-4bbd-949f-dc299711a029', tino_rangatiratanga_unextinguished, deontological).
narrative_ontology:cs_axiom('83e613f4-9f63-4bbd-949f-dc299711a029', secondary, crown_legitimacy_requires_hapu_consent).
narrative_ontology:cs_axiom_status(crown_legitimacy_requires_hapu_consent, holdable).
narrative_ontology:cs_axiom_grounding('83e613f4-9f63-4bbd-949f-dc299711a029', crown_legitimacy_requires_hapu_consent, instrumental).
narrative_ontology:cs_reference_frame('83e613f4-9f63-4bbd-949f-dc299711a029', maori_text_partnership_order).
narrative_ontology:cs_drift_state('83e613f4-9f63-4bbd-949f-dc299711a029', contemporary_post_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('83e613f4-9f63-4bbd-949f-dc299711a029', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, iwi_and_hapu).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_executive).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, paakeha_settler_and_commercial_interests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold continuing authority over their lands, villages, waters, and taonga under the arrangement the Māori text records. They granted a governor limited powers of administration while keeping their own chiefly authority intact, and their agreement is the standing condition on which Crown actions affecting their interests are supposed to proceed. Since the 1980s many have rebuilt collective capacity through settlement negotiations and post-settlement entities that exercise and defend that authority. Leaving the relationship is not a meaningful option: descent, land, and communal obligation tie them to it permanently — exit would mean ceasing to be who they are.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, iwi_and_hapu, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, iwi_and_hapu, agenda_setter).

% Exercises the governance powers the Māori text delegates: legislation, administration, courts, defence. Under this reading its mandate exists only inside the bargain — actions touching Māori interests are supposed to proceed by agreement, and unilateral steps lack legitimacy even where parliament could enact them. It collects the mandate itself and the stability of ordered coexistence, and it pays in constrained action: negotiation duties, redress liabilities running to billions, and co-governance obligations embedded in statute. Dissolving the bargain would undercut the legitimacy story the state tells about itself, so walking away is not realistically available.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_executive, agenda_setter,
    institutional, generational, constrained, national).

% Farming, forestry, fishing, infrastructure, and development projects that touch land, water, or coastline must clear the agreement hurdle before proceeding. They bear the delays, co-design costs, and settlement-funded transfers, and their predecessors responded historically by lobbying for legislative override and, before that, by war. They also collect the dividends of a lawful, non-insurrectionary environment and market access under a stable flag. Individuals can emigrate and capital can relocate; the class as such stays and adapts.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, paakeha_settler_and_commercial_interests, payer,
    powerful, biographical, mobile, national).

% Interprets what the bargain requires. After a century of treating the Treaty as unenforceable (Wi Parata 1877), the courts reversed course: the 1987 Lands Case articulated partnership and active protection as enforceable principles, and later rulings police Crown conduct against them. Judges neither fund nor receive the arrangement's flows; their stake is doctrinal custody — deciding which text, which canon, and which obligations bind.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% A standing commission of inquiry established in 1975, empowered to hear claims that Crown actions breached the bargain and to recommend remedies, with retroactive jurisdiction extended in 1985. Its reports — including the 2014 finding that sovereignty was not ceded in the North — shape settlement negotiations and public understanding, though most recommendations do not bind. It investigates and reports; it administers nothing and collects nothing.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% A majority of Māori now live in cities, many disconnected from the iwi structures through which settlements and co-governance speak. The arrangement channels recognition, redress, and authority through mandated collective bodies they did not join and cannot easily influence; their interests surface only when those bodies choose to carry them. They carry the identity ties that make exit meaningless while lacking a seat in the consent machinery conducted in their name.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, urban_non_affiliated_maori, excluded,
    moderate, biographical, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__rangatiratanga_retention_reading, crown_executive).
narrative_ontology:fixing_cost_class(treaty_authority_cession__rangatiratanga_retention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines how two polities share one territory without subjugating either: day-to-day governance (kāwanatanga) may be exercised by the Crown while authority over taonga — lands, villages, treasures — remains with iwi and hapū, and exercises of governance touching Māori interests proceed by agreement rather than unilateral decree.
% TRANSFER_FUNCTION: Moves decision authority, not title: rangatira delegate a defined sphere of governance to the Crown; the Crown returns recognition of continued rangatiratanga and submits its acts to a consent condition. Material flows — land, resources, revenue — move only through transactions separately agreed; the arrangement itself conveys no property.
% ABSENT_VOICES: Urban Māori outside iwi settlement structures sit outside the consent machinery that speaks in hapū names; the rangatira who assented in 1840 are gone and cannot re-consent, so their successors' authorization is inferred; and the descendants of rangatira in regions where the Treaty was never signed hold a position that was never recorded. They stand outside the negotiating room, represented by structures they did not choose.
% DISAPPEARANCE_RATIONALE: Settlement redress contracts, co-governance bodies (river and geothermal arrangements, the Te Urewera board), Treaty-clause statutes, and the state's legitimacy narrative all presuppose the consent condition. Overnight removal would void the consideration underlying settled claims, strand co-governance statutes, and reopen the foundational dispute the arrangement has managed since 1840 — the realistic alternatives are renewed conflict or unchecked majoritarian rule.
% FOUNDING_PROBLEM: In 1840: bringing the Crown's own subjects and speculative land companies under law in a territory where Māori polities held effective authority, while securing for rangatira a protective alliance against musket-era instability, foreign encroachment, and settler land hunger. Each party signed to solve its own version of one problem: ordering shared territory.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Waitangi Tribunal's Te Paparahi o te Raki report (2014) found rangatira did not cede sovereignty in the North — a statutory inquiry that heard the Crown's own case; the Court of Appeal's partnership articulation in NZ Māori Council v Attorney-General (1987) arose from litigation Māori brought against the Crown, not from Crown self-description; and constitutional scholars outside both camps (Brookfield, McHugh, Palmer) treat consent-bounded authority as a serious account of the founding bargain. No attestation comes only from the parties that benefit.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).
:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (end-state 0.30) because the arrangement transfers little by design — it bounds transfer rather than executing it; its costs are negotiation and compliance burdens, which rose slightly as enforceability grew after 1975 and 1987. Suppression (0.38) reflects that the arrangement coerces little intrinsically — consent is its anti-coercion core — but the suppression_requirement series tracks the enforcement capacity needed to HOLD it against the stronger party: near-zero reliance in 1840, escalation through the invasion wars and the judicial-refusal era (peak 0.78 circa 1900), then institutionalized decline as statute and case law internalized the duty. Theater peaked (~0.74, 1880–1945) when the Treaty was praised rhetorically while its requirements were denied, then fell as the Tribunal, the Lands Case, and the settlements era gave it functioning teeth. Accessibility collapse is low (0.30): the alternatives — assimilation, subjugation, parallel statehood — remain live political positions the arrangement does not close off. Resistance is high (0.75): a century of warfare, judicial refusal, and legislative override is the historical record of what meeting this arrangement cost. All three tracked series share one time grid; every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the settler-commercial seat the arrangement presents as obstruction — a hurdle taxing every project, borne by the class with the least say in its terms. From the iwi and hapū seat it presents as the surviving guarantee of retained authority. From the Crown seat it is double: the source of the mandate (without the bargain, kāwanatanga has no foundation on this reading) and a standing limit on action. The judiciary and Tribunal seats compute stewardship — neither collecting nor paying. The engine derives these per-seat classifications from the structural data; the divergence between the payer seat's likely extractive-flavored computation and the beneficiary seat's coordination-flavored computation is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive the derivation: iwi_and_hapu sit near the full-beneficiary end (protected authority, identity-locked — no arbitrage-grade exit exists because the relationship is constitutive), and crown_executive derives beneficiary-direction from its declaration. The Crown's true position is nearer symmetric than the derivation alone suggests: it collects the mandate but pays in constrained action, redress liability, and co-governance duties — a net d around 0.35. No directionality_override is authored because the schema keys overrides by power atom, and an override on 'institutional' would also capture the judiciary and Tribunal seats, whose position as non-collecting stewards the derivation already handles correctly; the Crown's dual position is documented here instead. Paakeha settler-commercial interests derive high target-direction as the declared cost-bearing class — their projects bear the consent condition — moderated somewhat by the peace-and-order dividend they collect. Urban non-affiliated Māori are excluded rather than seated: they bear the arrangement's representational costs without access to its consent channel.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (status: contested), so no mandatrophy is declared. Classifying the arrangement as rope guards against two opposite errors. Reading it as pure extraction erases the genuine coordination achievement: the counterfactuals to the partnership are war or subjugation, and the arrangement solved a real collective-action problem between polities. Reading it as costless harmony erases who pays: the consent condition taxes settler-commercial projects and constrains the Crown materially. The snare-flavored portion of the history — takings that rode on the translation gap — is quarantined in the retrospective_snare_exposure sibling so that this file's ε stays invariant and the coordination function stays visible. If the founding problem were ever resolved (a settled constitutional compact both parties ratify), this arrangement would complete its transition and the story would convert to a resolved-mandate record.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the kernel treaty_authority_cession. If the crown_cession_reading prevailed instead, what happens to this constraint''s structure?',
    'Doctrinal and political resolution of which text controls and what was transferred — binding appellate treatment of the textual question, Crown acceptance of Tribunal findings, or a negotiated constitutional settlement ratified by both parties.',
    'Under the cession reading this arrangement dissolves into a completed historical transfer: no ongoing consent condition survives, the beneficiary structure collapses to the Crown alone, and the classification migrates from coordination toward a spent instrument. Under the snare-exposure variant the translation asymmetry becomes the primary structure and the classification migrates toward pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: which reading of the Treaty-authority kernel this constraint instantiates, and what sibling readings would change structurally.').

omega_variable(
    hapu_consent_representation,
    'Whose agreement counts as hapū consent — post-settlement iwi corporations and mandated representatives, or some other formation?',
    'Observation of consent practice: which bodies actually grant agreement, on what mandate, and whether dissenting hapū within iwi boundaries accept the results attributed to them.',
    'If corporate structures count as consent, the arrangement operates smoothly but the consent condition risks becoming formal; if only direct hapū mandate counts, transaction costs rise sharply and many Crown acts currently treated as agreed become unauthorized — raising both the arrangement''s friction and its fidelity to the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hapu_consent_representation, empirical, 'Operationalization of the consent condition — who may speak for hapū.').

omega_variable(
    breach_epsilon_boundary,
    'How much of the observed historical dispossession belongs to this arrangement''s own operation versus the sibling structure in which the textual divergence itself is the taking mechanism?',
    'Counterfactual comparison across the kernel family: model the same history under the cession reading (takings as lawful completion of cession and purchase) and under the snare-exposure reading (mistranslation doing the work), and attribute the residuals.',
    'Drawing the boundary low keeps this file''s ε near coordination cost and the computed type near rope; drawing it high imports the breach history into this ε and pushes computed classifications toward hybrid or extractive types — the divergence between claim and computation would then measure the import, not the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(breach_epsilon_boundary, conceptual, 'ε-invariance boundary between the partnership reading and the retrospective snare-exposure sibling.').

omega_variable(
    contra_proferentem_treaty_fit,
    'Does contra proferentem — a canon built for contracts between parties of unequal drafting power — properly extend to a founding constitutional compact between polities?',
    'Comparative doctrinal analysis: how other jurisdictions resolve indigenous-treaty text conflicts (United States Indian canons, the Canadian Sparrow line) and whether the extension rests on drafting-power asymmetry or on independent grounds such as the honour of the Crown.',
    'If the canon fits, the Māori-text-control premise is doctrinally secure and this reading''s foundation holds; if it does not, the reading needs a different footing and its stability weakens — shifting weight onto the deontological rangatiratanga axiom alone.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contra_proferentem_treaty_fit, conceptual, 'Whether the interpretive canon anchoring Māori-text primacy is sound for treaties as opposed to private contracts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.3).
narrative_ontology:measurement(trea_tr_t1860, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1860, 0.5).
narrative_ontology:measurement(trea_tr_t1880, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1880, 0.68).
narrative_ontology:measurement(trea_tr_t1900, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1900, 0.72).
narrative_ontology:measurement(trea_tr_t1920, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1920, 0.74).
narrative_ontology:measurement(trea_tr_t1945, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1945, 0.7).
narrative_ontology:measurement(trea_tr_t1975, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1975, 0.55).
narrative_ontology:measurement(trea_tr_t1987, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1987, 0.4).
narrative_ontology:measurement(trea_tr_t2000, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2000, 0.32).
narrative_ontology:measurement(trea_tr_t2025, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.18).
narrative_ontology:measurement(trea_be_t1860, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1860, 0.24).
narrative_ontology:measurement(trea_be_t1880, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1880, 0.26).
narrative_ontology:measurement(trea_be_t1900, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1900, 0.27).
narrative_ontology:measurement(trea_be_t1920, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1920, 0.26).
narrative_ontology:measurement(trea_be_t1945, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement(trea_be_t1975, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1975, 0.27).
narrative_ontology:measurement(trea_be_t1987, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1987, 0.29).
narrative_ontology:measurement(trea_be_t2000, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2000, 0.31).
narrative_ontology:measurement(trea_be_t2025, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2025, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1840, 0.15).
narrative_ontology:measurement(trea_su_t1860, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1860, 0.6).
narrative_ontology:measurement(trea_su_t1880, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1880, 0.75).
narrative_ontology:measurement(trea_su_t1900, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1900, 0.78).
narrative_ontology:measurement(trea_su_t1920, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1920, 0.76).
narrative_ontology:measurement(trea_su_t1945, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1945, 0.72).
narrative_ontology:measurement(trea_su_t1975, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(trea_su_t1987, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 1987, 0.5).
narrative_ontology:measurement(trea_su_t2000, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(trea_su_t2025, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, biculturalism_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% Constraint family for the kernel treaty_authority_cession, decomposed per the ε-invariance principle: the colloquial label 'the Treaty' covers structurally distinct claims with different ε values. This file authors the rangatiratanga_retention_reading — the partnership-consent arrangement (coordination profile, low-moderate ε). crown_cession_reading authors the completed-transfer claim (its ε is assessed against a finished historical transaction). retrospective_snare_exposure authors the translation-asymmetry mechanism itself (high ε; the divergence between the texts as the instrument of taking). This reading is upstream of the snare-exposure sibling: its Māori-text-control premise supplies the baseline that makes mistranslation legible as taking at all, and it exerts downstream pressure on the cession reading via Tribunal findings and the contra proferentem canon without having displaced it — the Crown still proceeds formally on cession-plus-principles. All family members link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
