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
 *   human_readable: Crown Cession Reading of the Treaty of Waitangi — Unitary Sovereignty Enclosure
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story authors the crown_cession_reading of the Treaty of Waitangi as
 *   an operative constitutional constraint: the English text is
 *   authoritative, kawanatanga denotes the full cession of sovereignty, and
 *   the 1840 signing therefore completes a legal transfer of land and
 *   legislative authority to the Crown. The standing arrangement under
 *   contest — the thing this story is about — is the resulting enclosure: a
 *   single legal order in which land title, legislation, and adjudication run
 *   through Crown institutions, Maori customary authority is unrecognized or
 *   subordinate, and the alienation of Maori land is rendered procedurally
 *   legitimate. The claim and the metrics are authored independently: I claim
 *   tangled_rope because the structure carries a real coordination function
 *   (one administrable legal order) and a real asymmetric transfer (land and
 *   authority moving from communal holders to Crown and settlers) through the
 *   same machinery; the metric values record what I judge descriptively true
 *   of its operation, including a suppression arc that peaked during the
 *   land-war and land-court decades and has since decayed into administrative
 *   management. Family note: this is one of three linked readings of the
 *   treaty_authority_cession kernel; the siblings are separate files with
 *   their own epsilon and victim sets (see network.dual_formulation_note and
 *   commentary.kernel_context).
 *
 * KEY AGENTS:
 *   - crown_government: Agenda-setter and dual-positioned beneficiary seat (institutional/arbitrage) — administers the enclosure, writes its boundaries, and collects revenue along the transfer path
 *   - european_settlers: Principal recipient of transferred land (organized/mobile) — the terminal concentration point of the land asset
 *   - land_speculators: Intermediary beneficiaries (organized/arbitrage) — capitalize and accelerate the transfer
 *   - maori_iwi_hapu: Primary target (powerless/identity_locked) — bears the land loss and the subordination of their own law
 *   - rangatira_signatories: Signatory generation, dual-positioned (moderate/identity_locked) — gave marks under one text, were governed under another
 *   - kotahitanga_parliament: Excluded voice (organized/trapped) — sought co-legislative standing and was refused entry to the conversation
 *   - waitangi_tribunal: Analytical observer (institutional/analytical) — hears the history, recommends remedies, cannot bind
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__crown_cession_reading, 0.62).
domain_priors:suppression_score(treaty_authority_cession__crown_cession_reading, 0.46).
domain_priors:theater_ratio(treaty_authority_cession__crown_cession_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(treaty_authority_cession__crown_cession_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__crown_cession_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__crown_cession_reading, "Crown Cession Reading of the Treaty of Waitangi — Unitary Sovereignty Enclosure").
narrative_ontology:topic_domain(treaty_authority_cession__crown_cession_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__crown_cession_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__crown_cession_reading, 'd4f4b587-d657-4707-a91c-a0895915aa10').
narrative_ontology:cs_kernel_codification('d4f4b587-d657-4707-a91c-a0895915aa10', fixed_text).
narrative_ontology:cs_authority_grounding('d4f4b587-d657-4707-a91c-a0895915aa10', lineage).
narrative_ontology:cs_interpretation_layer_present('d4f4b587-d657-4707-a91c-a0895915aa10').
narrative_ontology:cs_reading_relation('d4f4b587-d657-4707-a91c-a0895915aa10', treaty_authority_cession__rangatiratanga_retention_reading, coexists_with).
narrative_ontology:cs_reading_relation('d4f4b587-d657-4707-a91c-a0895915aa10', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('d4f4b587-d657-4707-a91c-a0895915aa10', foundational, english_text_controls_treaty_meaning).
narrative_ontology:cs_axiom_status(english_text_controls_treaty_meaning, holdable).
narrative_ontology:cs_axiom_grounding('d4f4b587-d657-4707-a91c-a0895915aa10', english_text_controls_treaty_meaning, conventional).
narrative_ontology:cs_axiom('d4f4b587-d657-4707-a91c-a0895915aa10', foundational, kawanatanga_denotes_full_sovereignty).
narrative_ontology:cs_axiom_status(kawanatanga_denotes_full_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d4f4b587-d657-4707-a91c-a0895915aa10', kawanatanga_denotes_full_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('d4f4b587-d657-4707-a91c-a0895915aa10', secondary, cession_transfers_legislative_authority_completely).
narrative_ontology:cs_axiom_status(cession_transfers_legislative_authority_completely, holdable).
narrative_ontology:cs_axiom_grounding('d4f4b587-d657-4707-a91c-a0895915aa10', cession_transfers_legislative_authority_completely, conventional).
narrative_ontology:cs_reference_frame('d4f4b587-d657-4707-a91c-a0895915aa10', unitary_crown_sovereignty_by_cession).
narrative_ontology:cs_drift_state('d4f4b587-d657-4707-a91c-a0895915aa10', contemporary_post_tribunal_findings, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d4f4b587-d657-4707-a91c-a0895915aa10', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__crown_cession_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, crown_government).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, european_settlers).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, land_speculators).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(treaty_authority_cession__crown_cession_reading, rangatira_signatories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__crown_cession_reading, rangatira_signatories).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, english_text_primacy_doctrine).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, kawanatanga_full_sovereignty_translation).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, completed_cession_legal_positivism).
narrative_ontology:constraint_vindicates(treaty_authority_cession__crown_cession_reading, crown_preemption_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts governing authority over the islands on the strength of the English-language instrument, legislates for all inhabitants, acquires land through preemption, negotiated purchase, court-converted title, and wartime confiscation, and enforces compliance through courts, constabulary, and where resisted, military campaign. Collects customs revenue and land-sale proceeds along the way. It writes the boundary lines of the arrangement, so exit is not a category that applies at its seat.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, crown_government, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, crown_government, beneficiary).

% Arrive under assisted migration schemes, take up former Maori land through the Crown's monopoly purchases and later open-market transfers, and build farms, towns, schools, and a franchise on it. Their votes staff the legislature that administers the arrangement and their militia service enforced it in the 1860s. If returns disappoint, emigration to Australia or Britain remains open.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, european_settlers, beneficiary,
    organized, biographical, mobile, national).

% Capitalize the transfer: organized companies operating from London and the colony acquire vast tracts cheaply from the Crown or directly, subdivide, and resell at margin. Capital is mobile; a disappointing colonial market means liquidation and redeployment elsewhere.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, land_speculators, beneficiary,
    organized, immediate, arbitrage, global).

% Hold land communally through descent lines; between 1840 and 1930 lose the great majority of it through low-price purchase, court-imposed individualization of title, and confiscation after the wars, while their own law continues without state recognition. Remaining on ancestral ground is not one option weighed against others — the ground is constitutive of who they are — so departure is not experienced as available. Collective responses run through petition, parallel-parliament attempts, protest, and litigation.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, maori_iwi_hapu, payer,
    powerless, generational, identity_locked, national).

% Northern and other chiefs who marked the 1840 sheets, receiving blankets, tobacco, muskets, and trade goods, and understanding themselves to be securing a governor for settler disorder while retaining their own authority over their own people. Within a generation the legislative machine treats their authority as superseded; they age into a colony that cites their marks for a bargain most of them never read in its English form.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, rangatira_signatories, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__crown_cession_reading, rangatira_signatories, beneficiary).

% The pan-tribal assembly of the 1890s: passes its own laws, levies its own committees, and petitions the Queen and the colonial House for recognized co-legislative status. The General Assembly declines recognition and its proposals never enter the constitutional conversation; it has no procedural door by which to become a party.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, kotahitanga_parliament, excluded,
    organized, generational, trapped, national).

% Standing commission of inquiry hearing claims that the Crown failed to honor its promises, taking historical evidence from all sides and reporting findings with recommendations, including settlement packages. Its findings carry moral and quasi-legal weight; its recommendations bind the government only when the government chooses to adopt them.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__crown_cession_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__crown_cession_reading, european_settlers).
narrative_ontology:fixing_cost_class(treaty_authority_cession__crown_cession_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one legal order across the islands: uniform statute law, registered title, courts formally open to chief and settler alike, and a single external face for trade and defense — replacing a patchwork of chiefly jurisdictions and company improvisation with a single administrable system.
% TRANSFER_FUNCTION: Moves land — first via Crown preemption at administered prices, then via court-converted individual titles and outright confiscation — from communal Maori holding to the Crown and onward to settler owners; moves legislative authority from rangatira councils to the colonial General Assembly; moves goods, cash, and protection promises toward Maori signatories.
% ABSENT_VOICES: Most chiefs signed only the Maori text, whose sovereignty clause differs decisively from the English; their assent was never given to the wording now treated as controlling. The pan-Maori parliaments of the 1890s asked for a seat in the constitutional conversation and were refused. Wahine rangatira with decision authority in many hapu stood largely outside the signing conversation.
% DISAPPEARANCE_RATIONALE: Every fee-simple title in the country traces through the Crown-acquisition premise; the General Assembly's plenary authority, the courts' jurisdiction over Maori land, and the state's capacity to be a treaty-settlement counterparty all presuppose the cession reading. Overnight removal would unravel title chains, strand the legislature's warrant, and reopen every settled and unsettled claim at once.
% FOUNDING_PROBLEM: Regularize the British presence before private settlement companies precipitated disorder: the New Zealand Company was selling land it did not clearly hold, French annexation loomed, settler-Maori violence was escalating, and the Colonial Office wanted a single orderly instrument acquiring both sovereignty and a land pipeline.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the Waitangi Tribunal's Te Paparahi o Te Raki inquiry (2014) found that the northern chiefs did not cede sovereignty in 1840; the historiography descending from Claudia Orange documents the textual divergence and the chiefs' recorded understandings; He Whakaputanga (1835), recognized by the Crown itself, asserts pre-existing Maori sovereignty. Successive governments attest the opposite — that cession was completed and remains the constitutional foundation — which is precisely why the status is contested rather than dead.
narrative_ontology:disappearance_verdict(treaty_authority_cession__crown_cession_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__crown_cession_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__crown_cession_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__crown_cession_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__crown_cession_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.62: the transfer was enormous in kind (roughly ninety-five percent of the Maori land base gone by the late 1930s) but ran substantially through priced transactions, however unequal, and modern settlements return a small fraction — the reading's own lights price the founding bargain as exchange, which caps epsilon below what a breach-framing would yield. Suppression 0.46 is the current raw structural intensity: the enforcement machinery (registered title, police powers, refusal to recognize parallel authority) exists and is used, but nothing like the 1860s-1900s coercive peak; suppression is authored unscaled, as a structural property — only extractiveness gets scaled by directionality and scope in the engine. Theater 0.46: the protective and partnership vocabulary performs heavily in contemporary state practice while the authority wall stands untouched, but settlements and co-governance arrangements do real allocative work, keeping the ratio below majority. Accessibility collapse 0.60: once sovereignty was asserted, no parallel legal order was tolerated (the Kingitanga's alternative was crushed), yet customary practice persisted de facto for decades and modern co-governance partially reopens alternatives. Resistance 0.70: petition, the Kingitanga and Kotahitanga coalitions, armed and passive resistance (Parihaka), and fifty years of Tribunal-era litigation — sustained, organized, and documented across generations; the coalition attempts matter for the powerless target seat and were met with force and refusal respectively. The measurement series share one seven-point grid (1840-2026) with all three metrics authored at every point: extraction peaks in the land-court era (~1907) as the last accessible land converts, then declines as there is simply less left to move; suppression ratchets sharply upward 1840-1865 (wars, confiscation), decays through the twentieth century as assimilation is normalized and force becomes unnecessary, and flattens in the management era; theater climbs as the justification migrates from settlement necessity to protection to partnership performance, peaking in the bicultural-rhetoric decade before settlements began doing visible work.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the crown_government seat the arrangement is the constitutional foundation it administers — a completed, lawful cession that merely needs housekeeping; from the maori_iwi_hapu seat the same structure operates as an enclosure that converted their inherited ground into someone else's fee-simple estate while refusing to recognize the law they actually lived under; from the settler seat it is a purchase history, imperfect but paid. The identity-lock dynamic is load-bearing for the target seat: whakapapa-to-whenua is a relational identity, not a residence preference, so exit was never a live option to be suppressed — which is why the constraint needed so little anti-exit enforcement against Maori specifically and could concentrate its coercive budget on suppressing internal alternatives (the Kingitanga, the tohunga, the language). Post-war urban migration physically dispersed the population without breaking the whakapapa frame; it rerouted the resistance through cities, universities, and courts instead. Had that frame broken, the target's directionality would soften and the whole profile would shift; it has not.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the low-d end: crown_government (dual-positioned agenda-setter/beneficiary, arbitrage exit) derives near the beneficiary pole; european_settlers (mobile exit, terminal recipients of the land asset) and land_speculators (arbitrage exit) likewise. Targets sit at the high-d end: maori_iwi_hapu combine victim position, identity_locked exit, and — within the imposed order — negligible countervailing power, placing them near the full-target pole; rangatira_signatories are pulled slightly back from it by their dual position (they received consideration and nominal protection) and their moderate pre-1840 authority. The excluded kotahitanga_parliament seat feeds the consensus-provenance check rather than directionality: the unanimity of the constitutional settlement was produced partly by refusing entry to the party that would have disputed its premise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — orderly colonization plus a secured land pipeline — was accomplished within roughly seventy years of signing. What persists is the authority wall the founding built, now maintained under a protective/partnership vocabulary whose original referent (Crown protection of Maori interests) atrophied almost completely by the early twentieth century. The R5 mismatch read applies: founding_problem_status is contested (not cleanly dead, because the legitimacy characterization is itself the live dispute) while disappearance_verdict is world_rearranges — the arrangement is load-bearing regardless of which genealogy is true. The rising theater series tracks the mandate gap: performance fills the space where the protective function used to be. This is not yet a piton — the beneficiary set is concentrated (settled titleholders, the state itself), enforcement is real rather than ceremonial, and fixing is prohibitive rather than merely neglected — but the trajectory from enforcement-held enclosure toward rhetorically-maintained enclosure is exactly the drift the temporal series is positioned to catch. The tangled_rope claim is what keeps the mandatrophy analysis honest in both directions: the coordination function (one legal order) is genuine, so the structure is not a pure snare wearing a treaty costume; the transfer was asymmetric and identity-locked on the paying side, so it is not a pure rope either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operative_reading_instantiation,
    'This constraint is one reading of the treaty_authority_cession kernel — the crown_cession_reading. Which reading the legal system actually instantiates determines the entire structural profile: victim set, beneficiary set, and epsilon all change under the sibling readings.',
    'Observe which text the courts treat as controlling, which translation of kawanatanga the judiciary adopts, and whether partnership-and-consent obligations enter binding doctrine (rangatiratanga_retention_reading) or whether the divergence itself is judicially named as the operative wrong (retrospective_snare_exposure).',
    'Under the retention reading the standing arrangement is a breached partnership with a living counterparty (different victims, different epsilon, likely higher extraction); under the snare-exposure reading the mistranslation is itself the mechanism and the classification pressures toward snare. This file''s numbers are valid only for the cession reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operative_reading_instantiation, conceptual, 'Committer-frame indexicality: per-reading constraint identity for the treaty_authority_cession kernel.').

omega_variable(
    cession_actuality,
    'Did the chiefs who signed in 1840 actually assent to ceding sovereignty — did they understand kawanatanga as the complete transfer of governing authority the English text asserts?',
    'Philological reconstruction of 1840-era northern Maori usage of kawanatanga and tino rangatiratanga, contemporaneous oral testimony records, missionary correspondence, and the Waitangi Tribunal''s Te Paparahi o Te Raki findings.',
    'If cession-in-understanding fails, the doctrine''s warrant reduces to assertion backed by enforcement, raising effective extraction and pushing classification toward snare; if it holds, part of the measured transfer is the price of a (unequal but consensual) constitutional bargain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cession_actuality, empirical, 'Whether the founding transfer was assented-to or imposed under semantic divergence.').

omega_variable(
    textual_divergence_extraction_role,
    'Is the Maori-text/English-text divergence an incidental drafting artifact, or does it function as the mechanism by which land and authority moved — assent gathered under one text, enforcement run under another?',
    'Archival tracing of what each signatory text promised versus what was subsequently enforced, cross-referenced against land-transfer volumes following signing; the retrospective_snare_exposure sibling story develops this hypothesis in full.',
    'If the divergence is functional, the cession reading''s own legitimacy account collapses and this constraint''s epsilon is understated; if incidental, the divergence is background noise and the extraction must be explained by ordinary purchase and conquest dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_divergence_extraction_role, empirical, 'Whether mistranslation is the load-bearing extraction mechanism or a side condition.').

omega_variable(
    enforcement_vs_inertia_persistence,
    'Does the enclosure persist because active enforcement holds it (title registration, police powers, refusal of parallel authority) or because institutional inertia makes it cheaper to leave standing?',
    'Counterfactual analysis of enforcement-withdrawal episodes: periods when the Crown declined to enforce (e.g., tolerance of continuing customary occupation) versus periods of active enforcement (land court sittings, confiscation, arrest of protesters), and whether the arrangement reverted or held.',
    'Enforcement-dependent persistence supports the tangled_rope/snare side of the profile; pure inertia would push toward piton dynamics — but the concentrated beneficiary set (settled title holders, the state itself) makes pure inertia unlikely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_inertia_persistence, empirical, 'Persistence basis: active enforcement versus institutional drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__crown_cession_reading, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t1840, treaty_authority_cession__crown_cession_reading, theater_ratio, 1840, 0.28).
narrative_ontology:measurement(trea_tr_t1865, treaty_authority_cession__crown_cession_reading, theater_ratio, 1865, 0.2).
narrative_ontology:measurement(trea_tr_t1885, treaty_authority_cession__crown_cession_reading, theater_ratio, 1885, 0.24).
narrative_ontology:measurement(trea_tr_t1907, treaty_authority_cession__crown_cession_reading, theater_ratio, 1907, 0.32).
narrative_ontology:measurement(trea_tr_t1950, treaty_authority_cession__crown_cession_reading, theater_ratio, 1950, 0.42).
narrative_ontology:measurement(trea_tr_t1985, treaty_authority_cession__crown_cession_reading, theater_ratio, 1985, 0.5).
narrative_ontology:measurement(trea_tr_t2026, treaty_authority_cession__crown_cession_reading, theater_ratio, 2026, 0.46).

% Extraction over time
narrative_ontology:measurement(trea_be_t1840, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1840, 0.42).
narrative_ontology:measurement(trea_be_t1865, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1865, 0.68).
narrative_ontology:measurement(trea_be_t1885, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1885, 0.74).
narrative_ontology:measurement(trea_be_t1907, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1907, 0.76).
narrative_ontology:measurement(trea_be_t1950, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(trea_be_t1985, treaty_authority_cession__crown_cession_reading, base_extractiveness, 1985, 0.64).
narrative_ontology:measurement(trea_be_t2026, treaty_authority_cession__crown_cession_reading, base_extractiveness, 2026, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t1840, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1840, 0.35).
narrative_ontology:measurement(trea_su_t1865, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1865, 0.78).
narrative_ontology:measurement(trea_su_t1885, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1885, 0.72).
narrative_ontology:measurement(trea_su_t1907, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1907, 0.66).
narrative_ontology:measurement(trea_su_t1950, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement(trea_su_t1985, treaty_authority_cession__crown_cession_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement(trea_su_t2026, treaty_authority_cession__crown_cession_reading, suppression_requirement, 2026, 0.46).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__crown_cession_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, rangatiratanga_retention_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__crown_cession_reading, retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% Constraint family for the treaty_authority_cession kernel, decomposed per the epsilon-invariance principle: 'what the Treaty established' is three structurally distinct constraints, not one observable-dependent claim. This file authors the crown_cession_reading (English text controls; kawanatanga equals full sovereignty; cession completes) with its epsilon priced as that reading sustains the standing arrangement. rangatiratanga_retention_reading authors the same history under contra proferentem and retained tino rangatiratanga — different victim set, different epsilon. retrospective_snare_exposure authors the textual divergence itself as the extraction mechanism — highest epsilon of the family. The cession reading is the historically dominant, upstream member: its adoption created the enforcement conditions the exposure reading diagnoses, hence the influences edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
