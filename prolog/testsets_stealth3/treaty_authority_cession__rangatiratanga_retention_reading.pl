% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Tiriti Partnership Covenant — Rangatiratanga Retention Reading
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   This story instantiates one reading of the founding instrument of
 *   Aotearoa New Zealand: the reading in which the Māori text controls
 *   interpretation (contra proferentem against the drafting party),
 *   kāwanatanga is limited to delegated governance, tino rangatiratanga was
 *   retained by the chiefs, and the instrument therefore establishes a
 *   continuing partnership in which Crown action is legitimate only with
 *   ongoing hapū consent. The standing arrangement under assessment is the
 *   actual Crown–hapū authority relationship operating under that covenant
 *   since 1840. Assessed by this reading's own lights, the arrangement
 *   carries a genuine coordination function — it provided the framework for
 *   two polities to coexist where the settler-colonial default was
 *   dispossession or elimination, it anchors every modern institution of
 *   redress, and its guarantee clauses remain the legal basis for
 *   rangatiratanga claims — while substantial asymmetric extraction runs
 *   through the same structure: landholding fell from effectively total Māori
 *   possession in 1840 to a small fraction within eighty years, facilitated
 *   by the gap between what the chiefs assented to and what the administering
 *   institutions enforced. Family documentation (per the ε-invariance
 *   decomposition rule): this is one of three readings of kernel
 *   treaty_authority_cession, each a separate file with its own ε. The
 *   crown_cession_reading authors the same standing arrangement as a
 *   completed, lawful transfer of sovereignty (near-zero extraction from its
 *   seat); the retrospective_snare_exposure reading authors the textual
 *   divergence itself as the extraction mechanism (the highest ε of the
 *   family). This file's ε (0.63 end-state) reflects the middle position:
 *   real covenant, heavily breached operation, extraction continuing through
 *   unresolved claims and unilateral governance.
 *
 * KEY AGENTS:
 *   - crown_governing_apparatus: agenda setter (institutional/constrained) — administers the arrangement, sets settlement terms, collects the historical land and jurisdiction transfer; cannot exit without forfeiting its own legitimacy claim
 *   - hapu_collectives: primary target and nominal guarantee-holder (organized/identity_locked) — bore the extraction; hold retained authority under the covenant; locked in by whakapapa and whenua
 *   - settler_descendant_polity: principal secondary beneficiary (powerful/mobile) — inherits the transferred estate with voluntary, reversible commitment
 *   - unresolved_raupatu_hapu: concentrated target (moderate/identity_locked) — carry the sharpest uncompensated losses
 *   - urban_maata_waka: excluded voice (moderate/mobile) — outside the iwi-based representation architecture
 *   - waitangi_tribunal: analytical observer (institutional/analytical) — produces the fidelity record without enforcement power
 *   - constitutional_judiciary: analytical observer (institutional/analytical) — articulates principles case by case under parliamentary supremacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.63).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.55).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, tangled_rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Tiriti Partnership Covenant — Rangatiratanga Retention Reading").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '04d24ad5-8bad-4822-8e39-1e1a8e93cb7a').
narrative_ontology:cs_kernel_codification('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', fixed_text).
narrative_ontology:cs_authority_grounding('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', lineage).
narrative_ontology:cs_interpretation_layer_present('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a').
narrative_ontology:cs_reading_relation('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', foundational, maori_text_controls_interpretation).
narrative_ontology:cs_axiom_status(maori_text_controls_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', maori_text_controls_interpretation, conventional).
narrative_ontology:cs_axiom('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', foundational, rangatiratanga_not_ceced).
narrative_ontology:cs_axiom_status(rangatiratanga_not_ceced, holdable).
narrative_ontology:cs_axiom_grounding('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', rangatiratanga_not_ceced, deontological).
narrative_ontology:cs_axiom('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', secondary, crown_legitimacy_requires_hapu_consent).
narrative_ontology:cs_axiom_status(crown_legitimacy_requires_hapu_consent, holdable).
narrative_ontology:cs_axiom_grounding('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', crown_legitimacy_requires_hapu_consent, instrumental).
narrative_ontology:cs_reference_frame('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', rangatira_sovereignty_delegated_kawanatanga).
narrative_ontology:cs_drift_state('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', post_wai1040_contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('04d24ad5-8bad-4822-8e39-1e1a8e93cb7a', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, crown_governing_apparatus).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, settler_descendant_polity).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, hapu_collectives).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, hapu_collectives).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, unresolved_raupatu_hapu).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, contra_proferentem_against_the_drafting_party).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, tino_rangatiratanga_guarantee).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, ongoing_consent_partnership_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the terms of every settlement negotiation, drafts the legislation that interprets the founding instrument, and controls the public estate and the regulatory levers over land, water, and coast. Received the historical transfer of land and holds continuing jurisdictional primacy; extends apologies, settlement packages, and co-governance seats on its own timetable. Leaving the arrangement outright would mean abandoning the founding instrument of its own legitimacy, so it manages the relationship rather than exits it — periodically testing reinterpretation against organized resistance.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_governing_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Hold whakapapa-based authority over their territories that predates the founding instrument; under this reading they retained absolute chiefly authority while delegating governance functions. Across the arrangement's operation they transferred the overwhelming share of land, forests, fisheries, and coercive jurisdiction, receiving protection guarantees, settlement consideration, and co-governance participation that arrive late, fractionally, and on the counterparty's terms. Exit would mean dissolving the kinship-to-place ties that constitute them as collectives, so they negotiate from inside the structure whatever its terms.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, hapu_collectives, payer,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, hapu_collectives, beneficiary).

% Inherits the land base, infrastructure, and demographic majority built on the transferred estate; holds undisturbed fee-simple title and electoral dominance over the arrangement's terms. Many hold foreign citizenship and emigration pathways, so their commitment to the arrangement's continuation is voluntary rather than captive; they bear its costs mainly as contested legitimacy and occasional co-governance adjustments.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, settler_descendant_polity, beneficiary,
    powerful, generational, mobile, national).

% Hapū whose lands were militarily confiscated and whose claims remain wholly or partly unsettled. They carry the arrangement's sharpest concentrated losses — productive land, autonomous institutions, displaced populations — while waiting on counterparty-controlled negotiation sequencing; their consent is sought last and priced lowest.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, unresolved_raupatu_hapu, payer,
    moderate, generational, identity_locked, regional).

% Indigenous residents living outside their ancestral territories and outside mandated-iwi representation, whose interests in housing, health, and urban taonga fall between the settlement architecture's iwi-based seats. They would contest the arrangement's representation model — who may speak for whom inside consent mechanisms — but hold no standing in counterparty negotiation mandates or territory-bound inquiry proceedings.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, urban_maata_waka, excluded,
    moderate, biographical, mobile, national).

% Commission of inquiry hearing indigenous claims against Crown acts and omissions since 1840; produces the historical and juridical record against which the arrangement's fidelity is tested. Recommends remedies without binding force; its findings reshape the interpretive terrain while leaving enforcement with the counterparty.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Superior courts articulating treaty principles — partnership, active protection, redress — inside ordinary law. They mediate between parliamentary supremacy and the covenant's guarantees case by case, holding no power to entrench the consent requirement against repeal.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__rangatiratanga_retention_reading, crown_governing_apparatus).
narrative_ontology:fixing_cost_class(treaty_authority_cession__rangatiratanga_retention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the cohabitation of two polities — a governing state and kin-based collectives holding retained territorial authority — on one territory: it defines who may govern what, guarantees protection of the collectives' treasures and taonga, and channels inter-polity disputes into negotiation rather than war.
% TRANSFER_FUNCTION: Moves land, jurisdictional authority, and resource control from hapū collectives to the Crown and the settler economy; moves protection guarantees, settlement consideration, and co-governance participation back from Crown to hapū. The flows are asymmetric: under translation asymmetry, the larger movement ran toward the Crown.
% ABSENT_VOICES: Urban Māori outside mandated-iwi representation; hapū whose claims remain unsettled; future generations whose inheritance is transacted now; and — historically decisive — the rangatira signatories themselves, whose understanding (the Māori text) was never the operative one in the institutions that administered the arrangement. They sit outside cabinet, outside select committees, and outside the counterparty's mandate-setting process.
% DISAPPEARANCE_RATIONALE: If the partnership constraint vanished overnight, the Crown would govern purely unilaterally and the retained-authority claim would lose its legal anchor: remaining collective land protections, settlement deeds, and co-governance instruments rest on the covenant and would dissolve with it, returning the constitutional question to force or secession. Every current institution of redress presupposes this arrangement.
% FOUNDING_PROBLEM: How can a colonizing power and the sovereign kin-collectives of these islands share one territory without permanent war — securing the newcomers' governance needs while guaranteeing the chiefs' continued authority over their lands, villages, and treasures?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Waitangi Tribunal district inquiries (notably the Te Paparahi o Te Raki / Wai 1040 findings) attest that the chiefs did not surrender their authority; superior-court jurisprudence repeatedly acknowledges the constitutional bargain remains unfinished; and the Matike Mai Aotearoa report (2016) documents hapū testimony nationwide that the founding terms have never been implemented as written. The counterparty itself concedes specific breaches through formal apologies, corroborating that the founding problem — the unimplemented terms — is live.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (end-state 0.63, peak 0.90 around t=70) because the standing arrangement moved nearly the entire land base and coercive jurisdiction from hapū to the Crown while the compensating flows (guarantees, settlements, co-governance) arrived late and fractional — total settlement consideration is a small percentage of conservatively estimated losses, and major claim clusters (Taranaki, freshwater, coastal customary title) remain open. Suppression (0.55 end-state) is structural first: parliamentary supremacy forecloses any rangatiratanga exit regardless of overt force; the series traces the decline of the overt-force component (wars, confiscation enforcement, constabulary coercion peaking at 0.86) into administrative and legislative suppression, with a visible re-hardening at the foreshore-and-seabed override (t=160) and the current reinterpretation pressure (t=185). Theater (0.46 end-state) is moderate-high: partnership, protection, and participation rhetoric and commemorative activity have grown faster than actual consent-transfer, though the inquiry machinery and binding settlement deeds keep the ratio below piton territory. Accessibility_collapse (0.60): full-rangatiratanga constitutional models remain articulated and litigable but are foreclosed in practice by supremacy, while co-governance experiments preserve partial openings — alternatives are neither fully available nor fully collapsed. Resistance (0.78) is continuous and defining across the whole interval: armed resistance in the wars, passive resistance at Parihaka, the Kingitanga's persistence, the 1975 land march, Bastion Point, the 2004 foreshore mobilization, and the 2024 hīkoi — the arrangement has never operated without organized pushback. The dynamics are not cyclical but rise-crash-partially-remedy-with-reintensification; the post-t=130 dip in theater and extraction marks the inquiry-and-settlement era forcing real function back into the arrangement. All three series share one ten-point grid spanning 1840 (t=0) to 2025 (t=185). Claim and metrics are independent: the claim is tangled_rope because both functions demonstrably run through the structure; the metrics describe the arrangement's actual operation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the hapū seats, the standing arrangement operates as extraction with a coordination shell: the covenant's guarantee side perpetually deferred, the transfer side long since executed — expect high effective extraction amplified by identity-locked exit. From the Crown seat, the arrangement is a net asset (legitimacy, land, jurisdiction) while the covenant's consent demand registers as a constraint on future discretion — the same structure is subsidy backward and restraint forward. The settler-descendant seat experiences mild cost (contested legitimacy, occasional co-governance adjustment) with mobile exit damping any extraction attributed to it. The analytical seats see the whole: a covenant whose text supports one allocation and whose administration performed another. The divergence between the payer-seat computation and the agenda-setter-seat computation is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. crown_governing_apparatus sits at the beneficiary end (received the transfer, controls the rules; d near 0). settler_descendant_polity is beneficiary with arbitrage-grade mobility pushing it further toward the subsidized end. hapu_collectives are declared both beneficiary (holders of the guarantee side) and victim (bearers of the transfer side); across the interval the victim side dominates decisively — the guarantees materialized late and fractionally — so their derived d sits near the full-target end, amplified by identity_locked exit. unresolved_raupatu_hapu derive the highest d in the story: concentrated uncompensated loss, regional scope, no exit. urban_maata_waka are excluded rather than coordinated; their exclusion is itself maintained by the iwi-based representation design and feeds no direct d. The analytical observers carry no positional weight. No directionality overrides are used: the beneficiary/victim declarations plus exit options already produce the correct differentiation, and an override keyed to a shared power atom (three institutional stakeholders with opposite positions) would corrupt rather than refine the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how two polities share authority on one territory — is live: the consent mechanism this reading requires has never been instituted, and the parties dispute what implementation would even look like. Status=live combined with verdict=world_rearranges is the consistent pairing; no zombie flag arises, and none should. The classification work here is preventive in both directions: labeling the standing arrangement pure rope would erase the extraction the reading itself insists is visible in the land record (that erasure is the crown-cession seat's characteristic error); labeling it pure snare would erase the genuine coordination that made coexistence possible and that now carries every institution of redress (that flattening is the snare-exposure seat's risk). Tangled rope holds both truths the reading holds: the covenant is real, and the extraction ran through it. Mandatrophy is not resolved and should not be declared — the arrangement's mandate is unfulfilled, not outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the rangatiratanga_retention_reading of kernel treaty_authority_cession; what structurally changes if a sibling reading is adopted instead?',
    'Comparative adjudication: adoption of the contra proferentem canon and plurilingual-treaty interpretive rules by the courts, or a constitutional settlement explicitly choosing among readings; cross-file comparison of the three stories'' computed classifications.',
    'Under crown_cession_reading the victim/beneficiary sets invert (hapū become grantees of rights rather than holders of retained authority, the Crown''s historical transfer becomes lawful consideration) and this story''s ε referent collapses; under retrospective_snare_exposure the arrangement''s coordination function is demoted to cover and the classification migrates toward snare. The kernel membership routes foreclosure computation across the family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the authority-cession kernel this story instantiates and what siblings would change.').

omega_variable(
    contra_proferentem_textual_control,
    'Does the contra proferentem canon validly assign textual control to the Māori version when the drafting party prepared and controlled both versions, or does the English text govern?',
    'Archival linguistics reconstructing 1840 te reo usage; Waitangi Tribunal Wai 1040 findings; comparison with international plurilingual-treaty interpretive rules; doctrinal development in the superior courts.',
    'If the English text controls, this constraint dissolves into the crown-cession instantiation — the retained-authority victim set converts into a granted-rights beneficiary set and ε collapses toward zero from the resulting seat. If the Māori text controls, the standing arrangement''s measured extraction is attributable to breach of a binding covenant rather than operation of a completed cession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contra_proferentem_textual_control, conceptual, 'Which text controls interpretation — the load-bearing premise separating this reading from its crown-cession sibling.').

omega_variable(
    historical_extraction_attribution,
    'How much of the measured extraction belongs to the standing arrangement''s own operation versus to breaches of it — does the arrangement extract, or does non-compliance extract while the arrangement forbids it?',
    'Counterfactual land-loss trajectories under a honored-covenant baseline versus actual history; causal tracing of specific instruments (Crown preemption, the Native Land Court, confiscation statutes, individualization regimes) classified as covenant operations versus covenant violations.',
    'If extraction is attributable to breach rather than the arrangement, the constraint''s own effective extraction falls toward rope levels and remedial obligation attaches to the breaching party rather than the structure — materially changing the computed classification for every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_extraction_attribution, empirical, 'Attribution of the land-record extraction between the covenant and its violation.').

omega_variable(
    ongoing_consent_operationalization,
    'What does ongoing hapū consent require operationally — veto, consultation-plus, co-decision — and at which scale (hapū, iwi, or national), such that partnership binds without paralysis?',
    'Constitutional design analysis (Matike Mai models, co-governance precedents), comparative federal-indigenous arrangements, and observed performance of existing co-management bodies.',
    'Determines whether the partnership function is executable at all: an unrealizable consent standard would push the reading''s aspiration toward scaffold-like transition status; a minimal one would collapse the consent requirement into the theatrical consultation the theater_ratio already detects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ongoing_consent_operationalization, conceptual, 'Operational content of the consent requirement — the reading''s central unspecifled term.').

omega_variable(
    settlement_sunset_question,
    'Do full-and-final settlement deeds constitute a sunset clause converting the arrangement toward transitional status, or do hapū retain open consent claims notwithstanding settlement?',
    'Doctrinal analysis of settlement deed finality clauses against the covenant''s continuing-guarantee character; observed relitigation pressure (freshwater, coastal title) as revealed preference by the parties.',
    'If settlements sunset the relationship, the arrangement trends scaffold-then-piton (administered residual); if consent claims survive settlement, the arrangement remains a live tangled rope with the founding problem permanently open.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_sunset_question, conceptual, 'Whether the settlement architecture imposes a sunset on the partnership function.').

omega_variable(
    suppression_structural_internalized_mix,
    'Of the measured suppression, how much is structural (parliamentary supremacy, legal foreclosure, economic dependency) versus internalized (assimilation-era effects persisting after barrier removal)?',
    'Post-liberalization trajectory analysis: whether rangatiratanga expression expands where legal barriers lifted (broadcasting, language revival, co-governance) or remains suppressed by internalized patterns traceable to schooling and language-suppression policy.',
    'If internalized components are large, the constraint''s effective suppression exceeds the structural measure — targets carry it beyond the arrangement''s formal reach, raising computed χ for identity-locked seats; if small, suppression tracks institutional design and remedies can be institutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_internalized_mix, empirical, 'Structural versus internalized composition of the suppression scalar.').

omega_variable(
    whakapapa_identity_lock_durability,
    'Is hapū identity-lock (exit impossibility via whakapapa and whenua) constitutive of the collectives or contingent on current constitutional non-recognition — would recognized hapū autonomy convert identity_locked into constrained or mobile?',
    'Observation of collectives operating under recognized self-governance (settlement-era post-settlement governance entities, co-governance bodies): does engagement deepen rather than dissolve kinship-based commitment?',
    'Identity-lock amplifies the hapū seats'' derived d toward the full-target end; if the lock is contingent and would loosen under recognition, target-side amplification softens and the seat-level extraction computation falls correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whakapapa_identity_lock_durability, empirical, 'Durability of the identity-lock that fixes hapū exit options.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trea_tr_t0, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(trea_tr_t0, observed).
narrative_ontology:measurement(trea_tr_t25, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement_basis(trea_tr_t25, observed).
narrative_ontology:measurement(trea_tr_t50, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement_basis(trea_tr_t50, observed).
narrative_ontology:measurement(trea_tr_t70, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 70, 0.38).
narrative_ontology:measurement_basis(trea_tr_t70, observed).
narrative_ontology:measurement(trea_tr_t90, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 90, 0.41).
narrative_ontology:measurement_basis(trea_tr_t90, observed).
narrative_ontology:measurement(trea_tr_t110, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 110, 0.39).
narrative_ontology:measurement_basis(trea_tr_t110, observed).
narrative_ontology:measurement(trea_tr_t130, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 130, 0.31).
narrative_ontology:measurement_basis(trea_tr_t130, observed).
narrative_ontology:measurement(trea_tr_t145, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 145, 0.29).
narrative_ontology:measurement_basis(trea_tr_t145, observed).
narrative_ontology:measurement(trea_tr_t160, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 160, 0.37).
narrative_ontology:measurement_basis(trea_tr_t160, observed).
narrative_ontology:measurement(trea_tr_t185, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 185, 0.46).
narrative_ontology:measurement_basis(trea_tr_t185, observed).

% Extraction over time
narrative_ontology:measurement(trea_be_t0, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(trea_be_t0, observed).
narrative_ontology:measurement(trea_be_t25, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement_basis(trea_be_t25, observed).
narrative_ontology:measurement(trea_be_t50, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 50, 0.86).
narrative_ontology:measurement_basis(trea_be_t50, observed).
narrative_ontology:measurement(trea_be_t70, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 70, 0.9).
narrative_ontology:measurement_basis(trea_be_t70, observed).
narrative_ontology:measurement(trea_be_t90, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 90, 0.84).
narrative_ontology:measurement_basis(trea_be_t90, observed).
narrative_ontology:measurement(trea_be_t110, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 110, 0.74).
narrative_ontology:measurement_basis(trea_be_t110, observed).
narrative_ontology:measurement(trea_be_t130, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 130, 0.68).
narrative_ontology:measurement_basis(trea_be_t130, observed).
narrative_ontology:measurement(trea_be_t145, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 145, 0.62).
narrative_ontology:measurement_basis(trea_be_t145, observed).
narrative_ontology:measurement(trea_be_t160, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 160, 0.58).
narrative_ontology:measurement_basis(trea_be_t160, observed).
narrative_ontology:measurement(trea_be_t185, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 185, 0.63).
narrative_ontology:measurement_basis(trea_be_t185, observed).

% Suppression requirement over time
narrative_ontology:measurement(trea_su_t0, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(trea_su_t0, observed).
narrative_ontology:measurement(trea_su_t25, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(trea_su_t25, observed).
narrative_ontology:measurement(trea_su_t50, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 50, 0.86).
narrative_ontology:measurement_basis(trea_su_t50, observed).
narrative_ontology:measurement(trea_su_t70, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 70, 0.74).
narrative_ontology:measurement_basis(trea_su_t70, observed).
narrative_ontology:measurement(trea_su_t90, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 90, 0.62).
narrative_ontology:measurement_basis(trea_su_t90, observed).
narrative_ontology:measurement(trea_su_t110, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 110, 0.55).
narrative_ontology:measurement_basis(trea_su_t110, observed).
narrative_ontology:measurement(trea_su_t130, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 130, 0.52).
narrative_ontology:measurement_basis(trea_su_t130, observed).
narrative_ontology:measurement(trea_su_t145, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 145, 0.48).
narrative_ontology:measurement_basis(trea_su_t145, observed).
narrative_ontology:measurement(trea_su_t160, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 160, 0.52).
narrative_ontology:measurement_basis(trea_su_t160, observed).
narrative_ontology:measurement(trea_su_t185, treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 185, 0.55).
narrative_ontology:measurement_basis(trea_su_t185, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, resource_allocation).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession__retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the Treaty did with sovereignty' decomposes into three structurally distinct constraints — one per reading of the kernel treaty_authority_cession. The ε values differ by construction: crown_cession_reading authors a completed lawful transfer (ε near zero from its seat); this rangatiratanga_retention_reading authors a real covenant with heavily extractive operation (ε 0.63 end-state); retrospective_snare_exposure authors the translation asymmetry itself as the mechanism (family-maximum ε). Influence direction: this reading is upstream of the snare-exposure sibling — its establishment of Māori-text priority is what makes mistranslation legally cognizable as an extraction mechanism — while standing in direct logical contradiction to the crown-cession sibling, since full cession and retained sovereignty cannot both be true of the same signing. Each member links the others via affects_constraints; contamination propagates along the family because adoption of any reading rewrites the victim/beneficiary structure of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
