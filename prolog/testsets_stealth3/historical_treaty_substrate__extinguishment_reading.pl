% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__extinguishment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__extinguishment_reading, []).

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
 *   constraint_id: historical_treaty_substrate__extinguishment_reading
 *   human_readable: Extinguishment Reading of the Historical Treaty Substrate (Completed Property Conveyance)
 *   domain: legal anthropology / indigenous law / comparative constitutional theory
 *
 * SUMMARY:
 *   This story instantiates the extinguishment_reading of the kernel
 *   historical_treaty_substrate: the nineteenth-century land treaties between
 *   Indigenous nations and settler states are read as completed property
 *   conveyances — territorial sovereignty transferred once and for all at
 *   signing, in exchange for defined reserves, annuities, and enumerated
 *   harvesting rights — leaving the settler state as sole legitimate
 *   terrestrial authority. The epsilon referent is the standing arrangement
 *   under contest (the operative treaty settlement and its enforcement as it
 *   exists today), assessed by this reading's own lights: the reading regards
 *   the conveyance as valid and final, yet its own adjudicative practices
 *   (specific-claims processes, breach-of-promise findings) register
 *   delivered-versus-promised shortfalls and imposed administration as
 *   ongoing costs borne by the selling side. The claim and the metrics are
 *   independent authored facts: claimed_type states the structure I judge
 *   true (real coexistence coordination fused with real asymmetric
 *   extraction, actively enforced); the metrics describe observed operation.
 *   Sibling readings (stewardship_reading, nation_to_nation_reading) are
 *   separate constraints over the same substrate with their own epsilon,
 *   beneficiary structures, and classifications; the contest among them is
 *   recorded in omega variables and kernel_context, never folded into this
 *   file's classification.
 *
 * KEY AGENTS:
 *   - settler_state_governments: agenda-setting beneficiary (institutional/arbitrage) — administers the ceded territory, sole legitimate authority under this reading, controls interpretation and amendment of the arrangement
 *   - settler_land_and_resource_industries: primary material beneficiary (powerful/mobile) — occupies and produces from the opened lands under titles and leases issued atop the settlement
 *   - indigenous_treaty_nations: dual-positioned seat (organized/identity_locked) — holders of narrow treaty benefits (reserves, annuities, enumerated rights) who simultaneously bear the costs of extinguished jurisdiction and substituted administration
 *   - metis_scrip_excluded_descendants: structurally affected but conversationally excluded (organized/trapped) — extinguished via scrip or nothing, absent from both benefit rolls and settlement forums
 *   - dominant_juridical_institutions: enforcement interpreter of the fixed texts (institutional/constrained) — adjudicate treaty meaning and anchor title reasoning to the conveyance record
 *   - international_rights_monitoring_bodies: analytical observer (institutional/analytical) — review the arrangement against adopted international standards without enforcement power inside the frame
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, 0.58).
domain_priors:suppression_score(historical_treaty_substrate__extinguishment_reading, 0.66).
domain_priors:theater_ratio(historical_treaty_substrate__extinguishment_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(historical_treaty_substrate__extinguishment_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__extinguishment_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__extinguishment_reading, "Extinguishment Reading of the Historical Treaty Substrate (Completed Property Conveyance)").
narrative_ontology:topic_domain(historical_treaty_substrate__extinguishment_reading, "legal anthropology / indigenous law / comparative constitutional theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__extinguishment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__extinguishment_reading, '086f11f9-7362-4565-b33b-d11e5071b706').
narrative_ontology:cs_kernel_codification('086f11f9-7362-4565-b33b-d11e5071b706', fixed_text).
narrative_ontology:cs_authority_grounding('086f11f9-7362-4565-b33b-d11e5071b706', lineage).
narrative_ontology:cs_interpretation_layer_present('086f11f9-7362-4565-b33b-d11e5071b706').
narrative_ontology:cs_reading_relation('086f11f9-7362-4565-b33b-d11e5071b706', historical_treaty_substrate__stewardship_reading, forecloses).
narrative_ontology:cs_reading_relation('086f11f9-7362-4565-b33b-d11e5071b706', historical_treaty_substrate__nation_to_nation_reading, forecloses).
narrative_ontology:cs_axiom('086f11f9-7362-4565-b33b-d11e5071b706', foundational, signing_extinguishes_prior_territorial_sovereignty).
narrative_ontology:cs_axiom_status(signing_extinguishes_prior_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('086f11f9-7362-4565-b33b-d11e5071b706', signing_extinguishes_prior_territorial_sovereignty, conventional).
narrative_ontology:cs_axiom('086f11f9-7362-4565-b33b-d11e5071b706', secondary, legitimate_territorial_authority_requires_negotiated_transfer).
narrative_ontology:cs_axiom_status(legitimate_territorial_authority_requires_negotiated_transfer, holdable).
narrative_ontology:cs_axiom_grounding('086f11f9-7362-4565-b33b-d11e5071b706', legitimate_territorial_authority_requires_negotiated_transfer, deontological).
narrative_ontology:cs_reference_frame('086f11f9-7362-4565-b33b-d11e5071b706', completed_conveyance_property_settlement).
narrative_ontology:cs_drift_state('086f11f9-7362-4565-b33b-d11e5071b706', post_tsilhqotin_undrip_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('086f11f9-7362-4565-b33b-d11e5071b706', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, settler_land_and_resource_industries).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__extinguishment_reading, metis_scrip_excluded_descendants).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, treaty_conveyance_finality).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__extinguishment_reading, exclusive_crown_terrestrial_jurisdiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the ceded territory under its own laws: issues tenures and licences over lands the treaty texts describe as surrendered, collects taxes and resource royalties, disburses or withholds the scheduled payments, and interprets treaty meaning through its statutes and courts. Its obligations were fixed at historical payment levels; its authority over the territory is not open to renegotiation within this reading. Stepping outside the arrangement would mean disavowing the legal foundation of its own land regime.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, settler_state_governments, beneficiary).

% Farms, logs, mines, ranches, and builds across the lands opened by the settlement. Holds fee-simple titles and resource leases issued on top of the treaty conveyance, paying licence fees and taxes into the state that granted them. Capital and operations can relocate to other jurisdictions if local conditions become unfavorable.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, settler_land_and_resource_industries, beneficiary,
    powerful, biographical, mobile, continental).

% Hold reserve allotments and scheduled annual payments recorded in the treaty texts, and exercise enumerated hunting and fishing rights on defined tracts. Simultaneously live under administrative regimes the state imposed in place of their own governance, with little authority to tax, allocate, or develop lands beyond reserve boundaries, and with payment schedules frozen at their nineteenth-century amounts. Maintain oral chronicles of what was promised at negotiation that diverge from the written instruments. Belonging to the nation and relationship to the home territory constitute communal identity; leaving the arrangement would mean dissolving the people's continuity with its lands rather than changing address.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__extinguishment_reading, indigenous_treaty_nations, payer).

% Descendants of communities offered scrip certificates or nothing during the treaty era, under the same extinguishment logic applied to the treaty nations. Fall outside both the annuity rolls and the land-claim settlements tied to treaty-band membership. Politically organized but holding no seat in the negotiations that produced the settlement or in the forums that maintain it.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, metis_scrip_excluded_descendants, excluded,
    organized, generational, trapped, national).

% Adjudicate what the treaty texts mean: historically required evidence of surrender before acknowledging any continuing Indigenous interest in land, and still anchor most title reasoning to the conveyance record. Occasionally produce decisions that strain the frame — most prominently recognizing title where no prior surrender existed — while remaining institutionally bound to the state whose authority they interpret.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, dominant_juridical_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Review the arrangement against adopted international standards on Indigenous rights, publish assessments and calls for legislative alignment, and document gaps between treaty implementation and those standards. Hold review standing but no enforcement power inside the state's legal order.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__extinguishment_reading, international_rights_monitoring_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__extinguishment_reading, settler_state_governments).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__extinguishment_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of two or more peoples occupying the same territory: fixes reserve boundaries, payment schedules, and harvesting rights in place of open-ended conflict, giving both sides defined, administrable expectations about land and obligation.
% TRANSFER_FUNCTION: Moves territorial jurisdiction and effective control of land and resources from Indigenous nations to the settler state; moves back defined reserve parcels, fixed annual payments, and enumerated harvesting and assistance promises to the Indigenous signatories — in magnitudes that the delivered-versus-recorded record shows were far from symmetric.
% ABSENT_VOICES: Nations that never signed or signed under famine and epidemic pressure would dispute the completeness of the exchange, and their oral chronicles record richer promises than the written texts carry. Scrip-era Metis descendants were extinguished without a treaty seat at all. Women who lost standing through later status rules, and descendants whose communities' verbal commitments never entered the text, are likewise absent from the forums that maintain the settlement.
% DISAPPEARANCE_RATIONALE: If the completed-conveyance arrangement and its enforcement vanished overnight, the settler states' terrestrial title structure would lose the juridical foundation it cites for nearly all derived tenure: fee-simple chains, resource leases, municipal boundaries, and infrastructure corridors on ceded lands would require re-legitimation, and the nations' jurisdictional claims — currently answered by pointing to the closed transaction — would reopen immediately. Land-use planning, resource licensing, and fiscal arrangements across entire regions would reorganize.
% FOUNDING_PROBLEM: Securing durable settler occupation and agricultural and resource development without perpetual frontier war, by converting Indigenous presence into a defined, bounded, administrable form with fixed consideration flowing back.
% FOUNDING_PROBLEM_CORROBORATION: The state and its juridical institutions attest the founding problem was solved by the completed transaction, citing the conveyance record and generations of administered coexistence. Outside the benefiting parties, Indigenous oral chronicles and the record of specific-claims adjudications — which repeatedly found promised terms breached or diminished — attest that the underlying question of how to share the territory remained open; international rights-monitoring bodies independently document the same gap. No single attesting consensus exists across these seats, hence contested.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__extinguishment_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__extinguishment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__extinguishment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__extinguishment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__extinguishment_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__extinguishment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__extinguishment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__extinguishment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58 is moderate-high even by this reading's own accounting: the conveyance is treated as valid, but annuity schedules fixed at nineteenth-century levels, reserve reductions executed without renewed consent, unfulfilled oral promises, and substituted governance register as continuing costs the frame's own courts adjudicate. Suppression 0.66 is a raw structural property (unscaled by power or scope): the completed-transaction premise survives only because alternatives to it are actively closed — courts historically required proof of surrender before recognizing any continuing interest, administrative regimes displaced self-government, and jurisdictional assertions were treated as illegitimate. Theater 0.34 reflects a growing ceremonial layer (commemorations, statements of reconciliation, consultative processes that rarely alter outcomes) over a functional conveyance-and-payment core. Accessibility collapse 0.52: once the premise is accepted, jurisdictional alternatives collapse almost entirely, but the sibling readings remain live in discourse and partial legal openings (settlement agreements that stopped demanding express extinguishment, domestic recognition of title without prior surrender) keep alternatives partly reachable. Resistance 0.62: sustained litigation, political mobilization, blockade-era confrontations, and international advocacy meet the arrangement continuously. The temporal series run on one shared eight-point grid (T=0..70, decade steps across roughly 1951-2021): base_extractiveness ratchets upward through the assimilation-policy endgame and the comprehensive-claims era when new settlements demanded express extinguishment of title, peaks near T=40-50, then recedes modestly as courts recognized title without extinguishment and legislated implementation of international standards began; suppression_requirement is tracked because enforcement capacity genuinely ratcheted and then partially relaxed — an enforcement-capacity dynamic, not merely shifting extraction; theater_ratio climbs monotonically as ceremony substitutes for concession. The trajectory is a ratchet-and-relax arc, not cyclical; no intermittent-reinforcement mechanism is implicated.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. From the settler-state seat the arrangement is a settled conveyance it administers — obligations defined, transaction closed, remaining work merely custodial. From the indigenous-nations seat the same arrangement is experienced as administered dependency: fixed payments that never indexed, a land base sized by others, governance conducted under imposed rules, and oral memory of richer promises than the text carries. From the industries' seat it is an ordinary property regime whose origin question is dormant. From the juridical seat it is authoritative-text interpretation, occasionally strained by its own outputs. The engine computes per-seat types from the structural data; the authored claim does not adjudicate among these experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The state sits nearest the beneficiary end: it collects jurisdiction, taxation, and royalties and controls the interpretive rules — its exit is arbitrage-grade because it wrote the frame and staffs its courts. Industries sit similarly low-d: pure collectors with mobile capital. The nations carry a dual declaration — listed among beneficiaries for the narrow treaty consideration they receive and among victims for the jurisdictional and administrative burdens they bear through the same structure; identity-locked exit pins them toward the target end despite nominal benefit receipt, so the derivation yields elevated effective extraction for this seat. Scrip-era descendants are victims with trapped exit and no conversational seat, amplifying aggregate extraction. The derivation chain from beneficiary/victim declarations plus exit options suffices for every seat; no directionality overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two symmetrical mislabelings. A pure-rope reading would credit the coordination (peace terms, defined obligations, protected land cores, payment schedules that ended open warfare) while erasing the asymmetric consideration, the unindexed obligations, and the suppressed jurisdictional alternatives — the extraction is real and enforced, not coordination overhead. A pure-snare reading would erase the genuine coordination achieved and the real consideration delivered, flattening a functioning-if-unjust settlement into pure cover — the coordination function is real and identifiable. Tangled rope preserves both halves: someone is coordinated (two peoples sharing a territory under defined obligations) and someone pays (the selling side, through extinguished jurisdiction and delivered-less-than-promised terms), with active enforcement holding the whole. On obsolescence: the founding problem (durable terms of coexistence) is contested, not dead — the arrangement persists by enforcement and adjudication rather than inertia alone, so no mandatrophy resolution is declared; the R5 mismatch consumer reads status=contested crossed with verdict=world_rearranges and finds no zombie signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extinguishment_kernel_framing_contest,
    'Is the standing treaty arrangement correctly structured by the extinguishment reading, or does its classification change materially under the stewardship reading (no cession, shared stewardship) or the nation-to-nation reading (ongoing consent between sovereign equals)?',
    'Comparative generation of the sibling stories (historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading) and cross-reading audit of beneficiary/victim sets and epsilon over the identical standing arrangement.',
    'Under either sibling reading, Indigenous nations re-enter as jurisdictional claimants rather than completed sellers, the completed-conveyance premise drops out, epsilon shifts upward, and the coordination function changes from contract administration to ongoing diplomacy — likely altering computed type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extinguishment_kernel_framing_contest, conceptual, 'This constraint is one reading of kernel historical_treaty_substrate; sibling readings instantiate different constraints over the same referent.').

omega_variable(
    oral_promise_incorporation_status,
    'Do the oral promises recorded by treaty commissioners and in Indigenous oral chronicles bind alongside the written instruments, or is the conveyance limited to the written text?',
    'Specific-claims adjudication records and archival comparison of commissioners'' spoken assurances against the signed texts and payment schedules.',
    'Incorporating oral terms widens the promised-versus-delivered gap (medicine chests, farming assistance, hunting ranges, education promises), raising epsilon and strengthening the victim declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_promise_incorporation_status, empirical, 'Whether the consideration side of the completed transaction is larger than the written text records.').

omega_variable(
    consent_validity_under_duress,
    'Do negotiations conducted amid famine, epidemics, and the aftermath of military defeat satisfy the consent conditions required for a completed property conveyance?',
    'Historical reconstruction of conditions at each negotiating round combined with comparative analysis of duress and undue-influence standards in property and contract law applied to sovereign-scale transactions.',
    'If consent fails the reading''s own validity standards, the conveyance premise weakens from within the extinguishment frame itself, pushing epsilon toward a snare-flavored profile and making enforcement the load-bearing element rather than agreed exchange.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_validity_under_duress, conceptual, 'Whether the founding transaction meets validity conditions by the reading''s internal criteria.').

omega_variable(
    consultation_substance_ambiguity,
    'Does the consultative and recognition activity surrounding the arrangement alter project and policy outcomes, or does it perform assent while leaving outcomes intact?',
    'Outcome tracking of consulted projects and policy changes against comparable unconsulted cases, and coding of consultation records for documented accommodation versus procedural completion.',
    'Confirmed performance without accommodation would push theater_ratio above 0.5 and date a piton-side drift for the frame''s ceremonial layer, with the functional conveyance core persisting underneath.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultation_substance_ambiguity, empirical, 'Substance versus performance in the arrangement''s contemporary interactive layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__extinguishment_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__extinguishment_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(hist_tr_t0, observed).
narrative_ontology:measurement(hist_tr_t10, historical_treaty_substrate__extinguishment_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(hist_tr_t10, observed).
narrative_ontology:measurement(hist_tr_t20, historical_treaty_substrate__extinguishment_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(hist_tr_t20, observed).
narrative_ontology:measurement(hist_tr_t30, historical_treaty_substrate__extinguishment_reading, theater_ratio, 30, 0.29).
narrative_ontology:measurement_basis(hist_tr_t30, observed).
narrative_ontology:measurement(hist_tr_t40, historical_treaty_substrate__extinguishment_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(hist_tr_t40, observed).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__extinguishment_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(hist_tr_t50, observed).
narrative_ontology:measurement(hist_tr_t60, historical_treaty_substrate__extinguishment_reading, theater_ratio, 60, 0.34).
narrative_ontology:measurement_basis(hist_tr_t60, observed).
narrative_ontology:measurement(hist_tr_t70, historical_treaty_substrate__extinguishment_reading, theater_ratio, 70, 0.34).
narrative_ontology:measurement_basis(hist_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 0, 0.47).
narrative_ontology:measurement_basis(hist_be_t0, observed).
narrative_ontology:measurement(hist_be_t10, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(hist_be_t10, observed).
narrative_ontology:measurement(hist_be_t20, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(hist_be_t20, observed).
narrative_ontology:measurement(hist_be_t30, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(hist_be_t30, observed).
narrative_ontology:measurement(hist_be_t40, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(hist_be_t40, observed).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 50, 0.63).
narrative_ontology:measurement_basis(hist_be_t50, observed).
narrative_ontology:measurement(hist_be_t60, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 60, 0.61).
narrative_ontology:measurement_basis(hist_be_t60, observed).
narrative_ontology:measurement(hist_be_t70, historical_treaty_substrate__extinguishment_reading, base_extractiveness, 70, 0.58).
narrative_ontology:measurement_basis(hist_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(hist_su_t0, observed).
narrative_ontology:measurement(hist_su_t10, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(hist_su_t10, observed).
narrative_ontology:measurement(hist_su_t20, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement_basis(hist_su_t20, observed).
narrative_ontology:measurement(hist_su_t30, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(hist_su_t30, observed).
narrative_ontology:measurement(hist_su_t40, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement_basis(hist_su_t40, observed).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(hist_su_t50, observed).
narrative_ontology:measurement(hist_su_t60, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 60, 0.69).
narrative_ontology:measurement_basis(hist_su_t60, observed).
narrative_ontology:measurement(hist_su_t70, historical_treaty_substrate__extinguishment_reading, suppression_requirement, 70, 0.66).
narrative_ontology:measurement_basis(hist_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__extinguishment_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__extinguishment_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the historic treaties' covers three structurally distinct constraints over one substrate (epsilon-invariance decomposition). This story authors the extinguishment_reading: completed conveyance, closed consent, state as sole legitimate authority — moderate-high epsilon with a real coordination core. Sibling stories: historical_treaty_substrate__stewardship_reading (no cession; mutual stewardship obligations; different victim set and higher epsilon for the standing arrangement) and historical_treaty_substrate__nation_to_nation_reading (persisting sovereign agreement; ongoing consent required; jurisdictional claimants replace sellers). The extinguishment reading is the frame most domestic adjudication has operated inside, so it functions as the upstream node whose premise conditions the operating environment of both siblings; each sibling links back here via its own affects_constraints edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
