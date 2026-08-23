% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

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
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate — Stewardship Reading (Relational Pact, No Cession)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   The standing arrangement under contest is the historical treaty regime as
 *   actually administered by settler states: treaties signed with Indigenous
 *   nations continue to structure the territorial order, but the state side
 *   of the relationship is operated as unilateral jurisdiction and resource
 *   take-off, with consultation processes layered on top. This file authors
 *   that standing arrangement from the stewardship reading's seat only — the
 *   reading that holds the treaties as relational pacts for shared
 *   territorial stewardship, with no cession of sovereignty and mutual
 *   obligations for coexistence. Per the epsilon-referent rule,
 *   extractiveness is assessed against the standing arrangement (never
 *   against the joint-management arrangement this reading would put in
 *   place), by the reading's own lights: measured against its own
 *   mutual-obligation standard, the standing arrangement extracts heavily.
 *   The claim/metric gap is deliberate: the constraint is CLAIMED as
 *   tangled_rope (genuine covenant framework operated extractively) while the
 *   metrics describe the arrangement's actual operation — the engine measures
 *   the divergence; do not reconcile the claim to the metrics. Family
 *   decomposition: the colloquial label 'the historical treaties' covers
 *   three structurally distinct constraints. The extinguishment reading
 *   authors low epsilon (transaction complete, obligations discharged); the
 *   nation_to_nation reading authors moderate epsilon (ongoing consent
 *   violations in an interstate frame); this stewardship reading authors high
 *   epsilon (state-side obligations wholesale unperformed). Same referent,
 *   reading-indexed values; the siblings are separate files linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - signatory_indigenous_nations: primary target (organized/trapped) — bears the extraction, holds the oral covenant, presses through courts, negotiation tables, and land defense
 *   - settler_state_governments: agenda setter and principal beneficiary (institutional/arbitrage) — administers interpretation, issues tenures, collects royalties, enforces jurisdiction
 *   - settler_resource_industries: secondary beneficiary (powerful/mobile) — works state-granted tenures on treaty territories; capital exits locally, never from the order itself
 *   - settler_municipalities_and_public: diffuse beneficiary (organized/constrained) — inhabits the property, energy, and food order the substrate underwrites; bears taxes and disruption
 *   - hereditary_womens_councils: excluded voice (moderate/constrained) — hold hereditary and oral authority; absent from the original signings and from modern negotiation tables
 *   - international_human_rights_bodies: analytical observer (institutional/analytical) — review compliance from outside the domestic order; no enforcement power inside it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.8).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.75).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate — Stewardship Reading (Relational Pact, No Cession)").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, 'b20db17f-64a0-4bd7-8ed7-de8ef2b58801').
narrative_ontology:cs_kernel_codification('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', fixed_text).
narrative_ontology:cs_authority_grounding('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', lineage).
narrative_ontology:cs_interpretation_layer_present('b20db17f-64a0-4bd7-8ed7-de8ef2b58801').
narrative_ontology:cs_reading_relation('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', foundational, inherent_sovereignty_inalienable).
narrative_ontology:cs_axiom_status(inherent_sovereignty_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', inherent_sovereignty_inalienable, deontological).
narrative_ontology:cs_axiom('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', foundational, mutual_stewardship_obligation).
narrative_ontology:cs_axiom_status(mutual_stewardship_obligation, holdable).
narrative_ontology:cs_axiom_grounding('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', mutual_stewardship_obligation, conventional).
narrative_ontology:cs_reference_frame('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', living_relational_covenant).
narrative_ontology:cs_drift_state('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', contemporary_administrative_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('b20db17f-64a0-4bd7-8ed7-de8ef2b58801', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state_governments).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_resource_industries).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_municipalities_and_public).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, signatory_indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, signatory_indigenous_nations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the treaties as living relational pacts transmitted orally alongside the written texts, and still perform their side: aid, sharing, peace-keeping. Their territories sit inside the administered arrangement — resources leave under state-issued tenures, jurisdictional decisions are made in distant capitals, and the reciprocal obligations owed them are largely unperformed. They receive fixed nominal annuities and defined reserve parcels, returns fixed at signing and eroded since. Exit is unavailable: the land is the nation's law, identity, and economy, and leaving it dissolves the nation. They press the relationship through courts, negotiation tables, and physical land defense.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, signatory_indigenous_nations, payer,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, signatory_indigenous_nations, beneficiary).

% Administer the treaty regime: interpret the texts in their own courts and cabinets, issue resource tenures on treaty territories, set the terms of consultation, enforce jurisdiction through police and injunctions, and collect royalties and taxes from extraction. When pressed, they restructure their own exposure — downloading obligations to provinces, commissioning inquiries, adjusting consultation templates — while keeping the underlying revenue and jurisdiction flows intact. They cannot abandon the substrate without dissolving the property and constitutional order built on it, so they manage it indefinitely.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, settler_state_governments, beneficiary).

% Operate forestry, mining, hydro, and agricultural enterprises under state-granted tenures on treaty territories. Pay royalties to the state treasury rather than to the nations whose territories they work. Capital relocates to the next viable deposit or jurisdiction when local conditions sour; impact-benefit agreements with nations are entered when projects require social license, not as standing obligation. Their exposure to the arrangement is project-scoped and temporary even though their aggregate dependence on it is permanent.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_resource_industries, beneficiary,
    powerful, biographical, mobile, global).

% Inhabit towns, cities, farms, and property regimes built on the treaty substrate. Receive cheap energy, timber, food, and water from treaty-territory extraction, and the security of land title that is never contested at the root. Bear diffuse costs: taxes that fund litigation and settlement payments, and periodic disruption when land defense blocks rail lines and roads. Cannot relocate away from the constitutional order the treaties underwrite; their stake is inherited rather than chosen.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_municipalities_and_public, beneficiary,
    organized, biographical, constrained, regional).

% Hold hereditary and clan-based authority in several signatory nations but were absent from the original treaty signings and remain marginal at modern negotiation tables structured around elected-band councils. Carry oral knowledge of the original commitments and the conditions attached to them. Would condition any settlement on restoring their seat in the covenant's interpretation. They are embedded in the nations and in the territory; there is no exit from the relationship for them, only exclusion from its administration.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, hereditary_womens_councils, excluded,
    moderate, generational, constrained, regional).

% Review state conduct against treaty-implementation and land-rights standards through periodic reporting, country visits, and issued findings on consultation adequacy and free, prior, and informed consent. Take testimony from nations and state delegations alike. Exert reputational and legal-normative pressure from outside the domestic order; hold no enforcement power inside it.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, international_human_rights_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__stewardship_reading, settler_state_governments).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the standing framework within which Indigenous nations and the settler polity share one territory: it fixes whose law applies where, channels diplomatic relations, defines mutual aid and peace obligations, and substitutes a negotiated coexistence order for open conflict over land.
% TRANSFER_FUNCTION: Moves territorial wealth — timber, minerals, water, hydro power, agricultural output — and jurisdictional authority from the nations' stewardship into state treasury and licensed industry control, returning fixed nominal annuities and defined reserve parcels to the nations.
% ABSENT_VOICES: Hereditary women's councils and the nations' own knowledge keepers are absent from the interpretation seats: treaty meaning is adjudicated in state courts and cabinets, and modern negotiation tables are built around elected-band structures. Future generations of the signatory nations — the parties to whom the duration promises were addressed — hold no seat in current implementation decisions.
% DISAPPEARANCE_RATIONALE: Land title regimes, provincial and state boundaries, resource tenures, and the constitutional division of powers all presuppose the treaty substrate. Overnight removal would reopen every land question at once, void the property settlement built on it, and force wholesale renegotiation of the territorial order.
% FOUNDING_PROBLEM: After generations of alliance, trade, war, and epidemic, two peoples occupying one territory needed a durable framework that would end open conflict over land: the treaties were made to fix the terms of sharing territory and living side by side.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: treaty commissioners' own dispatches and payment ledgers, missionary and trading-company records, the Royal Commission on Aboriginal Peoples, and independent historians and legal scholars all attest the founding coexistence problem; the state's own archived negotiating instructions confirm that sharing terms were offered and understood at signing. What the parties dispute is the resolution, not the problem: the state attests it was closed by cession, the nations attest it remains open as ongoing obligation. No archival source outside the nations independently establishes that the sharing terms were withdrawn or repudiated at signing.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80 at interval end) because the standing arrangement transfers territorial wealth and jurisdictional authority to the state and licensed industry while returning fixed nominal annuities and defined reserve parcels — a return fixed in the nineteenth century and eroded by inflation, against flows that scale with commodity markets. Suppression is high (0.75) because persistence depends on active enforcement: police intervention at land defenses, injunctions against land defenders, administrative control of reserve governance, and the residue of the assimilation machinery. Theater is moderate-high (0.55): consultation frameworks, environmental assessment processes, and reconciliation events perform shared governance without transferring decision power, though real transfers (settlement agreements, co-management boards) occur at the margin. Accessibility collapse is moderate (0.60): full-jurisdiction alternatives are foreclosed by enforcement, but modern treaties, self-government agreements, and court-recognized rights keep partial exits alive. Resistance is high (0.70): blockades, litigation, nation rebuilding, and international advocacy are sustained, not episodic. The suppression series is deliberately cyclical rather than monotonic: enforcement rose to an assimilation-era peak (pass system, ceremony bans, residential schools), relaxed during the liberalization decades, then re-intensified as each new extraction frontier opened (hydro, forestry, pipeline corridors). The oscillation tracks external commodity-frontier expansion — a side effect of extraction economics rather than an intermittent-reinforcement mechanism in itself — though each liberalization phase functions as a release valve that legitimizes the next round of expansion. All three metric series run on one shared time grid (t = 0, 25, 50, 75, 100, 125, 150) so every metric is authored at every examined time point; the terminal values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the payer seat (signatory nations), the arrangement is experienced as a breached covenant: obligations they still perform, reciprocated with extraction and consultation theater — a snare-flavored experience of a framework they affirm. From the agenda-setter seat (state governments), the same structure is managed domestic policy: a settled jurisdictional order with a consultation overhead and episodic negotiation costs — a rope-flavored experience. From the beneficiary industry seat, it is tenure security and regulatory predictability. The excluded hereditary-authority seat experiences a second-order exclusion: locked out of the interpretation seats even within the nations' own side. The observer seat sees a compliance gap between stated reconciliation commitments and audited flows. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map to real flows: the state treasury collects royalties, taxes, and jurisdictional control (principal capturer, d near the beneficiary end); licensed industry receives tenures and access while paying royalties to the state, not the nations (mobile capital, arbitrage-grade exit, d nearest the beneficiary end); the settler public receives cheap energy, timber, food, and uncontested title (incidental, diffuse benefit). The signatory nations bear the extraction with effectively no exit — the land is their law, identity, and economy — placing them near the full-target end, offset slightly from 1.0 by the annuity and reserve-parcel receipts they still collect (hence secondary_role beneficiary on the stakeholder surface, without entry in the beneficiaries array, which drives derivation on net position). One directionality override is authored: settler_municipalities_and_public at d = 0.30, because the structural derivation from their declared beneficiary status would undershoot the real costs they carry (taxes funding litigation and settlements, disruption from land defense, legitimacy costs) — their benefit is real but incidental, sitting nearer symmetric than the raw declaration implies. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   This is not a mandatrophy case of an outlived mandate: the founding problem — a durable framework for two peoples sharing one territory — is live, and the parties dispute only whether the treaties still carry it. The risk is the inverse of obsolescence: a live mandate whose function has been captured by one party. The classification guards both symmetrical errors. Reading the extraction record alone would mislabel a genuine covenant framework as a snare, erasing the coordination function the stewardship reading affirms and the nations themselves insist on. Reading the covenant language alone would excuse the unilateral extraction as coordination cost — the rope error that lets the state's non-performance pass as the price of coexistence. Tangled rope holds both truths at once: the same structure that coordinates coexistence extracts asymmetrically because one party stopped performing its obligations while retaining the framework's full benefits. If the state's side were restored (consent, shared governance, joint management), the same substrate would recompute toward rope; if the coordination story collapsed entirely (treaties repudiated as fraud), it would recompute toward snare. The current structure sits between, and the temporal series shows which way it is drifting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the historical_treaty_substrate kernel governs the arrangement — extinguishment (completed cession), nation_to_nation (sovereign-equals internationalism), or stewardship (relational pact with no cession)? This story instantiates the stewardship reading only.',
    'Constitutional adjudication, comprehensive-claim renegotiation outcomes, or a ratified national framework that fixes the kernel''s operative reading; until then the readings compete across courts, negotiation tables, and public law.',
    'Extinguishment would remove the nations from any beneficiary position (residual reserve property, no jurisdictional claim) and collapse epsilon toward a settled-transaction profile; nation_to_nation would restore consent rights but recast obligations as interstate diplomacy; stewardship keeps nations positioned to benefit from territorial jurisdiction and places the state under consent and shared-governance obligations, with territorial resources jointly managed rather than unilaterally extracted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: which reading governs the treaty substrate.').

omega_variable(
    oral_written_kernel_authority,
    'Does authoritative kernel meaning reside in the written treaty texts (as state courts treat them) or in the parallel oral commitments the nations transmit (as the stewardship reading requires)?',
    'Comparative adjudication that admits oral tradition as evidence, plus archival reconstruction of the commissioners'' spoken promises against the written text.',
    'If oral commitments are authoritative, the no-cession and mutual-obligation premises harden and the standing arrangement''s epsilon rises further; if the written text alone governs, the stewardship reading loses its evidentiary anchor and the contest shifts toward extinguishment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oral_written_kernel_authority, conceptual, 'Where authoritative treaty meaning lives: written text versus oral covenant.').

omega_variable(
    resource_flow_attribution,
    'What share of the value extracted annually from treaty territories accrues to the state treasury, to licensed industry, and to the nations?',
    'Public accounts, royalty registries, and impact-benefit-agreement disclosures permit a fiscal audit of flows by seat.',
    'A concentrated state-industry capture pattern confirms the receipt structure authored here; material nation-side flows approaching parity would push the arrangement toward genuine joint management and lower effective extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resource_flow_attribution, empirical, 'Fiscal attribution of extracted territorial value across seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__stewardship_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__stewardship_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__stewardship_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__stewardship_reading, theater_ratio, 100, 0.42).
narrative_ontology:measurement(hist_tr_t125, historical_treaty_substrate__stewardship_reading, theater_ratio, 125, 0.52).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__stewardship_reading, theater_ratio, 150, 0.55).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__stewardship_reading, base_extractiveness, 25, 0.45).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__stewardship_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__stewardship_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__stewardship_reading, base_extractiveness, 100, 0.74).
narrative_ontology:measurement(hist_be_t125, historical_treaty_substrate__stewardship_reading, base_extractiveness, 125, 0.78).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__stewardship_reading, base_extractiveness, 150, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__stewardship_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__stewardship_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__stewardship_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__stewardship_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(hist_su_t125, historical_treaty_substrate__stewardship_reading, suppression_requirement, 125, 0.66).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__stewardship_reading, suppression_requirement, 150, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, resource_allocation).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: 'the historical treaties' is one colloquial label over three structurally distinct constraints. Extinguishment reads the transaction as complete (low epsilon, obligations discharged, nations reduced to residual property holders); nation_to_nation reads it as interstate and ongoing (moderate epsilon, consent violations in a sovereign-equals frame); stewardship reads it as a relational covenant whose state-side obligations are unperformed (high epsilon, nations retain territorial jurisdiction as a matter of the pact's own terms). Same referent — the standing treaty regime — reading-indexed values. Citation pressure runs from extinguishment (the state's operational default, cited as evidence that the questions are settled) toward the other two readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__stewardship_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
