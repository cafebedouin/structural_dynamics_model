% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Doctrine of Encyclopedic Custody
 *   domain: international law/cultural property/post-colonial studies
 *
 * SUMMARY:
 *   This story instantiates the universal-heritage reading of the contested
 *   cultural-property kernel: the claim that artifacts are humanity's shared
 *   inheritance and that legitimate authority over them rests with whichever
 *   institutions best preserve and display them, regardless of where the
 *   objects came from. In practice this reading underwrites the standing
 *   arrangement in which large encyclopedic museums in former imperial
 *   capitals hold vast collections removed during colonial expansion, answer
 *   origin-state restitution petitions from a defensive legal posture, and
 *   justify retention through preservation capacity and universal access. The
 *   reading is sincere for many of its holders — conservation at the major
 *   institutions is genuinely excellent and access is genuinely broad — yet
 *   the same structure transfers custodial authority, prestige, scholarship
 *   priority, and exhibition economics away from origin states and descendant
 *   communities, who bear legal costs, diplomatic friction, and identity harm
 *   in contests that the frame itself labels illegitimate particularism. Per
 *   the epsilon-invariance principle, 'who owns cultural property' decomposes
 *   into three structurally distinct claims (this file, the
 *   sovereign-repatriation reading, and the indigenous-stewardship reading);
 *   each gets its own epsilon, its own beneficiary/victim structure, and its
 *   own classification, linked through the network. The claim/metric gap is
 *   deliberate: the claimed type is stated independently of the metrics,
 *   which describe heavily extractive operation — the engine measures the
 *   divergence rather than the author reconciling it.
 *
 * KEY AGENTS:
 *   - KEY AGENTS (by structural relationship):
 *   - - encyclopedic_museums: agenda-setting beneficiary (institutional / identity_locked) — administer retention, run the conservation and scholarship apparatus, and collect the prestige, research-priority, and exhibition economics the arrangement concentrates
 *   - - claimant_source_states: primary target (organized / trapped) — bear multi-decade legal, diplomatic, and curatorial campaign costs to recover patrimony they cannot purchase, borrow affordably, or abandon
 *   - - origin_descendant_communities: deepest-cost target (powerless / trapped) — sacred, funerary, and ceremonial objects held abroad under display conditions that foreclose ritual use; leverage runs through sympathetic states rather than direct standing
 *   - - metropolitan_museum_publics: incidental beneficiary (organized / mobile) — consume universal access at home while bearing essentially none of the arrangement's costs
 *   - - international_arts_market: opportunistic beneficiary (powerful / arbitrage) — trades antiquities under circulation-legitimizing framings and pre-1970 documentation cutoffs that the reading's legalism stabilizes
 *   - - intergovernmental_restitution_bodies: analytical observer (institutional / analytical) — convene, mediate, and recommend without enforcement power over either side
 *   - - source_country_regional_museums: excluded voice (moderate / trapped) — hold the residual dispersed collections but are priced out of borrowing their own heritage by loan fees, insurance, and courier requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.71).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.76).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Doctrine of Encyclopedic Custody").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international law/cultural property/post-colonial studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, '1fd339bb-3643-4131-8508-eb7a70fd0ec9').
narrative_ontology:cs_kernel_codification('1fd339bb-3643-4131-8508-eb7a70fd0ec9', fixed_text).
narrative_ontology:cs_authority_grounding('1fd339bb-3643-4131-8508-eb7a70fd0ec9', expertise).
narrative_ontology:cs_interpretation_layer_present('1fd339bb-3643-4131-8508-eb7a70fd0ec9').
narrative_ontology:cs_reading_relation('1fd339bb-3643-4131-8508-eb7a70fd0ec9', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('1fd339bb-3643-4131-8508-eb7a70fd0ec9', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('1fd339bb-3643-4131-8508-eb7a70fd0ec9', foundational, heritage_authority_vests_in_humanity_at_large).
narrative_ontology:cs_axiom_status(heritage_authority_vests_in_humanity_at_large, holdable).
narrative_ontology:cs_axiom_grounding('1fd339bb-3643-4131-8508-eb7a70fd0ec9', heritage_authority_vests_in_humanity_at_large, deontological).
narrative_ontology:cs_axiom('1fd339bb-3643-4131-8508-eb7a70fd0ec9', secondary, custody_follows_stewardship_capacity_not_descent).
narrative_ontology:cs_axiom_status(custody_follows_stewardship_capacity_not_descent, holdable).
narrative_ontology:cs_axiom_grounding('1fd339bb-3643-4131-8508-eb7a70fd0ec9', custody_follows_stewardship_capacity_not_descent, instrumental).
narrative_ontology:cs_reference_frame('1fd339bb-3643-4131-8508-eb7a70fd0ec9', cosmopolitan_universal_custody).
narrative_ontology:cs_drift_state('1fd339bb-3643-4131-8508-eb7a70fd0ec9', contemporary_restitution_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('1fd339bb-3643-4131-8508-eb7a70fd0ec9', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, metropolitan_museum_publics).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, international_arts_market).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, claimant_source_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, origin_descendant_communities).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, cultural_internationalism_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, declaration_on_importance_and_value_of_universal_museums_2002).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__universal_heritage_reading, preservation_capacity_supremacy_argument).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold and display large collections acquired during imperial expansion; run conservation laboratories, provenance research programs, and selective loan operations; answer origin-state restitution petitions from a defensive posture governed by trustees and, in several cases, national statutes barring disposal of collections. Signature objects anchor institutional identity and civic branding; returning them is framed internally as breach of the founding charge to hold world culture in trust, while selective returns and long-term partnerships are the adjustments the governance structure can absorb.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums, beneficiary).

% Maintain standing restitution claims for artifacts removed during occupation or colonial administration; fund legal counsel, provenance verification, and permanent diplomatic representation on the issue; negotiate loan-versus-return packages; build replacement museums designed around expected returns. Abandoning a claim is domestically untenable — the patrimony question recurs in elections, school curricula, and national narratives — so the campaign continues regardless of win rate, and the costs recur with it.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, claimant_source_states, payer,
    organized, generational, trapped, national).

% Descendant groups whose ceremonial, funerary, or sacred objects sit in foreign vitrines, many having learned the full extent of holdings only through digitization projects. Access requires international travel, visas, and institutional permission; ritual handling is generally impossible under conservation-display conditions. Their leverage runs through sympathetic states, NGO campaigns, and museum-sector allies rather than any direct seat in retention governance.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, origin_descendant_communities, payer,
    powerless, generational, trapped, regional).

% Visit encyclopedic collections free or cheaply in London, Paris, Berlin, and New York, encountering world cultures without travel or border friction. Returns under any realistic settlement would thin small shares of large galleries and expand loan circuits; these publics would retain nearly all access while bearing none of the campaign costs, and polling shows tolerance for returns varies with economic mood rather than principled attachment.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, metropolitan_museum_publics, beneficiary,
    organized, biographical, mobile, continental).

% Dealers, auction houses, and private collectors trading antiquities and ethnographic material under framings that legitimize circulation irrespective of origin. Pre-1970 documentation cutoffs define lawful title, and the universalist language that artifacts transcend borders stabilizes the criteria on which the trade's legitimacy cases rest. Exposure arrives when restitution norms tighten documentation requirements or provenance due diligence becomes mandatory.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_arts_market, beneficiary,
    powerful, immediate, arbitrage, global).

% UNESCO's intergovernmental committee and allied bodies convene claimant and holding parties, publish mediation sessions, and maintain ethical codes for the sector. They can recommend, facilitate, and shame but cannot compel transfers; effectiveness depends entirely on both sides' continued willingness to keep the process alive, which gives them a discursive seat and no custodial one.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, intergovernmental_restitution_bodies, observer,
    institutional, generational, analytical, global).

% National and regional museums in origin countries hold the residual portions of dispersed collections and are the natural receiving institutions for any return. They face loan fees, insurance valuations, and courier requirements that price them out of temporarily exhibiting their own heritage abroad-held counterparts, and they enter retention debates mainly through state ministries rather than museum-governance networks, since the international museum-professional bodies that set sector norms meet in the metropolitan capitals.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, source_country_regional_museums, excluded,
    moderate, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, encyclopedic_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates conservation science, climate control, security, and cataloguing for millions of fragile objects at well-funded institutions; provides single-site comparative study across traditions that dispersed custody cannot assemble; standardizes documentation and loan logistics for objects whose origin regions now span dozens of jurisdictions with uneven custodial infrastructure.
% TRANSFER_FUNCTION: Moves custodial authority over culturally originating artifacts — and the prestige, research priority, exhibition revenue, and urban footfall attached to them — from origin states and descendant communities to institutions in a handful of former imperial capitals; moves claimant states' budgets into decades-long legal and diplomatic recovery campaigns whose outputs are usually loans rather than returns.
% ABSENT_VOICES: Origin-descendant communities and source-country regional museums are absent from retention governance: the 2002 universal-museum declaration was issued by eighteen directors of large Euro-American museums with no claimant-state consultation; descendant communities of taken sacred objects had no seat when retention norms crystallized; regional museums in origin countries are priced out of the professional forums where loan and custody standards are set. Where they are: outside trustee councils and declaration processes, petitioning from request seats rather than governing seats.
% DISAPPEARANCE_RATIONALE: If the retention arrangement and its justifying frame vanished overnight, restitution petitions would convert into scheduled transfers, source-state museums would receive flagship objects within planning cycles, metropolitan galleries would reorganize around loan circuits and digital surrogacy, comparative scholarship would decentralize to distributed study collections, cultural tourism would partially redistribute toward origin capitals, and the art market's lawful-title criteria would tighten sharply — the physical objects survive, but the rents and authority the arrangement concentrates would relocate.
% FOUNDING_PROBLEM: Artifacts removed between the late eighteenth and twentieth centuries left regions lacking museums, climate control, or secure storage amid conquest and instability; the frame answered the problem of keeping such objects intact and viewable by concentrating them where preservation capacity existed, on the theory that capacity, not geography, should determine custody.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: Greek, Egyptian, and Nigerian cultural ministries attest that the custodial-capacity gap has largely closed (the New Acropolis Museum, the Grand Egyptian Museum, and the planned Edo Museum of West African Art exist precisely to receive returns); UNESCO committee records document the narrowing asymmetry across decades of session proceedings; conservation literature on transferred objects (the Axum obelisk reassembly, post-transfer Benin bronze monitoring) tests the capacity claim empirically. The holding institutions alone continue to attest the problem as live, citing active-conflict source regions and funding gaps — no external party corroborates that version at general scope.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.71) because the standing arrangement moves custodial authority and its attached rents from origin nations to a handful of holding institutions while claimant states finance recovery campaigns whose success rate is historically marginal; the genuine services rendered (conservation, cataloguing, free access) partially offset but do not approach parity with what is transferred. Suppression is higher than extraction (0.76) and is a raw structural input, not scaled by power or scope — only extractiveness is scaled in the engine's computation. Its mechanism is predominantly structural (roughly 80 percent): national anti-disposal statutes such as the British Museum Act's prohibition on deaccession, charity-law fiduciary constraints on trustees, treaty-era documentation cutoffs deployed defensively, and diplomatic asymmetry between claimant ministries and metropolitan institutions. A smaller internalized component (roughly 20 percent) appears where claimant-side elites absorb the frame themselves — origin states applying identical universal-access rhetoric to their own minority patrimony — which the positional_frame_deployability omega tracks. Theater ratio (0.34) reflects a real functional core (conservation labs, provenance research, free entry) wrapped in growing performance: universal-museum declarations issued without consulting claimants, loan diplomacy marketed as magnanimity, and 'shared heritage' language that changes custody of nothing. Accessibility collapse is moderate (0.55): the alternative path — return — demonstrably exists (Italian returns, Benin bronze transfers, Dutch policy revision) but remains exceptional, negotiated case by case rather than available as a standing option. Resistance (0.62) is persistent and institutionalizing: standing state campaigns, UNESCO mediation machinery, the post-2018 restitution-report wave, and dissent inside the museum sector itself. Temporal series run on one shared decade grid (all three metrics at all eight points, t0=1954 Hague codification through t70) so the engine samples a complete matrix: extraction and suppression climb together as the legal machinery hardened through the 1970 convention's defensive use and the 2002 universal-museum declaration, with the post-2018 period showing contested loosening rather than reversal — a monotonic hardening profile, not an oscillation, so no cyclical reinforcement mechanism is implicated. Same-power divergence: claimant states share a nominal power atom (organized states) yet differ sharply in constraint-specific leverage — an EU-member claimant can mobilize bloc diplomacy and domestic museum-ready infrastructure, a hydro-strategic claimant can trade water and migration cooperation for cultural concessions, while a conflict-fragile claimant can neither embargo loans nor threaten market closure, so identical formal standing yields different exit horizons. Inter-institutionally, the holding museums, origin-state ministries, and intergovernmental bodies occupy the same legal arena with different authority sources: statute-backed trusteeship, ministerial mandate, and convening legitimacy respectively — which is why the observer seat can shape discourse but never custody.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the agenda-setting seat the arrangement reads as a service civilization performs for itself: the museum experiences its own position as trusteeship, burden-bearing, and generosity under attack — and the identity-lock is institutional rather than merely financial; the organization's self-concept has fused with holding-world-culture-in-trust, so returning a signature object registers internally as mission dissolution rather than asset transfer, and the classification from that seat carries an inertial, self-justifying character that would break quickly if the trusteeship frame were publicly renegotiated into a custodian-until-return frame. From the trapped payer seats the identical structure reads as cover over dispossession: the same declaration that the trustee seat experiences as philosophy, the claimant seat experiences as the refusal letterhead. Metropolitan publics compute near-benign (benefit without payment), the arts market computes as frame-leveraging arbitrage, and the excluded regional-museum seat experiences a conversation it is priced out of joining. The engine derives these divergent per-seat classifications from the structural data; this commentary explains why they must diverge rather than adjudicating among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Encyclopedic museums sit nearest the beneficiary pole (d low): they collect the concentrated rents and control the rules, with identity-locked exit pushing them to hold position regardless. Claimant states and descendant communities sit near the full-target pole (d high): trapped agents whose demand channel the frame itself delegitimizes sit nearer the target end than mobile ones would. Metropolitan publics derive modest beneficiary directionality — real access gains, negligible costs. The arts market derives beneficiary directionality through the frame's stabilization of lawful-title criteria. The observer seat is approximately symmetric by construction. Scope amplification applies at the global scale the arrangement operates on: verification of provenance claims and conservation equivalence across jurisdictions is hard, which scales effective extraction upward for the trapped targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification work here is preventing mislabeling in both directions. Reading the arrangement as pure coordination (rope) — the frame's self-description — would erase the asymmetric extraction that funds and motivates retention, and would direct remedies at access subsidies rather than authority transfer. Reading it as pure extraction (snare) — the polemic counter-frame — would erase the genuine conservation and access function, inviting remedies (wholesale dispersal) that damage the objects the arrangement claims to serve. Tangled rope holds both truths: a real coordination function and real extraction through the same structure, requiring active enforcement to hold. On mandate obsolescence: the founding problem (fragile local custodianship) has substantially narrowed — origin states now operate first-rank facilities — but is not uniformly dead, since active-conflict source regions remain genuinely risky; hence founding_problem_status is contested rather than dead, and mandatrophy is unresolved. The telling structural absence: the arrangement carries no sunset clause and no transition machinery — a scaffold-shaped remedy (structured return schedules with conservation benchmarks) has no hook in the current legal corpus, which is itself diagnostic of how far the arrangement has drifted from anything transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This file instantiates only the universal_heritage_reading of the cultural_property_legal_corpus kernel. Would the standing retention arrangement measure differently under the sovereign_repatriation_reading or the indigenous_stewardship_reading, and where exactly do the readings disagree?',
    'Author the two sibling stories over the same referent (the standing retention arrangement) and diff epsilon, beneficiary/victim sets, and computed per-seat classifications. The disagreement is located in the locus of legitimate custodial authority: humanity-at-large (this reading) versus successor states claiming historical continuity versus descent communities maintaining cultural continuity.',
    'Under the sibling readings the same physical arrangement computes with higher effective extraction and expanded victim sets, and holding institutions shift from coordination providers toward pure collectors; the classification of THIS story holds only within its own reading''s premises, which is why the readings are separate files linked through the network rather than averaged into one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of a three-reading kernel; siblings are separate constraints over a shared referent.').

omega_variable(
    preservation_capacity_asymmetry_liveness,
    'Does the founding preservation-capacity asymmetry (the reason concentration was justified) still bind anywhere it matters, or has it lapsed now that origin states operate major conservation infrastructure?',
    'Comparative conservation-outcome audits of returned and transferred objects (Axum obelisk reassembly, Benin bronze condition monitoring after Nigerian transfers, New Acropolis Museum and Grand Egyptian Museum facility performance) benchmarked against holding-institution baselines, plus conflict-zone risk assessment for remaining source regions.',
    'If the asymmetry has broadly lapsed, the arrangement''s coordination function collapses toward cover and classification drifts toward pure extraction; if it remains live for a defined subset (active-conflict source regions), a residual genuine-coordination component survives and remedies should distinguish the subsets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_capacity_asymmetry_liveness, empirical, 'Whether the founding justification for concentrated custody is still empirically operative.').

omega_variable(
    trustee_identity_fusion,
    'Is retention at the holding institutions driven by material position (prestige economies, scholarship monopoly, exhibition revenue) or by internalized institutional identity (the self-concept of trusteeship-for-humanity into which the organizations have fused)?',
    'Behavioral contrast: observe transfer decisions where reputational credit and material incentive are stripped out (anonymous unconditional returns, quiet deaccessions) versus negotiated transfers that maximize announced partnership value. Persistent refusal under zero-cost conditions indicates identity-driven maintenance.',
    'If identity-driven, enforcement pressure persists even after statutes and treaties are amended — an internalized component that legal remedies alone will not release; classification of the agenda-setting seat then carries an inertial quality that pure interest accounts miss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trustee_identity_fusion, empirical, 'Material versus internalized driver of retention behavior at the agenda-setting seat.').

omega_variable(
    access_maximization_claim_validity,
    'Does concentrated foreign custody actually maximize global access, as this reading''s operative criterion requires, or would distributed custody with digital surrogacy and structured circulation raise aggregate reach?',
    'Access-modeling studies comparing visitor reach, scholarly throughput, and origin-community access under the current arrangement versus counterfactual return-plus-circulation regimes; use natural experiments from completed partial returns and long-term loan agreements.',
    'If retention fails its own maximization criterion, this reading''s foundational warrant weakens and the vindicated propositions lose their evidentiary floor, exposing the arrangement as retention justified by inherited position rather than performed function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_maximization_claim_validity, conceptual, 'Whether the reading''s own operational axiom (capacity-maximizing custody) is satisfied by the standing arrangement.').

omega_variable(
    positional_frame_deployability,
    'Is the universal-heritage justification content-neutral authorization of possession — available to whoever holds objects — rather than a principled civilizational commitment?',
    'Survey intra-national retention practice: origin states applying identical universal-access rhetoric to their own minority and regional patrimony (Egyptian custody of Nubian materials, Ethiopian center-region custody of peripheral heritage, state custody of indigenous collections in post-colonial states), and compare the justificatory language to the metropolitan corpus.',
    'Confirmation that the frame travels with possession rather than with principle would indicate the extraction is positional, strengthening the reading that the arrangement''s persistence depends on who currently holds rather than on any invariant good — and tightening the boundary between this reading and its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positional_frame_deployability, conceptual, 'Whether the universalist justification is principle or positional cover, tested by its deployment by non-metropolitan holders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(cult_tr_t0, observed).
narrative_ontology:measurement(cult_tr_t10, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(cult_tr_t10, observed).
narrative_ontology:measurement(cult_tr_t20, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(cult_tr_t20, observed).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(cult_tr_t30, observed).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(cult_tr_t40, observed).
narrative_ontology:measurement(cult_tr_t50, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 50, 0.29).
narrative_ontology:measurement_basis(cult_tr_t50, observed).
narrative_ontology:measurement(cult_tr_t60, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 60, 0.32).
narrative_ontology:measurement_basis(cult_tr_t60, observed).
narrative_ontology:measurement(cult_tr_t70, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 70, 0.34).
narrative_ontology:measurement_basis(cult_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cult_be_t0, observed).
narrative_ontology:measurement(cult_be_t10, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 10, 0.49).
narrative_ontology:measurement_basis(cult_be_t10, observed).
narrative_ontology:measurement(cult_be_t20, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement_basis(cult_be_t20, observed).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(cult_be_t30, observed).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(cult_be_t40, observed).
narrative_ontology:measurement(cult_be_t50, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 50, 0.65).
narrative_ontology:measurement_basis(cult_be_t50, observed).
narrative_ontology:measurement(cult_be_t60, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 60, 0.69).
narrative_ontology:measurement_basis(cult_be_t60, observed).
narrative_ontology:measurement(cult_be_t70, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 70, 0.71).
narrative_ontology:measurement_basis(cult_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(cult_su_t0, observed).
narrative_ontology:measurement(cult_su_t10, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(cult_su_t10, observed).
narrative_ontology:measurement(cult_su_t20, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(cult_su_t20, observed).
narrative_ontology:measurement(cult_su_t30, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement_basis(cult_su_t30, observed).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement_basis(cult_su_t40, observed).
narrative_ontology:measurement(cult_su_t50, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 50, 0.69).
narrative_ontology:measurement_basis(cult_su_t50, observed).
narrative_ontology:measurement(cult_su_t60, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement_basis(cult_su_t60, observed).
narrative_ontology:measurement(cult_su_t70, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 70, 0.76).
narrative_ontology:measurement_basis(cult_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who owns cultural property' covers three structurally distinct claims with different epsilon, different victim sets, and different failure modes. This file instantiates the universal_heritage_reading (authority vests in capacity-maximizing institutions; claimant demands read as particularist threats to a public good; extraction lands on claimant states as legal-diplomatic cost and identity harm). The sovereign_repatriation_reading instantiates state-sovereignty continuity (colonial acquisition as illegitimate extraction; holding institutions as unauthorized possessors). The indigenous_stewardship_reading instantiates communal-sacred continuity (descent communities as legitimate authority; both metropolitan institutions and successor states as misrecognizing holders). The upstream reading in citation flow is this one — the universal-heritage frame is the incumbents' warrant and is cited against the downstream readings — so contamination propagates from this file toward its siblings when this reading's access-maximization warrant degrades.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
