% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__sovereign_repatriation_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__sovereign_repatriation_reading
 *   human_readable: Sovereign Repatriation Regime for Colonial-Era Cultural Property
 *   domain: legal/international/post-colonial
 *
 * SUMMARY:
 *   A body of international law and diplomatic practice — anchored in the
 *   1970 UNESCO Convention, the 1995 UNIDROIT Convention, and a growing web
 *   of bilateral memoranda — treats colonial-era removals of cultural objects
 *   as conferring no valid title and vests legitimate authority over
 *   expropriated patrimony in successor states claiming historical continuity
 *   with the dispossessed polities. Under this arrangement, encyclopedic
 *   museums in former imperial centers face structured restitution claims;
 *   origin-state governments prosecute those claims through treaty bodies,
 *   courts, and diplomacy; and returned objects pass into national custody.
 *   The arrangement solves a real coordination problem (determinate title
 *   rules, a channel for claims, deterrence of illicit trafficking) while
 *   imposing asymmetric costs: holding institutions divest and defend, and
 *   the specific claims of communities inside successor states are mediated —
 *   often overridden — by the state's title. KEY AGENTS (by structural
 *   relationship): - successor_state_governments: Primary beneficiary
 *   (institutional/arbitrage) — collects custody, symbolic capital, and
 *   diplomatic leverage - universal_museums: Primary target
 *   (institutional/constrained) — bears divestment, litigation, and
 *   reputational costs - substate_origin_communities: Secondary target
 *   (powerless/identity_locked) — claims subordinated to state title -
 *   origin_state_national_museums: Secondary beneficiary
 *   (organized/constrained) — receives returned holdings -
 *   unesco_intergovernmental_committee: Procedural administrator
 *   (institutional/constrained) - colonial_diaspora_descendants: Secondary
 *   target (powerless/identity_locked) - private_collectors_and_dealers:
 *   Excluded market actors (powerful/arbitrage) -
 *   cultural_property_legal_scholars: Analytical observer
 *   (analytical/analytical). Epsilon's referent is the standing
 *   state-centered restitution arrangement itself, assessed by this reading's
 *   own lights — never the arrangement a rival framing of cultural property
 *   would install.
 *
 * KEY AGENTS:
 *   - successor_state_governments: primary beneficiary (institutional/arbitrage) — collects custody and symbolic capital
 *   - universal_museums: primary target (institutional/constrained) — bears divestment and defense costs
 *   - substate_origin_communities: secondary target (powerless/identity_locked) — claims subordinated to state title
 *   - origin_state_national_museums: secondary beneficiary (organized/constrained) — receives returned holdings
 *   - unesco_intergovernmental_committee: procedural administrator (institutional/constrained)
 *   - colonial_diaspora_descendants: secondary target (powerless/identity_locked)
 *   - private_collectors_and_dealers: excluded market actors (powerful/arbitrage)
 *   - cultural_property_legal_scholars: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.55).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.48).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Regime for Colonial-Era Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "legal/international/post-colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, 'c5b01299-3425-4008-a534-a875afdc137a').
narrative_ontology:cs_kernel_codification('c5b01299-3425-4008-a534-a875afdc137a', formalized).
narrative_ontology:cs_authority_grounding('c5b01299-3425-4008-a534-a875afdc137a', lineage).
narrative_ontology:cs_interpretation_layer_present('c5b01299-3425-4008-a534-a875afdc137a').
narrative_ontology:cs_reading_relation('c5b01299-3425-4008-a534-a875afdc137a', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5b01299-3425-4008-a534-a875afdc137a', cultural_property_legal_corpus__indigenous_stewardship_reading, forecloses).
narrative_ontology:cs_axiom('c5b01299-3425-4008-a534-a875afdc137a', foundational, colonial_acquisition_void_ab_initio).
narrative_ontology:cs_axiom_status(colonial_acquisition_void_ab_initio, holdable).
narrative_ontology:cs_axiom_grounding('c5b01299-3425-4008-a534-a875afdc137a', colonial_acquisition_void_ab_initio, deontological).
narrative_ontology:cs_axiom('c5b01299-3425-4008-a534-a875afdc137a', foundational, state_continuity_confers_repatriation_authority).
narrative_ontology:cs_axiom_status(state_continuity_confers_repatriation_authority, holdable).
narrative_ontology:cs_axiom_grounding('c5b01299-3425-4008-a534-a875afdc137a', state_continuity_confers_repatriation_authority, conventional).
narrative_ontology:cs_reference_frame('c5b01299-3425-4008-a534-a875afdc137a', sovereign_title_restoration_framework).
narrative_ontology:cs_drift_state('c5b01299-3425-4008-a534-a875afdc137a', contemporary_negotiated_settlement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c5b01299-3425-4008-a534-a875afdc137a', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, origin_state_national_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, substate_origin_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_diaspora_descendants).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, national_patrimony_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, state_succession_title_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, post_colonial_restitution_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% National governments claiming historical continuity with polities whose patrimony was removed under colonial rule. They file restitution claims through diplomatic channels and treaty bodies, ratify conventions, and negotiate bilateral memoranda. When objects return, custody and the prestige attached to them accrue to the state; they also spend negotiating resources and accept reciprocal loan obligations. Pursuing, delaying, or shelving individual claims remains available to them at will.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments, agenda_setter).

% State-run museums in origin countries that receive repatriated objects into national collections. They gain holdings, exhibition programs, and visitor revenue. They depend on government allocation for acquisition budgets, storage, and conservation capacity, and some lack the infrastructure to care for returned material — which leaves them advocating for more transfers while depending on the ministries that control the transfers.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, origin_state_national_museums, beneficiary,
    organized, generational, constrained, national).

% Encyclopedic museums in former imperial centers holding large colonial-era collections. They face restitution claims, litigation, and reputational campaigns, and they respond with retention defenses, long-term loan offers, and joint research projects. Their governing charters commit them to holding collections in perpetuity, their loan networks and donor relations depend on staying inside the ethical mainstream, and deaccessioning whole collections to claimant states conflicts with their own legal rules — so their realistic moves are defense, delay, and negotiated partial return.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_museums, payer,
    institutional, civilizational, constrained, global).

% Communities, kingdoms, and peoples within successor states whose ancestors made or used the contested objects. Restitution negotiated between foreign ministries typically delivers objects to national capitals and national museums. These communities hold ceremonial and genealogical ties to specific pieces but rarely hold a seat in the negotiations; their own claims are mediated, and sometimes overridden, by the state's title. When objects come home they may gain access and recognition, or may watch the object arrive in a distant city under someone else's ownership.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, substate_origin_communities, payer,
    powerless, generational, identity_locked, regional).

% Descendants of expropriated peoples living outside the successor state. Their heritage identity is tied to specific objects, but their access to recovered patrimony runs entirely through the state's custody decisions, and they hold no standing in interstate proceedings. They bear the arrangement's outcome without participating in its making.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, colonial_diaspora_descendants, payer,
    powerless, biographical, identity_locked, continental).

% The intergovernmental body providing the standing forum for restitution disputes. It receives state petitions, commissions studies, brokers mediations, and drafts model provisions and return frameworks. Its recommendations bind no one directly, but its procedures define what counts as a proper claim and its convening power shapes which disputes reach resolution. It depends on member-state funding and consensus, which bounds how far it can push against any major party.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, unesco_intergovernmental_committee, agenda_setter,
    institutional, generational, constrained, global).

% Collectors, auction houses, and dealers trading antiquities and ethnographic material. Export controls, provenance due-diligence norms, and restitution precedents raise their acquisition risk and shrink the lawful supply of colonial-era material. They operate largely outside the treaty framework, shift activity toward jurisdictions with weaker controls, and lobby against the framework's expansion into the market.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, private_collectors_and_dealers, excluded,
    powerful, immediate, arbitrage, global).

% Academic lawyers, archaeologists, and museum-studies researchers who map the corpus of conventions, cases, and museum policies, and publish on title theory, state succession, and acquisition ethics. They advise both claimant states and holding institutions, and they observe the system's operation without holding stakes in particular objects.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_state_governments).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__sovereign_repatriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes determinate title rules for contested cultural property: a standing channel for restitution claims that displaces ad hoc dispute, and a documented-provenance standard that deters future illicit excavation and trafficking by making undocumented post-convention acquisitions voidable.
% TRANSFER_FUNCTION: Moves custody and ownership of objects — and the symbolic capital, tourism revenue, and narrative authority attached to them — from holding institutions in former imperial centers to successor-state governments; moves loan guarantees, cooperation agreements, and diplomatic concessions in the opposite direction.
% ABSENT_VOICES: Substate origin communities and diaspora descendants are structurally absent from state-to-state negotiations: the object's originating locality usually has no seat when two foreign ministries bargain over its fate. Private collectors and dealers also sit outside the treaty framework that reshapes their market, and object-specific experts (descendant custodians, ritual practitioners) appear only when a state chooses to invite them.
% DISAPPEARANCE_RATIONALE: If the sovereign-repatriation regime vanished overnight, pending claims would lose their legal-diplomatic channel and revert to raw moral pressure and bilateral bargaining; museums would face unstructured legitimacy campaigns instead of codified process; states would lose the treaty hooks they use to prosecute claims; and the provenance due-diligence standards that suppress illicit trafficking would weaken, reorganizing the antiquities market around weaker verification.
% FOUNDING_PROBLEM: Colonial-era expeditions and administrations removed vast quantities of cultural property under conditions in which the originating peoples had no consent and no recourse; newly independent states inherited the loss with no legal instrument to recover what had been taken or to stop further removal.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the 1970 Convention's own preamble was accepted by the former colonial powers that now resist its fullest application; the major museum associations' ethics codes independently direct members to scrutinize colonial-period acquisitions; and the documentary record of the takings themselves (expedition archives, colonial-era export licenses, auction catalogs) is maintained and cited by historians and holding institutions alike. No party disputes that the takings occurred; the live dispute is over remedy, not history.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__sovereign_repatriation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 (moderate): the arrangement executes real transfers with real costs on both sides — museums lose holdings they regard as lawfully curated, states absorb negotiation and reciprocity costs, and substate communities see claims diluted — while delivering a genuine restorative function. Suppression is 0.48: enforcement runs through treaty obligation, litigation, export controls, and diplomatic pressure — coercive enough that refusal carries real consequences, short of trapping any seat entirely. Theater ratio is 0.35: high-profile symbolic returns increasingly outpace substantive transfer volume, and restitution events serve state image-making, but the underlying claim-processing function is real. Accessibility collapse is 0.52: alternatives (long-term loans, purchases, digital access, shared-stewardship pilots) survive the frame's adoption but lose legal standing once sovereign title is accepted in a given forum. Resistance is 0.60: an organized retentionist counter-movement (the 2002 universal-museum declaration, litigation, slow-walked negotiations) actively contests the arrangement. The claim (tangled_rope) and the metrics are authored independently — the claim states what I believe is structurally true (both coordination and asymmetric extraction, actively enforced); the metrics state what I believe is descriptively true of its operation. The temporal series run on one shared seven-point grid (all three metrics authored at every point) so no metric row borrows another's endpoints. The suppression_requirement series documents a deliberate enforcement ratchet — ratification spread, memorandum proliferation, customs and due-diligence hardening — which is why it is tracked temporally rather than left as a static scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the universal_museums seat the arrangement operates as enforced divestment of collections their charters commit them to hold in perpetuity — high extraction, constrained exit. From the successor_state_governments seat the same structure operates as restoration of wrongfully removed patrimony — a subsidy of justice and identity, with costs they voluntarily incur. From the substate_origin_communities seat it is mixed: the object comes home, but to a capital city and a national collection, under a title that is not theirs. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the beneficiary seats toward the low-d end: successor_state_governments (arbitrage-grade exit — they pursue, delay, or shelve claims at will) sit nearest the beneficiary pole; origin_state_national_museums (constrained, dependent on state allocation) sit slightly higher. Victim declarations drive universal_museums toward the target pole — victim status plus constrained exit (perpetuity charters, loan-network dependence) leaves them bearing scaled extraction. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and the global scope at which verification happens. One explicit override is declared: the powerless seats (substate_origin_communities, colonial_diaspora_descendants) would derive near-full-target directionality from victim status plus locked exit, but both seats receive part of the restoration benefit when objects return to the origin country — access, ceremony, recognition — so their true structural position sits below full target at d = 0.8.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — colonial-era takings for which origin nations had no legal recourse — remains live: unresolved claims continue to accumulate and new disputes arise with each auction and excavation. Mandatrophy is therefore not resolved, and no sunset clause is authored. The tangled_rope classification matters here in both directions: it prevents mislabeling the arrangement as a pure snare (which would erase the real title-clarity and anti-trafficking coordination that even retentionist parties rely on), and it prevents rope certification (which would erase the measurable asymmetry — museums and communities pay through the same structure that subsidizes states). The R5 mismatch consumer currently reads consistently: founding_problem_status = live with disappearance_verdict = world_rearranges raises no zombie flag; if the founding problem were ever declared dead while the world still rearranged around the regime, that mismatch would fire against the theater trend visible in the measurement series.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the sovereign_repatriation_reading of the cultural_property_legal_corpus kernel. What would adopting a sibling reading change structurally?',
    'Comparative analysis across the three authored readings of the kernel: recompute beneficiary/victim sets, directionality, and epsilon under each reading and compare the resulting classifications.',
    'Under universal_heritage_reading the beneficiary set inverts (holding institutions and the global public benefit; claimant states become cost-bearers) and epsilon drops toward coordination-cost levels. Under indigenous_stewardship_reading the successor states themselves become extractors vis-a-vis the communities they claim to represent, and the victim set expands to include substate communities directly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    historical_continuity_eligibility,
    'Which states genuinely qualify as successors ''claiming historical continuity with expropriated peoples,'' versus mere territorial successors with attenuated connection to the expropriated polity?',
    'Genealogical and legal analysis of state-succession doctrine applied case by case: compare the expropriated polity (kingdom, empire, community) with the claiming modern state''s institutional and demographic continuity.',
    'If continuity is thin for many claimants, the reading''s benefits concentrate in states with weak connection to the making communities while costs stay fixed on holding institutions — raising effective extraction and pushing the arrangement toward the snare end; robust continuity supports the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_continuity_eligibility, conceptual, 'Whether the beneficiary class tracks expropriated polities or merely successor territories.').

omega_variable(
    community_claim_subordination_degree,
    'In practice, how completely do state-level title claims extinguish or subordinate the specific claims of substate origin communities to their own objects?',
    'Track outcomes of completed repatriations: destination of returned objects (national capital vs. originating region), consultation records, and litigation between states and their own communities over custody.',
    'High subordination means the arrangement coordinates states and museums while imposing its residual costs on the least powerful seat — strengthening the asymmetric-extraction component; low subordination means the state channel functions as a pass-through and the extraction reading weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_claim_subordination_degree, empirical, 'Degree to which state title overrides community claims in settled cases.').

omega_variable(
    repatriation_gain_capture,
    'Do the gains of repatriation — symbolic capital, tourism revenue, narrative authority — accrue to national publics broadly, or concentrate in state executives and national museums?',
    'Follow-the-money analysis of returned-object economies: exhibition siting, admission revenue flows, and political uses of restitution events versus community access outcomes.',
    'Concentrated capture supports naming a capturer seat and sharpens the extraction profile; broad diffusion supports the coordination reading of the transfer function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(repatriation_gain_capture, empirical, 'Distribution of repatriation benefits across national publics and state elites.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sovereign_repatriation_tr_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sovereign_repatriation_tr_t0, observed).
narrative_ontology:measurement(sovereign_repatriation_tr_t9, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 9, 0.18).
narrative_ontology:measurement_basis(sovereign_repatriation_tr_t9, observed).
narrative_ontology:measurement(sovereign_repatriation_tr_t18, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement_basis(sovereign_repatriation_tr_t18, observed).
narrative_ontology:measurement(sovereign_repatriation_tr_t27, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 27, 0.26).
narrative_ontology:measurement_basis(sovereign_repatriation_tr_t27, observed).
narrative_ontology:measurement(sovereign_repatriation_tr_t36, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 36, 0.3).
narrative_ontology:measurement_basis(sovereign_repatriation_tr_t36, observed).
narrative_ontology:measurement(sovereign_repatriation_tr_t45, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 45, 0.33).
narrative_ontology:measurement_basis(sovereign_repatriation_tr_t45, observed).
narrative_ontology:measurement(sovereign_repatriation_tr_t54, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 54, 0.35).
narrative_ontology:measurement_basis(sovereign_repatriation_tr_t54, observed).

% Extraction over time
narrative_ontology:measurement(sovereign_repatriation_be_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(sovereign_repatriation_be_t0, observed).
narrative_ontology:measurement(sovereign_repatriation_be_t9, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 9, 0.37).
narrative_ontology:measurement_basis(sovereign_repatriation_be_t9, observed).
narrative_ontology:measurement(sovereign_repatriation_be_t18, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 18, 0.41).
narrative_ontology:measurement_basis(sovereign_repatriation_be_t18, observed).
narrative_ontology:measurement(sovereign_repatriation_be_t27, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 27, 0.46).
narrative_ontology:measurement_basis(sovereign_repatriation_be_t27, observed).
narrative_ontology:measurement(sovereign_repatriation_be_t36, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 36, 0.5).
narrative_ontology:measurement_basis(sovereign_repatriation_be_t36, observed).
narrative_ontology:measurement(sovereign_repatriation_be_t45, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 45, 0.53).
narrative_ontology:measurement_basis(sovereign_repatriation_be_t45, observed).
narrative_ontology:measurement(sovereign_repatriation_be_t54, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 54, 0.55).
narrative_ontology:measurement_basis(sovereign_repatriation_be_t54, observed).

% Suppression requirement over time
narrative_ontology:measurement(sovereign_repatriation_su_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement_basis(sovereign_repatriation_su_t0, observed).
narrative_ontology:measurement(sovereign_repatriation_su_t9, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 9, 0.29).
narrative_ontology:measurement_basis(sovereign_repatriation_su_t9, observed).
narrative_ontology:measurement(sovereign_repatriation_su_t18, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 18, 0.33).
narrative_ontology:measurement_basis(sovereign_repatriation_su_t18, observed).
narrative_ontology:measurement(sovereign_repatriation_su_t27, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 27, 0.38).
narrative_ontology:measurement_basis(sovereign_repatriation_su_t27, observed).
narrative_ontology:measurement(sovereign_repatriation_su_t36, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 36, 0.42).
narrative_ontology:measurement_basis(sovereign_repatriation_su_t36, observed).
narrative_ontology:measurement(sovereign_repatriation_su_t45, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 45, 0.45).
narrative_ontology:measurement_basis(sovereign_repatriation_su_t45, observed).
narrative_ontology:measurement(sovereign_repatriation_su_t54, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 54, 0.48).
narrative_ontology:measurement_basis(sovereign_repatriation_su_t54, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial question 'who owns culture?' covers three structurally distinct claims with different epsilon values, beneficiary/victim sets, and failure modes, and is therefore authored as three linked stories rather than one story with a measurement parameter. This story instantiates the sovereign_repatriation_reading (state-centered title restoration, moderate epsilon). universal_heritage_reading is the upstream sibling — older and institutionalized in museum practice — whose preservation-and-access claim historically justified the holdings this reading now challenges; this reading structurally influences it by forcing access claims to be renegotiated as loans rather than assumed as rights. indigenous_stewardship_reading is the downstream sibling: this reading's state-channel template sets the procedural environment through which community claims must pass, shaping that sibling's operating conditions without resolving the contest between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__sovereign_repatriation_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
