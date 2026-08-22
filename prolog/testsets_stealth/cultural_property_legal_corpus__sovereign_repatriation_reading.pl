% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__sovereign_repatriation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Sovereign Repatriation Reading of the Cultural Property Corpus
 *   domain: international law/cultural property/post-colonial studies
 *
 * SUMMARY:
 *   This story instantiates the sovereign_repatriation_reading of the
 *   cultural_property_legal_corpus kernel: the operative arrangement under
 *   which artifacts displaced during colonial acquisition are the sovereign
 *   property of successor states claiming historical continuity with the
 *   expropriated peoples, and holding institutions in former colonial
 *   metropoles bear enforced restitution. The reading recodes holding
 *   institutions from universal trustees into custodians of extracted
 *   identity capital, which is what converts their retention into an
 *   enforcement object and their return of objects into a duty rather than a
 *   gift. The epsilon referent is that operative state-vested repatriation
 *   arrangement — patrimony statutes, bilateral memoranda, UNESCO mediation,
 *   the active return caseload — assessed by this reading's own lights: the
 *   reading condemns the colonial retention regime as theft, but this story's
 *   constraint is the repatriation structure itself, whose costs
 *   (repatriation logistics, diplomatic friction, access loss, community
 *   re-mediation) and gains (symbolic capital, authority, tourism) are both
 *   real, yielding moderate epsilon. Family decomposition: the sibling
 *   readings are separate constraints with different epsilon — under
 *   universal_heritage_reading the same holding arrangement computes as
 *   low-extraction coordination; under indigenous_stewardship_reading the
 *   state apparatus is the extractor and communities are beneficiaries. This
 *   story authors only the sovereignty reading and does not hedge across
 *   siblings.
 *
 * KEY AGENTS:
 *   - successor_states: agenda-setting beneficiary (institutional / identity_locked) — post-colonial states asserting sovereign ownership of colonial-era removals; collect symbolic capital, nationalist legitimacy, and tourism; their continuity claims are constitutive of founding identity narratives
 *   - universal_museums: primary payer (institutional / constrained) — encyclopedic institutions in former colonial metropoles bearing restitution demands, statutory deaccession bars, and diplomatic campaigns
 *   - descendant_communities: re-mediated payer (powerless / trapped) — communities descended from expropriated peoples whose direct claims are legally invisible; returned objects pass into state custody over their heads
 *   - state_heritage_ministries: secondary beneficiary (organized / constrained) — administer patrimony laws, permits, export controls, and negotiations; returns expand portfolios and budgets
 *   - origin_state_national_museums: secondary beneficiary (institutional / constrained) — receive returned collections; custody and access policies set by the state
 *   - holding_state_governments: institutional payer (institutional / constrained) — absorb diplomatic friction, legislate return exceptions, trade repatriations for bilateral goodwill
 *   - antiquities_market_intermediaries: excluded (powerful / arbitrage) — dealers and auction houses whose trade the sovereignty premise criminalizes; no seat in the allocating channel
 *   - global_museum_publics: diffuse payer (organized / constrained) — international visitors, researchers, and diaspora audiences losing access as objects relocate behind state custody
 *   - unesco_return_committee: analytical observer (institutional / analytical) — ICPRCP mediates without binding authority; cannot seat community claimants
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.5).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.6).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign Repatriation Reading of the Cultural Property Corpus").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international law/cultural property/post-colonial studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, '87efab19-dc90-4948-b1a7-87ff60bc5a17').
narrative_ontology:cs_kernel_codification('87efab19-dc90-4948-b1a7-87ff60bc5a17', distributed).
narrative_ontology:cs_authority_grounding('87efab19-dc90-4948-b1a7-87ff60bc5a17', distributed).
narrative_ontology:cs_reading_relation('87efab19-dc90-4948-b1a7-87ff60bc5a17', cultural_property_legal_corpus__universal_heritage_reading, forecloses).
narrative_ontology:cs_reading_relation('87efab19-dc90-4948-b1a7-87ff60bc5a17', cultural_property_legal_corpus__indigenous_stewardship_reading, forecloses).
narrative_ontology:cs_axiom('87efab19-dc90-4948-b1a7-87ff60bc5a17', foundational, colonial_acquisition_invalidates_title).
narrative_ontology:cs_axiom_status(colonial_acquisition_invalidates_title, holdable).
narrative_ontology:cs_axiom_grounding('87efab19-dc90-4948-b1a7-87ff60bc5a17', colonial_acquisition_invalidates_title, deontological).
narrative_ontology:cs_axiom('87efab19-dc90-4948-b1a7-87ff60bc5a17', foundational, state_continuity_confers_cultural_authority).
narrative_ontology:cs_axiom_status(state_continuity_confers_cultural_authority, holdable).
narrative_ontology:cs_axiom_grounding('87efab19-dc90-4948-b1a7-87ff60bc5a17', state_continuity_confers_cultural_authority, conventional).
narrative_ontology:cs_axiom('87efab19-dc90-4948-b1a7-87ff60bc5a17', secondary, national_patrimony_inalienable).
narrative_ontology:cs_axiom_status(national_patrimony_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('87efab19-dc90-4948-b1a7-87ff60bc5a17', national_patrimony_inalienable, conventional).
narrative_ontology:cs_reference_frame('87efab19-dc90-4948-b1a7-87ff60bc5a17', sovereign_patrimony_restoration_order).
narrative_ontology:cs_drift_state('87efab19-dc90-4948-b1a7-87ff60bc5a17', contemporary_non_retroactivity_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('87efab19-dc90-4948-b1a7-87ff60bc5a17', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, state_heritage_ministries).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, origin_state_national_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_museums).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, descendant_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_state_governments).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, global_museum_publics).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, state_patrimony_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, sovereign_continuity_principle).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_nationalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Post-colonial states such as Egypt, Greece, Nigeria, Ethiopia, Turkey, and Cambodia assert sovereign ownership of artifacts removed during the colonial period, legislate national patrimony laws, sign bilateral memoranda, and petition or litigate for returns. Recovered objects feed national identity narratives, museum holdings, tourism, and diplomatic standing. Their continuity claims are woven into founding legitimacy stories, which makes retreating from the claims costly in domestic politics, and international law offers no other recognized claimant identity.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states, beneficiary).

% Encyclopedic museums in former colonial metropoles hold the disputed collections. They face restitution demands, litigation risk, and coordinated diplomatic campaigns. Several are barred by their governing statutes from deaccessioning; their practical options are negotiated returns, loan-backs, joint custody, and digital surrogates, all within terms the claimant states help set. They cannot sell, cannot walk away from their buildings and audiences, and draw reputational fire whichever way they move.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_museums, payer,
    institutional, generational, constrained, global).

% Communities descended from the expropriated peoples hold living cultural and often sacred relationships to the objects. The recognized claimant in the international channel is the state, not the community: returned objects pass into national custody, and the community's own claim has no legal forum. Many of these communities are also minorities or political losers inside the very state that claims to speak for them.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, descendant_communities, payer,
    powerless, generational, trapped, regional).

% Culture ministries and antiquities authorities write and administer patrimony laws, issue excavation permits, run export controls, and conduct restitution negotiations. Successful claims expand their mandates, budgets, and staff. Their institutional purpose is bound to the state-patrimony frame; recognizing community-level authority over objects would dissolve the basis of their own office.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, state_heritage_ministries, beneficiary,
    organized, generational, constrained, national).

% National museums in claimant states receive returned collections and gain holdings, prestige, and visitor revenue. What happens to an object after return — display, storage, ceremonial use, access for local communities — is decided under the same state authority that claimed it, and practices vary widely.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, origin_state_national_museums, beneficiary,
    institutional, generational, constrained, national).

% Governments of holding states absorb the diplomatic friction of restitution politics, pass the legislative exceptions that let national museums return objects, and sometimes trade repatriations for bilateral goodwill and market access abroad. They lose objects of national prestige while gaining soft-power returns; the balance differs by country and by election cycle.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_state_governments, payer,
    institutional, generational, constrained, national).

% Dealers, auction houses, and freeport operators trade in the same object class. The sovereignty premise criminalizes much of the trade in claimed patrimony and permanently removes the strongest pieces from the market. They have no seat in the state-to-state channel where authority is allocated; they adapt by shifting business to jurisdictions with weaker implementation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, antiquities_market_intermediaries, excluded,
    powerful, biographical, arbitrage, global).

% International visitors, researchers, and diaspora audiences who could see and study the objects in universal museums. As objects relocate to origin-state custody, their access depends on that state's policies — opening hours, ticketing, storage versus display, permits for study. Travel and digital surrogates are partial substitutes, both controlled by the custodial state.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, global_museum_publics, payer,
    organized, biographical, constrained, global).

% UNESCO's Intergovernmental Committee for Promoting the Return of Cultural Property (est. 1978) mediates claims, drafts model principles, and documents the unresolved caseload. It has no binding power, its mediation presupposes the state-to-state channel, and it cannot seat community claimants.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, unesco_return_committee, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__sovereign_repatriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates recognized authority over culturally displaced objects among competing claimants: the state-sovereignty channel gives restitution claims a workable legal vehicle (state-to-state negotiation, UNESCO mediation, bilateral memoranda), deters ongoing looting by vesting ownership in a identifiable principal, and prevents a default in which possessors and market actors settle the question unilaterally.
% TRANSFER_FUNCTION: Moves custody and title of colonial-era artifacts from holding institutions in former colonial metropoles to successor-state control; moves symbolic capital, nationalist legitimacy, and tourism revenue to claimant states; moves costs — repatriation logistics, access loss, diplomatic friction — to holding institutions, holding-state governments, and global publics.
% ABSENT_VOICES: Antiquities market intermediaries are structurally excluded from the state-to-state channel that allocates authority. Stateless or unrecognized communities — peoples whose successor state does not claim continuity with them, or whose state denies their continuity — have no standing anywhere in the regime: the channel recognizes only states, so their claims are invisible rather than rejected. Universal-museum publics are consulted, when at all, after allocation decisions are made.
% DISAPPEARANCE_RATIONALE: State patrimony statutes, bilateral memoranda, UNESCO mediation, and the active caseload of restitution claims all presuppose the sovereignty premise. Overnight removal would strand hundreds of pending claims with no recognized claimant vehicle, museums would retain most contested collections by default, claimant states would lose their principal instrument of heritage sovereignty, and the allocation question would rearrange around whichever rival doctrine — universal trusteeship or community stewardship — filled the vacuum.
% FOUNDING_PROBLEM: Colonial-era removal of cultural property without consent, and the post-colonial need for new states to protect remaining heritage from continued removal — the 1954 Hague and 1970 UNESCO instruments were built to stop wartime plunder and illicit trafficking and to give newly sovereign states legal title to their patrimony.
% FOUNDING_PROBLEM_CORROBORATION: Independent legal scholarship (the Merryman 'two ways of thinking' literature) and ICOM's cross-community ethics work attest both halves: illicit trafficking remains live, and the fit of the state-sovereignty instrument to historical restitution is disputed. The Sarr-Savoy commission (2018), authored by experts outside the benefiting states though commissioned by a holding state, attests the colonial-removal problem as unresolved. No attestation exists that is fully outside the field's beneficiary structure — UNESCO bodies are state-composed, and source-community advocacy is itself a party. That gap is itself signal.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.5 (moderate): the regime delivers genuine restitution — objects physically move, looting deterrence is real — while imposing asymmetric costs through the same structure: holding institutions bear disgorgement and legal exposure, descendant communities bear re-mediation (the artifact passes to the state that claims to speak for them, not to them), and global publics bear access loss behind state custody. Suppression 0.6: the coercive core is structural — international legal standing recognizes only states, so community claims are foreclosed rather than heard — plus diplomatic and economic leverage on holding institutions. Theater 0.4: real transfers coexist with heavy symbolic activity (single-object returns amid grand declarations, decades of ICPRCP mediation with a thin resolution record, retention-and-explain posturing). Accessibility_collapse 0.55: within the legal frame the sovereignty premise forecloses rival allocations (no standing for communities, no vehicle for universal trusteeship), though hybrid arrangements — loan-backs, joint custody — persist in practice. Resistance 0.62: sustained institutional resistance (statutory deaccession bars, the 2002 Universal Museums Declaration, market-state pushback, scholarly critique of access loss). The measurement series runs on one shared grid (t=0..70, mapping 1954 Hague to 2024): extraction and enforcement capacity rose together as the sovereignty premise extended from wartime protection to post-colonial patrimony; theater rose later as symbolic returns outpaced structural transfer. The claimed type and the metrics are authored independently; the engine computes per-seat types from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   Three seats compute different constraints from one structure. From the successor-state seat the arrangement is a restitution order — corrective, dignity-restoring, its costs borne by former expropriators. From the universal-museum seat it is enforced disgorgement under diplomatic coercion, with stewardship and access losses. From the descendant-community seat it is re-mediation: the state interposes itself between community and artifact, converting a restitution claim into state patrimony. The engine derives these divergences from the declared roles, power atoms, and exit options; the divergence between the state seat (low d) and the community seat (high d despite the restitution framing that nominally includes them) is this story's central measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (successor_states, state_heritage_ministries, origin_state_national_museums) derive low d for the state complex — the arrangement subsidizes it with symbolic capital, authority, and revenue. Victim declarations derive high d for universal_museums (constrained exit: statutory bars, no sale, diplomatic exposure) and the highest d for descendant_communities (trapped: no international standing, claims overridden by the recognized claimant). holding_state_governments sit near the target end but partially offset — returns convert into soft-power capital — a mixed position the derivation captures only coarsely; no override is authored because the override surface is per-power-atom and cannot separate institutional beneficiaries (origin museums) from institutional payers (universal museums) within the same atom. global_museum_publics carry diffuse moderate-high d. antiquities_market_intermediaries are excluded rather than coordinated — their exclusion is part of the enforcement object. unesco_return_committee holds the analytical seat with no extraction position. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification blocks two symmetric mislabels. Reading the regime as pure coordination (the universal-museum 'orderly restitution' frame, or the reading's own self-description) erases the asymmetric extraction: community re-mediation and access costs are paid through the same structure that delivers restitution. Reading it as pure extraction (the nationalist-consolidation critique) erases the genuine corrective function: colonial removal was real, returns are real, and looting deterrence solves a live collective-action problem. The R5 genealogy supports the hybrid: the founding problem (colonial removal, ongoing trafficking) is contested rather than dead — the parties dispute whether the state-sovereignty instrument still serves it — so this is not a case of a dead mandate kept alive by inertia; it is a live mandate whose instrument extracts asymmetrically. If post-return custody practice shows objects moving into state storage or ceremonial nationalist use, the extraction component rises and the classification drifts snare-ward; the temporal series and the restitution_vs_consolidation omega track exactly that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which allocation of authority over displaced cultural property does the cultural_property_legal_corpus itself mandate — state sovereignty (this reading), universal trusteeship, or community stewardship?',
    'Doctrinal analysis of the corpus''s texts and practice (1954 Hague, 1970 UNESCO, ICPRCP caseload, domestic patrimony statutes): the corpus under-specifies the authority question for pre-convention removals, which is why three readings compete; the disagreement is located specifically in who holds legitimate authority over objects removed before the conventions entered force.',
    'If the universal-heritage reading governs, holding institutions become coordinators and this story''s epsilon collapses toward coordination cost; if the indigenous-stewardship reading governs, descendant communities become beneficiaries and the state apparatus becomes the extractor. This story''s classification holds only within the sovereignty reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'This constraint is one reading of an under-determined kernel; sibling readings instantiate different constraints.').

omega_variable(
    continuity_claim_validity,
    'For which state-people pairs does the successor state''s claim of historical continuity with the expropriated polity actually hold — and where it fails, does the state''s claim replicate the removal it condemns?',
    'Case-by-case genealogical and representational analysis: does the modern state''s territory and institutional lineage connect to the expropriated polity, and does the state represent the descendant community''s interests in custody decisions?',
    'Where continuity fails — a state whose borders and population were drawn without reference to the expropriated polity — the reading''s allocation re-concentrates the artifact in a new distant custodian, and the constraint''s operation drifts toward pure extraction for those pairs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_claim_validity, empirical, 'Validity of state-to-polity continuity claims across successor states varies widely.').

omega_variable(
    restitution_vs_consolidation,
    'Is the regime''s operative function historical correction or nationalist consolidation — do returned objects become accessible to descendant communities and plural publics, or are they deployed in state identity narratives that may exclude minorities?',
    'Track post-return custody and access practices across returned collections: community access, display narratives, storage versus display, ceremonial state use.',
    'If consolidation dominates, the costs borne by descendant communities and publics rise above the authored moderate level and the coordination function thins toward cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restitution_vs_consolidation, empirical, 'Whether returns correct the historical removal or re-concentrate it at the state.').

omega_variable(
    access_loss_magnitude,
    'How large is the access cost to global publics and scholarship — a real cost borne by diffuse users, or a self-serving overstatement by holding institutions defending collections?',
    'Comparative access studies: origin-state museum access policies, storage rates for returned objects, digital access provision, and visitor flows before and after transfers.',
    'If access loss is minor, the constraint''s epsilon drops toward pure corrective coordination; if severe — objects in restricted storage — epsilon rises and the payer set widens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_loss_magnitude, empirical, 'Magnitude of the access-transfer cost the regime imposes on global publics.').

omega_variable(
    museum_identity_vs_interest,
    'Is universal-museum resistance driven by material interest (collections, tourism, scholarly capital) or by institutional identity (self-conception as trustees of a common human heritage)?',
    'Observe resistance when material interests are compensated (loan-backs, joint custody, funding): if resistance persists past compensation, the identity component dominates.',
    'If identity-driven, the regime''s pressure on museum alternatives is partly self-reinforcing and the coordination function is more stable than interest-based models predict; the tangled-rope reading holds with higher confidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(museum_identity_vs_interest, empirical, 'Identity-lock versus interest basis of holding-institution resistance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(cult_tr_t0, observed).
narrative_ontology:measurement(cult_tr_t10, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(cult_tr_t10, observed).
narrative_ontology:measurement(cult_tr_t20, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(cult_tr_t20, observed).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(cult_tr_t30, observed).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement_basis(cult_tr_t40, observed).
narrative_ontology:measurement(cult_tr_t50, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement_basis(cult_tr_t50, observed).
narrative_ontology:measurement(cult_tr_t60, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement_basis(cult_tr_t60, observed).
narrative_ontology:measurement(cult_tr_t70, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 70, 0.4).
narrative_ontology:measurement_basis(cult_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(cult_be_t0, observed).
narrative_ontology:measurement(cult_be_t10, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(cult_be_t10, observed).
narrative_ontology:measurement(cult_be_t20, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(cult_be_t20, observed).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(cult_be_t30, observed).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement_basis(cult_be_t40, observed).
narrative_ontology:measurement(cult_be_t50, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 50, 0.46).
narrative_ontology:measurement_basis(cult_be_t50, observed).
narrative_ontology:measurement(cult_be_t60, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement_basis(cult_be_t60, observed).
narrative_ontology:measurement(cult_be_t70, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 70, 0.5).
narrative_ontology:measurement_basis(cult_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(cult_su_t0, observed).
narrative_ontology:measurement(cult_su_t10, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 10, 0.28).
narrative_ontology:measurement_basis(cult_su_t10, observed).
narrative_ontology:measurement(cult_su_t20, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 20, 0.36).
narrative_ontology:measurement_basis(cult_su_t20, observed).
narrative_ontology:measurement(cult_su_t30, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement_basis(cult_su_t30, observed).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(cult_su_t40, observed).
narrative_ontology:measurement(cult_su_t50, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 50, 0.54).
narrative_ontology:measurement_basis(cult_su_t50, observed).
narrative_ontology:measurement(cult_su_t60, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 60, 0.57).
narrative_ontology:measurement_basis(cult_su_t60, observed).
narrative_ontology:measurement(cult_su_t70, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 70, 0.6).
narrative_ontology:measurement_basis(cult_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the cultural_property_legal_corpus kernel. The colloquial label 'cultural property law' conflates three structurally distinct authority allocations over displaced objects; per the epsilon-invariance principle each is authored as its own story with its own epsilon, beneficiary set, and classification. This story instantiates the sovereign_repatriation_reading (authority with continuity-claiming successor states; moderate epsilon; gains accrue to the state complex). The universal_heritage_reading computes the same holding arrangement as low-extraction coordination (institutions maximizing preservation and access); the indigenous_stewardship_reading computes the state apparatus itself as the extractor and descendant communities as beneficiaries. The corpus's shared anti-trafficking core feeds all three readings; the readings diverge precisely on the authority question the corpus under-specifies for pre-convention removals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
