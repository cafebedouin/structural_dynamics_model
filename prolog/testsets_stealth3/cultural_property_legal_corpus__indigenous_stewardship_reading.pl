% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Standing Artifact Retention Regime as Assessed by the Indigenous Stewardship Reading
 *   domain: international_law/cultural_property/post_colonial
 *
 * SUMMARY:
 *   The standing arrangement under contest is the retention regime: cultural
 *   artifacts - including sacred objects, ceremonial regalia, and ancestral
 *   remains - acquired under colonial conditions and held today by
 *   encyclopedic museums and successor-state agencies, with custody governed
 *   by a corpus of patrimony statutes, export regimes, affiliation
 *   procedures, and institutional ethics codes. This file generates the
 *   indigenous_stewardship_reading of that arrangement: artifacts are sacred
 *   or communal property of the communities maintaining cultural continuity
 *   with them, so custody by parties without such continuity is illegitimate
 *   however well-intentioned. KEY AGENTS (by structural relationship): -
 *   encyclopedic_museums: agenda-setting primary beneficiary
 *   (institutional/arbitrage) - administers custody rules and receives the
 *   regime's material gains; - successor_state_cultural_agencies: secondary
 *   beneficiary and co-enforcer (institutional/constrained) - convert
 *   holdings into state legitimation and control export and title; -
 *   source_indigenous_communities: primary target (organized/identity_locked)
 *   - bear lost custody, blocked ceremony, and separated ancestors; -
 *   descendant_diaspora_communities: secondary target (moderate/constrained)
 *   - same loss, weaker standing; - heritage_scholarship_sector: tertiary
 *   beneficiary (organized/mobile) - careers run on retained access; -
 *   artifact_market_participants: opportunistic beneficiary
 *   (powerful/arbitrage) - profits from custody ambiguity; -
 *   repatriation_advocacy_networks: dual payer/beneficiary (moderate/mobile)
 *   - carry claim costs, collect precedent wins; -
 *   international_heritage_bodies: analytical observer
 *   (institutional/analytical). EPSILON REFERENT DISCIPLINE: epsilon is
 *   authored for the standing retention arrangement as this reading assesses
 *   it, never for the community-custody arrangement this reading endorses -
 *   had epsilon been authored for the endorsed alternative, every advocacy
 *   reading would trivially compute zero. The claim/metric gap is deliberate
 *   and independent: the claimed type states this reading's structural
 *   assessment; the metrics describe the regime's observed operation; the
 *   engine measures their divergence rather than the author reconciling them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.86).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.7).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Standing Artifact Retention Regime as Assessed by the Indigenous Stewardship Reading").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '9ba82b2f-8d90-4456-9b3e-3cbb8e942962').
narrative_ontology:cs_kernel_codification('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', formalized).
narrative_ontology:cs_authority_grounding('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', lineage).
narrative_ontology:cs_interpretation_layer_present('9ba82b2f-8d90-4456-9b3e-3cbb8e942962').
narrative_ontology:cs_reading_relation('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', cultural_property_legal_corpus__sovereign_repatriation_reading, forecloses).
narrative_ontology:cs_axiom('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', foundational, continuity_confers_custodial_authority).
narrative_ontology:cs_axiom_status(continuity_confers_custodial_authority, holdable).
narrative_ontology:cs_axiom_grounding('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', continuity_confers_custodial_authority, deontological).
narrative_ontology:cs_axiom('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', foundational, sacred_artifacts_communally_inalienable).
narrative_ontology:cs_axiom_status(sacred_artifacts_communally_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', sacred_artifacts_communally_inalienable, deontological).
narrative_ontology:cs_reference_frame('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', community_continuity_custodianship).
narrative_ontology:cs_drift_state('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', contemporary_post_undrip_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9ba82b2f-8d90-4456-9b3e-3cbb8e942962', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_state_cultural_agencies).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, heritage_scholarship_sector).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, artifact_market_participants).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, source_indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, descendant_diaspora_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, repatriation_advocacy_networks).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, repatriation_advocacy_networks).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, continuity_based_custodial_legitimacy).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, repatriation_as_redress_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold large ethnographic and sacred-object collections acquired largely under colonial conditions. Set conservation, exhibition, loan, and repatriation-response policy; derive endowment growth, attendance, scholarly partnership, and civic prestige from retained holdings. Can deaccession selectively, lend internationally, or tour exhibitions to manage reputational exposure, and face repatriation demands case by case rather than as a standing obligation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums, beneficiary).

% Ministries, national museums, and antiquities authorities of states constituted over expropriated peoples - both former imperial centers retaining overseas collections and successor states holding objects taken from internal indigenous minorities. Assert legal title, license export and excavation, conduct state-to-state return diplomacy, and draw nationalist legitimation from anchor holdings. Their custody narrative binds them to the collection they administer, so unilateral release undermines the state story itself.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_state_cultural_agencies, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_state_cultural_agencies, agenda_setter).

% Maintain ceremonial calendars, oral law, and kinship obligations tied to specific masks, regalia, ancestral remains, and records. Bear blocked ritual access, the burden of documenting affiliation under procedures designed by holding institutions, litigation and travel costs, and the spiritual consequences attributed to separated objects. Pursue return through petitions, tribally funded legal programs, and international advocacy; abandoning the claim would mean severing the continuity the claim exists to protect.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, source_indigenous_communities, payer,
    organized, generational, identity_locked, regional).

% Descendants displaced by removal or migration whose continuity documentation is fragmentary. Bear the same separation from objects and ancestors with weaker standing in affiliation hearings, often excluded from both museum consultation rosters and state-to-state succession negotiations, and lacking the enrolled-community legal standing that statutory channels presuppose.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, descendant_diaspora_communities, payer,
    moderate, biographical, constrained, global).

% Curators, conservators, archaeologists, and art historians whose fields are built on stable access to retained collections. Publish, train students, staff exhibitions, and advise media on the strength of holdings that require no consent-based clearance; careers and department funding depend on continued availability, and consent regimes would raise operating costs and slow publication cycles.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, heritage_scholarship_sector, beneficiary,
    organized, biographical, mobile, global).

% Dealers and auction houses trading decontextualized ethnographic material. Benefit where custody legitimacy stays ambiguous enough to keep provenance titles negotiable; reprice instantly around repatriation headlines and jurisdictional gaps, and can route sales through the permissive jurisdictions that the corpus of national laws leaves open.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, artifact_market_participants, beneficiary,
    powerful, immediate, arbitrage, global).

% Indigenous lawyers, provenance researchers, and campaign organizations running claims across jurisdictions. Absorb travel, archival, and litigation costs for uncertain returns; accumulate professional standing and precedent wins when repatriations succeed, which funds and recruits for the next claim cycle.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, repatriation_advocacy_networks, payer,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, repatriation_advocacy_networks, beneficiary).

% Convention secretariats, museum-ethics bodies, and expert panels drafting soft-law norms that mediate custody disputes. Convene states, museums, and communities, publish guidance and model returns frameworks, and carry no coercive force over titled holdings; their influence runs through legitimacy pressure rather than enforcement.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_heritage_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__indigenous_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves preservation and access problems for fragile, looter-vulnerable, war-threatened material: centralized climate-controlled conservation, cataloguing, scholarly study, and public display are provided once, centrally, instead of per-community - and the corpus supplies a common legal vocabulary (title, provenance, affiliation) within which custody disputes can be argued at all.
% TRANSFER_FUNCTION: Moves custodial authority, ritual access, and the material and spiritual value of sacred objects away from source indigenous communities toward holding museums, successor-state agencies, scholarship, and markets; moves prestige, attendance revenue, and scholarly careers toward the holders.
% ABSENT_VOICES: Descendant lines outside official enrollment rolls cannot sit in affiliation hearings; communities whose continuity documentation was destroyed by the very dispossession that produced the holdings cannot satisfy evidentiary procedures designed around archives; future ceremonial practitioners not yet born have no seat anywhere. Holding-institution boards and donor bases encounter repatriation claims filtered through curatorial gatekeepers rather than directly.
% DISAPPEARANCE_RATIONALE: If the retention regime vanished overnight, ethnographic galleries would empty as titled holdings moved to continuity-holding communities, successor-state heritage narratives would lose their anchor objects, ceremonial practice would resume wherever surviving materials permit, museum economics built on permanent ethnographic display would restructure around loans negotiated from community custody, and the ethnographica market would reprice sharply as legitimate titles narrowed.
% FOUNDING_PROBLEM: Colonial-era expropriation left scattered, deteriorating, war-threatened collections with no agreed answer to who may legitimately hold them; the corpus was assembled to regularize title after imperial collapse, halt illicit traffic, and give states and institutions a defensible custody order.
% FOUNDING_PROBLEM_CORROBORATION: The trafficking and destruction half of the founding problem is corroborated from outside the benefiting parties: UNESCO illicit-traffic statistics, INTERPOL red lists, and documented wartime losses (Iraq, Syria, Mali, Libya) attest it remains live. The legitimacy half is attested as never-settled by indigenous legal scholarship, UNDRIP implementation reporting, and museum associations' own published acknowledgments that custody legitimacy is unresolved. No party attests that the legitimacy question was ever resolved; the benefiting parties affirm only the regularization half.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.86) because the regime transfers custodial authority, ritual access, and ancestral remains to parties this reading credits with no legitimate claim, and because the transfer compounds: every year of retention deepens ceremonial deprivation and strengthens holder title. Suppression is 0.70 and is authored as a raw structural property, deliberately unscaled: it reflects title regimes, export licensing, affiliation bureaucracies designed by holders, and litigation asymmetry, with a possible internalized component flagged by omega. Theater_ratio is 0.45: universal-stewardship rhetoric ('held in trust for all humanity') thickens as return fractions stay marginal relative to holdings, but conservation science, disaster protection, and cataloguing are genuinely functional, keeping the ratio well below inertial range. Accessibility_collapse is 0.38: alternatives - statutory channels, diplomatic return, community custody - do not collapse once the regime is understood; they are increasingly exercised, so this is not a natural-law-style closure. Resistance is 0.68: organized transnational claim networks, statutory mandates, and rights-framework advocacy meet the regime actively. The temporal series run on one shared grid (t = 0,6,12,18,24,30 indexing roughly 1990-2020, from NAGPRA's enactment to the contemporary repatriation era) with all three tracked metrics authored at every point; the trajectories show extraction accumulation and enforcement maturation, not cyclical oscillation, so no cycle-phase caveat applies. Final series values match the base_properties scalars by construction.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical nominal standing. From the encyclopedic_museums seat the regime looks coordination-dominant: it built conservation infrastructure, absorbs security and climate risk, lends and digitizes globally, and answers claims case by case. From the source_indigenous_communities seat the same structure computes extraction-dominant: the coordination good is delivered at their expense and without their authority, and exit is identity-locked because relinquishing the claim severs the continuity the claim protects. The successor_state seat experiences a legitimation device: it pays little and collects nationalist anchoring. Two institutional actors at the same power level (museums versus successor-state agencies) diverge through exit options - arbitrage versus constrained - because the museum can shed individual objects without dissolving itself, while the state's custody narrative collapses if the collection thesis fails. Coalition dynamics matter for the payer seats: individually outmatched, dispersed community seats gain leverage through transnational claim networks and shared precedents, which the engine should register as coalition potential rather than treating the organized power atom as static.
 *
 * DIRECTIONALITY LOGIC:
 *   Encyclopedic_museums sit nearest the beneficiary pole (d near 0.05): they set the rules and receive the gains, with arbitrage-grade exit letting them shed exposure object by object. Successor_state_agencies are beneficiaries with a twist (d near 0.15): the derived beneficiary position is correct, but their custody narrative locks their own exit, which the engine reads through the constrained exit atom. Heritage_scholarship_sector (d near 0.20) and artifact_market_participants (d near 0.10) collect secondary rents with mobile or arbitrage exit. Source_indigenous_communities sit at the target pole (d near 0.95): full cost-bearing with identity_locked exit, the configuration that maximizes effective extraction. Descendant_diaspora_communities are nearly as exposed (d near 0.85) with weaker standing. Repatriation_advocacy_networks carry a directionality_override (d = 0.45) because the derivation chain cannot see their dual position: they appear only on the stakeholder surface, not in the base beneficiary/victim arrays, so the fallback would misplace them; they are net payers of labor and litigation cost who collect precedent wins, sitting near symmetric with slight target tilt. International_heritage_bodies are analytical observers near d = 0.5, collecting no rents and bearing no costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification blocks two symmetrical mislabelings. Reading the regime as pure rope would let the genuine preservation function absorb the extraction - but custody without continuity forfeits legitimacy under this reading, so the coordination story cannot cover the transfer of sacred and ancestral material. Reading it as pure snare would erase real function: conservation capacity, wartime protection, and the scholarly commons are goods the regime actually delivers, and the founding problem (regularizing custody, halting illicit traffic) is corroborated live by sources outside the benefiting parties. Because the founding problem is live and the arrangement world-rearranging, no zombie flag applies: persistence here is a mix of a still-live coordination problem and concentrated holder gain, which is precisely the mixture the tangled_rope type exists to separate. Mandatrophy risk sits elsewhere - in the token-return dynamic flagged by omega: if selective returns come to serve chiefly as legitimation for the retained bulk, theater_ratio rises and the regime decays toward piton-or-snare territory while its defenders cite the returns as proof of function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates only the indigenous_stewardship_reading of the kernel cultural_property_legal_corpus; which structural facts change under the sibling readings?',
    'Read the sibling constraint stories (cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading): each fixes the same referent - the standing retention arrangement - and re-authors beneficiaries, victims, and epsilon from its own authority criterion.',
    'Under universal_heritage_reading the holding institutions become beneficiaries-by-function and source communities drop to petitioner status, with epsilon falling toward coordination-cost levels; under sovereign_repatriation_reading successor states become beneficiaries and extraction concentrates on imperial-center museums while community seats stay payers. The disagreement is located entirely in the locus-of-authority element of the kernel: the readings agree that objects have custodians and disagree only on whose continuity - cultural, state-historical, or none - confers the right.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Reading-indexed classification of the shared custody-legitimacy kernel.').

omega_variable(
    continuity_adjudication_bias,
    'Who adjudicates whether a claimant maintains sufficient cultural continuity, and does the adjudication procedure systematically favor holders?',
    'Audit affiliation determinations - review-committee decisions, museum documentation requirements - comparing outcomes for oral-tradition versus documentary-tradition claimants, and for communities whose archives were destroyed in the dispossession itself.',
    'Systematic procedural bias means the measured extraction understates the true figure, since the burden of proof is itself an extraction instrument; adjudication robust to disrupted transmission means part of the measured extraction reflects genuinely severed continuity rather than holder obstruction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_adjudication_bias, empirical, 'Whether continuity adjudication is neutral between claimants or tilted toward holders.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (title regimes, export licensing, affiliation bureaucracy, litigation asymmetry) or partly internalized (community absorption of the claim that the objects are safer and better cared for in institutional custody)?',
    'Post-return suppression trajectory: track communities after successful repatriation; if deference to institutional framing and reluctance to handle recovered objects persist after legal barriers fall, the internalized component is real and persistent.',
    'An internalized component raises effective suppression above the structural measure and means it travels with the community after exit, so remedies aimed only at legal machinery will underperform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in custody denial.').

omega_variable(
    sacred_category_width,
    'How wide is the class of artifacts this reading treats as communally inalienable, and does the boundary track community protocols or activist expansion?',
    'Survey community-level protocols distinguishing ceremonially restricted objects, ancestor remains, and records from publicly shareable heritage items, and compare against the categories deployed in repatriation claims.',
    'A wide inalienable category pushes epsilon toward the snare range as nearly all retained material counts as extracted; a narrow category confines extraction to a restricted subclass and leaves the bulk of holdings as ordinary contested custody.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sacred_category_width, conceptual, 'Boundary width of the sacred/communal category driving epsilon.').

omega_variable(
    token_return_laundering,
    'Do partial returns reduce the regime''s net extraction, or do they stabilize it by legitimating the retained bulk?',
    'Compare extraction indicators - claim backlogs, ceremonial-deprivation reports, affiliation-denial rates - before and after high-profile return waves, controlling for total holdings and claim volume.',
    'A laundering dynamic implies the theater_ratio understates performative stewardship and effective extraction exceeds the scalar, pushing the computed type toward snare; a genuine-return dynamic implies the regime is transitioning under pressure rather than entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(token_return_laundering, empirical, 'Whether selective returns relieve or entrench the retention arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(cult_tr_t0, observed).
narrative_ontology:measurement(cult_tr_t6, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(cult_tr_t6, observed).
narrative_ontology:measurement(cult_tr_t12, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement_basis(cult_tr_t12, observed).
narrative_ontology:measurement(cult_tr_t18, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement_basis(cult_tr_t18, observed).
narrative_ontology:measurement(cult_tr_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(cult_tr_t24, observed).
narrative_ontology:measurement(cult_tr_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement_basis(cult_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(cult_be_t0, observed).
narrative_ontology:measurement(cult_be_t6, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 6, 0.67).
narrative_ontology:measurement_basis(cult_be_t6, observed).
narrative_ontology:measurement(cult_be_t12, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 12, 0.72).
narrative_ontology:measurement_basis(cult_be_t12, observed).
narrative_ontology:measurement(cult_be_t18, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 18, 0.77).
narrative_ontology:measurement_basis(cult_be_t18, observed).
narrative_ontology:measurement(cult_be_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 24, 0.82).
narrative_ontology:measurement_basis(cult_be_t24, observed).
narrative_ontology:measurement(cult_be_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 30, 0.86).
narrative_ontology:measurement_basis(cult_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(cult_su_t0, observed).
narrative_ontology:measurement(cult_su_t6, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement_basis(cult_su_t6, observed).
narrative_ontology:measurement(cult_su_t12, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(cult_su_t12, observed).
narrative_ontology:measurement(cult_su_t18, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 18, 0.64).
narrative_ontology:measurement_basis(cult_su_t18, observed).
narrative_ontology:measurement(cult_su_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement_basis(cult_su_t24, observed).
narrative_ontology:measurement(cult_su_t30, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(cult_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, resource_allocation).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who owns cultural artifacts' decomposes into three structurally distinct constraints sharing one kernel: the custody-legitimacy question admits rival authority criteria (cultural continuity / state-historical continuity / preservation-access maximization), and each yields a different beneficiary-victim topology over the same standing arrangement. This story carries the highest epsilon of the family under the expected delta: artifacts are held by parties this reading credits with no legitimate claim, so both holding institutions and successor states land on the extraction side while source communities take the full target position. The upstream/downstream structure runs through shared evidence: provenance research and return precedents produced under one reading reshape the operating environment of the others, so the family members are linked bidirectionally through network edges rather than averaged into one epsilon-bearing label.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
