% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Cultural Property as Indigenous Sacred/Communal Property (Stewardship Reading)
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This is ONE reading of the contested cultural property kernel. The
 *   indigenous stewardship reading holds that artifacts sacred or central to
 *   indigenous cultural continuity are communal property whose legitimate
 *   authority rests with the communities maintaining that continuity, not
 *   with colonial successor states, Western museums, or universal-heritage
 *   doctrines. Under this reading, holding institutions (museums) and
 *   successor states are the extractors — they control artifacts to which
 *   indigenous communities have legitimate claims, deny communities access
 *   and authority, and justify the denial through doctrines (universal
 *   heritage, preservation expertise, acquisition legality) authored by
 *   colonial powers. The constraint's ε is high because indigenous
 *   communities cannot reclaim, control, or use artifacts central to their
 *   cultural identity, and the extraction is maintained through legal and
 *   institutional suppression. The measurement series runs 1950–2024 to
 *   capture the period from post-colonial state formation through the UNDRIP
 *   era.
 *
 * KEY AGENTS:
 *   - Indigenous communities: Beneficiaries under this reading (have legitimate authority); trapped victims (artifacts held by others, reclamation costs prohibitive)
 *   - Western museums: Agenda-setter (control artifacts, resist repatriation, frame doctrines); powerful institutional actor
 *   - Successor states: Institutional actors claiming sovereignty but often deferring to museums; constrained by diplomatic and economic relationships with Western powers
 *   - Indigenous advocacy networks: Organized but resource-constrained; face legal and institutional barriers to claims
 *   - International law / UNDRIP: Observer/advisory; produces declarations but lacks enforcement mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.89).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.78).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, snare).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Cultural Property as Indigenous Sacred/Communal Property (Stewardship Reading)").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '2ff2ebd8-a967-49f2-acc2-3341b249d1cf').
narrative_ontology:cs_kernel_codification('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', fixed_text).
narrative_ontology:cs_authority_grounding('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', extraction).
narrative_ontology:cs_interpretation_layer_present('2ff2ebd8-a967-49f2-acc2-3341b249d1cf').
narrative_ontology:cs_reading_relation('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', cultural_property_legal_corpus__universal_heritage_reading, forecloses).
narrative_ontology:cs_reading_relation('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_axiom('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', foundational, indigenous_community_sovereignty_over_cultural_patrimony).
narrative_ontology:cs_axiom_status(indigenous_community_sovereignty_over_cultural_patrimony, holdable).
narrative_ontology:cs_axiom_grounding('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', indigenous_community_sovereignty_over_cultural_patrimony, deontological).
narrative_ontology:cs_axiom('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', foundational, cultural_continuity_as_legitimate_authority_source).
narrative_ontology:cs_axiom_status(cultural_continuity_as_legitimate_authority_source, holdable).
narrative_ontology:cs_axiom_grounding('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', cultural_continuity_as_legitimate_authority_source, deontological).
narrative_ontology:cs_axiom('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', secondary, colonial_acquisition_as_illegitimate_extraction).
narrative_ontology:cs_axiom_status(colonial_acquisition_as_illegitimate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', colonial_acquisition_as_illegitimate_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', indigenous_stewardship_and_control).
narrative_ontology:cs_drift_state('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', contemporary_museum_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('2ff2ebd8-a967-49f2-acc2-3341b249d1cf', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, conservation_professionals).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_advocacy_networks).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, source_nation_governments).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_continuity_as_legitimacy_source).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, sacred_trust_doctrine).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, community_sovereignty_over_cultural_patrimony).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold legitimate authority under this reading to determine disposition of artifacts sacred or central to their cultural continuity. Their artifacts remain held by foreign museums and successor-state institutions. They cannot access these artifacts for ceremonial use, education, or community-based study without permission from holding institutions. They lack resources to fund repatriation claims or operate professional conservation facilities. Reclamation is possible but politically and financially costly; abandoning the claim forecloses cultural identity.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, beneficiary,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, payer).

% Hold large collections of non-Western cultural artifacts, primarily acquired during colonial periods. Justify retention through conservation expertise, global scientific access, preservation against loss or destruction in communities lacking professional infrastructure, and framing as universal heritage serving humanity. Actively resist repatriation through legal arguments, acquisition legitimacy claims, and institutional prestige. Control physical access, scholarly interpretation, and curatorial narratives. Increasingly adopt tokenistic gestures toward community consultation while retaining decision-making authority.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, western_museums, agenda_setter,
    institutional, generational, arbitrage, global).

% Claim sovereign ownership of artifacts as successor states to colonial administrations. Often lack enforcement capacity to compel repatriation from Western museums due to diplomatic and economic dependencies. Some operate national museums that themselves hold artifacts and resist claims from both international bodies and indigenous communities. Positions range from supporting community stewardship to competing with communities for state-level patrimony control. Often defer to museum expertise and international legal precedent.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states, agenda_setter,
    institutional, generational, constrained, national).

% Coordinate transnational repatriation claims, document sacred significance of artifacts, build international consensus for indigenous stewardship, and mobilize public pressure. Operate without formal enforcement mechanisms; depend entirely on moral suasion, legal argument, and diplomatic pressure. Many claims are rejected on technical grounds (incomplete documentation, contested proof of origin, perceived inability of communities to provide professional preservation). Face sustained institutional resistance from museums and legal barriers in Western property law.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_advocacy_networks, payer,
    organized, generational, constrained, global).

% Modern governments of territories where artifacts originated face competing domestic pressures: nationalist narratives of reclaiming stolen property, diplomatic relationships with Western states and museums, archaeological access dependent on museum cooperation, and (under this reading) potential tension with indigenous communities' superior authority claims. Lack enforcement capacity to compel Western museums. Often settle through negotiation or threaten restrictions on archaeological access in their territory.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, source_nation_governments, payer,
    moderate, generational, constrained, national).

% UNESCO, UNDRIP, and international courts theoretically adjudicate claims. Produce declarations (UNESCO Return of Cultural Property, UNDRIP Chapter 31 on indigenous self-determination) that are aspirational rather than binding. Lack enforcement mechanisms to override institutional autonomy of museums or sovereign immunity of states. Operate through consensus, which favors the status quo against repatriation. Some bodies increasingly recognize indigenous authority in principle while maintaining institutional deference to museums in practice.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% Provide and justify expert preservation services that museums claim justify retention. Their authority depends on recognizing museums as legitimate holders. Professional ethics increasingly incorporate community consultation, but decision-making authority over preservation methods and access remains with conservation institutions. Their expertise is both a genuine public good and a mechanism through which institutional authority over artifacts is legitimized and maintained.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, conservation_professionals, beneficiary,
    institutional, generational, constrained, global).

% The doctrines of discovery, salvage, universal heritage, and finders-keepers remain embedded in international law, museum bylaws, property law of Western states, and academic fields (archaeology, art history, conservation). These doctrines were authored by colonial powers and remain the default frame in which repatriation claims are evaluated. They are not the explicit subject of dispute — the dispute is whether they apply, whether they are legitimate, and who should have authority to override them.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_legal_doctrine, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_legal_doctrine).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__indigenous_stewardship_reading, western_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__indigenous_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the constraint does not solve a coordination problem. Instead, it is the mechanism by which authority over artifacts is extracted from legitimate custodians (indigenous communities) and concentrated in holding institutions. Museums frame it as coordinating preservation + access; this reading reframes it as coordinating institutional control despite lack of legitimate authority.
% TRANSFER_FUNCTION: Transfers decision-making authority over artifacts from indigenous communities (legitimate custodians) to museums (institutional holders) and successor states (sovereignty claimants). Transfers scholarly authority to conservation professionals and academic disciplines. Transfers cultural meaning from sacred/communal context to specimen/aesthetic/scientific context. Transfers access privileges from communities to paying international visitors and credentialed researchers.
% ABSENT_VOICES: Communities whose continuity was so disrupted that they cannot prove unbroken cultural transmission and thus lose claims under documentary-evidence gates; indigenous populations within successor states whose governments do not represent them; practitioners of other legitimate preservation models (community-based oral traditions, sacred-restricted-access protocols, ceremonial-context preservation) whose methods are not recognized by Western conservation standards; descendant communities of artifact creators who were colonized by peoples the holding state claims to represent.
% DISAPPEARANCE_RATIONALE: If the indigenous stewardship reading were implemented and enforcement mechanisms were created to recognize community authority, the global museum landscape would reorganize: major collections would be repatriated, museums would shrink or pivot to co-curated models and source-region partnerships, academic fields would reorient toward community-based research and knowledge transmission, and international law would enshrine indigenous sovereignty over cultural patrimony. This is not a marginal adjustment but a fundamental realignment of authority and institutional power.
% FOUNDING_PROBLEM: During colonialism, military conquest and administrative control enabled expropriation of non-Western artifacts. Institutions in colonial metropoles (museums, universities, collectors) acquired artifacts through military violence, forced sale, or looting. Communities lost access to sacred objects, ceremonial knowledge dependent on those objects, and authority to determine who studied or displayed them. This founding problem persists today: artifacts remain held by institutions the communities did not authorize, communities still cannot control disposition, and international law still defaults to treating colonial acquisition as legitimate.
% FOUNDING_PROBLEM_CORROBORATION: Documented by historical scholarship (Coll 'How the West Stole Our History,' Appiah 'Cosmopolitanism,' Sarr & Savoy 'The Return of African Cultural Heritage'), by indigenous advocacy organizations and governments (First Nations of Canada, Māori authorities, Aboriginal Australian organizations), by legal scholars advancing repatriation doctrine (Merryman, Nafziger), and by international human-rights bodies (UN Declaration on the Rights of Indigenous Peoples, adopted 2007 by UN General Assembly, endorsed by most nations including founding-era Western powers post-endorsement). Corroboration comes from sources outside the museum-beneficiary set: successor-state governments, international human-rights institutions, and sustained indigenous advocacy across 50+ years.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is very high (0.89 at 2024) because the core claim is that artifacts are held by unauthorized parties and communities cannot reclaim them — this is maximal structural extraction from the reading's own premises. Suppression is high (0.78) because museums use legal doctrines, acquisition legitimacy arguments, and conservation expertise claims to suppress community claims; source nations lack enforcement capacity; and international law produces aspirational rather than binding remedies. Theater ratio increases over the interval (0.15 to 0.42) because museums have increasingly adopted rhetoric of community engagement, repatriation consideration, and decolonization narratives while maintaining physical control of most collections — performative acknowledgment of indigenous authority without substantive transfer. Accessibility collapse is moderate-high (0.72) because communities could theoretically exit the claim (accept the constraint), but doing so requires abandoning legitimate authority and cultural identity — the exit cost is identity foreclosure. Resistance is substantial (0.68) because indigenous movements have mounted decades of claims, generated scholarly corroboration, secured UNDRIP endorsements, and won some repatriation cases, indicating real resistance to the extractive framing.
 *
 * PERSPECTIVAL GAP:
 *   The divergence between seats is extreme and structural. From indigenous communities' perspective, this is a snare: they cannot reclaim artifacts, their authority is denied, and suppression is both legal (property doctrine) and internalized (many communities accept museum framing that preservation requires expert Western conservation). From museums' perspective, the constraint is rope: they provide genuine preservation, enable global access, and serve scholarship — the cooperation they seek is scientific study and public education, which they claim benefits all parties. From successor states' perspective, it varies: some frame artifacts as national patrimony (sovereign repatriation reading) and sometimes partner with museums; others support indigenous claims but lack enforcement capacity. The engine computes these divergences from the structural data; this reading's claim-metric independence is intentional — high extractiveness under an indigenous stewardship frame coexists with rope-framing from the beneficiary institutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities are trapped victims from whom the constraint extracts authority and access (d near 1.0 — full targets). Museums are institutional beneficiaries who control narratives and resources (d near 0.0 — full beneficiaries from the extraction, though they may perceive themselves as coordinating). Successor states are positioned asymmetrically: they claim sovereignty (medium beneficiary position, d~0.3) but often lack enforcement capacity to rival museums (constrained, not arbitrage exit). Advocacy networks are organized but powerless, bearing costs of organizing without capture of the extraction (d~0.8, high targets). The directionality derives from beneficiary/victim declarations: indigenous communities are both (they should benefit from the constraint, but the current operation extracts from them), which surfaces the snare structure — a constraint that claims to protect a community's interests while systematically denying them control.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — colonial expropriation and continued unauthorized holding — remains live. Under the indigenous stewardship reading, the constraint's mandate is to protect indigenous control and cultural continuity; the operation persists in denying that control. This is mandatrophy: the arrangement outlives its stated purpose. Museums frame the mandate as conservation and universal access, which they claim they fulfill well. The tension between these mandates is exactly where the kernel reading divergence sits — who gets to define what the constraint is for. The snare classification reflects the indigenous stewardship reading's structural claim: authority claimed by communities is held by institutions not subject to community authority, and the arrangement is defended against repatriation through legal and rhetorical suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_continuity_proof,
    'What evidence suffices to establish that a community maintains genuine cultural continuity with ancestors who created/used an artifact? Where colonialization disrupted that continuity, does a community claiming stewardship need unbroken tradition or demonstrated effort to reconnect?',
    'Case-by-case adjudication by indigenous-majority bodies (not Western museums or courts) applying community-defined standards of authenticity and continuity. Document outcomes across diverse claims.',
    'If communities'' own standards become the evidentiary gate, more repatriation claims will succeed. If Western institutional standards (documentation, provenance, current practice alignment) remain gating, many claims will fail on technical grounds despite the stewardship reading''s normative validity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_continuity_proof, conceptual, 'Who defines and evaluates cultural continuity as the basis for stewardship authority.').

omega_variable(
    preservation_vs_access_tradeoff,
    'Is Western museum preservation (climate-controlled, professionally managed, inaccessible except by scholarly permission) structurally incompatible with indigenous stewardship (community ritual use, knowledge transmission, sacred handling protocols)? Or can hybrid models (co-curation, ceremonial access, community-informed conservation) reconcile both?',
    'Empirical observation of repatriation cases and co-curated models; outcomes measured on community satisfaction with preservation status, access for ritual/education, and decision-making authority.',
    'If hybrid models succeed, some extraction is resolved without full repatriation. If incompatibility is deep, then effective stewardship requires physical return, sharpening the snare classification and making the constraint harder to negotiate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_vs_access_tradeoff, empirical, 'Whether preservation and stewardship can coexist or require exclusivity.').

omega_variable(
    suppression_internalization_mechanism,
    'Is measured suppression (0.78) structural (museum control, legal barriers, cost of repatriation claims) or internalized (communities accept doctrines of expertise, accept that Western preservation is better, defer to state authority rather than asserting community authority)?',
    'Ethnographic and narrative evidence from communities: post-repatriation trajectories in cases where artifacts are returned; whether suppression persists after the structural barriers are removed; identity-fusion patterns in communities'' relationship to holding institutions.',
    'If suppression is largely internalized, the constraint''s effective extraction is higher than the structural measure — communities carry the suppression with them even if physical barriers fall. If structural, opening access may shift the suppression requirement substantially downward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Structural vs. internalized suppression in indigenous community claims-making.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the indigenous stewardship reading logically foreclose either the universal_heritage_reading or the sovereign_repatriation_reading, or do the readings coexist as live normative positions held by different institutional actors?',
    'Logical analysis: does stewardship authority vested in communities REQUIRE that artifacts not be universal heritage, or is universal access compatible with community authority? Does it REQUIRE that successor states have no claim, or can states support community stewardship?',
    'If readings foreclose each other, one reading is the correct frame and alternatives are inconsistent. If coexistent, all three are live positions and the constraint corpus models a genuine multi-reading kernel rather than a solved dispute.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical structure of the kernel''s sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1950, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement_basis(cult_tr_t1950, observed).
narrative_ontology:measurement(cult_tr_t1975, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1975, 0.22).
narrative_ontology:measurement_basis(cult_tr_t1975, observed).
narrative_ontology:measurement(cult_tr_t1995, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 1995, 0.31).
narrative_ontology:measurement_basis(cult_tr_t1995, observed).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement_basis(cult_tr_t2010, observed).
narrative_ontology:measurement(cult_tr_t2018, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2018, 0.41).
narrative_ontology:measurement_basis(cult_tr_t2018, observed).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(cult_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(cult_be_t1950, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1950, 0.92).
narrative_ontology:measurement_basis(cult_be_t1950, observed).
narrative_ontology:measurement(cult_be_t1975, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1975, 0.88).
narrative_ontology:measurement_basis(cult_be_t1975, observed).
narrative_ontology:measurement(cult_be_t1995, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 1995, 0.85).
narrative_ontology:measurement_basis(cult_be_t1995, observed).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2010, 0.87).
narrative_ontology:measurement_basis(cult_be_t2010, observed).
narrative_ontology:measurement(cult_be_t2018, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2018, 0.88).
narrative_ontology:measurement_basis(cult_be_t2018, observed).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 2024, 0.89).
narrative_ontology:measurement_basis(cult_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1950, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement_basis(cult_su_t1950, observed).
narrative_ontology:measurement(cult_su_t1975, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1975, 0.68).
narrative_ontology:measurement_basis(cult_su_t1975, observed).
narrative_ontology:measurement(cult_su_t1995, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement_basis(cult_su_t1995, observed).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement_basis(cult_su_t2010, observed).
narrative_ontology:measurement(cult_su_t2018, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2018, 0.77).
narrative_ontology:measurement_basis(cult_su_t2018, observed).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 2024, 0.78).
narrative_ontology:measurement_basis(cult_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.12).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).

% DUAL FORMULATION NOTE:
% The cultural_property_legal_corpus kernel instantiates three structurally distinct constraints depending on which reading of legitimate authority is adopted. This file models the indigenous_stewardship_reading: authority rests with communities maintaining cultural continuity. The universal_heritage_reading vests authority in global access and preservation; the sovereign_repatriation_reading vests authority in successor states. All three share the same artifacts as the referent but assign authority (and thus extraction, beneficiaries, victims) differently. They are linked via network.affects_constraints because each reading's success would alter the operating environment of the others — if stewardship authority is recognized, universal-access claims and state-sovereignty claims face legitimacy pressure. The readings coexist as live normative positions in contemporary dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
