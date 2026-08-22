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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   human_readable: Sovereign State Repatriation of Colonial-Era Cultural Property
 *   domain: international_law/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint embodies the sovereign repatriation reading of the
 *   cultural property kernel: the legal and moral claim that cultural
 *   artifacts expropriated during colonialism are the sovereign property of
 *   successor states, that colonial acquisition was illegitimate extraction,
 *   and that legitimate authority to decide their disposition rests with
 *   states claiming historical continuity with expropriated peoples. The
 *   constraint operates as an active enforcement structure — repatriation
 *   claims, international legal precedent, and diplomatic pressure from
 *   successor states — that extracts custody, prestige, and cultural
 *   authority from holding institutions (museums, universities, national
 *   collections in former colonial powers). The reading is contested by
 *   sibling readings that prioritize indigenous community stewardship or
 *   universal access over state sovereignty. The claim/metric gap is
 *   intentional: the author claims Tangled Rope (genuine coordination problem
 *   — who holds title to expropriated artifacts? — plus asymmetric extraction
 *   — successor states benefit from repatriation, holding institutions bear
 *   costs). The metrics describe moderately extractive operation (0.58
 *   extractiveness at interval end) because repatriation does restore
 *   symbolic capital and material sovereignty while imposing real costs
 *   (logistics, legal disputes, donor complications) on holding institutions.
 *
 * KEY AGENTS:
 *   - Successor states claiming historical continuity — beneficiary, agenda-setter, institutional power, civilizational horizon
 *   - Holding institutions in former colonial powers — payer, institutional power, constrained exit
 *   - Indigenous communities within successor states — excluded from this reading's authority framework, trapped exit
 *   - International legal bodies (UNESCO, courts, UNIDROIT) — observers, analytical seats, generational horizon
 *   - Universal access advocates and scholars — excluded from the sovereign reading, would prioritize research access and fragmentary preservation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.58).
domain_priors:suppression_score(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.42).
domain_priors:theater_ratio(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__sovereign_repatriation_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__sovereign_repatriation_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__sovereign_repatriation_reading, "Sovereign State Repatriation of Colonial-Era Cultural Property").
narrative_ontology:topic_domain(cultural_property_legal_corpus__sovereign_repatriation_reading, "international_law/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__sovereign_repatriation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__sovereign_repatriation_reading, '43ec516c-675b-461a-a3c3-978a1a636474').
narrative_ontology:cs_kernel_codification('43ec516c-675b-461a-a3c3-978a1a636474', fixed_text).
narrative_ontology:cs_authority_grounding('43ec516c-675b-461a-a3c3-978a1a636474', lineage).
narrative_ontology:cs_interpretation_layer_present('43ec516c-675b-461a-a3c3-978a1a636474').
narrative_ontology:cs_reading_relation('43ec516c-675b-461a-a3c3-978a1a636474', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_reading_relation('43ec516c-675b-461a-a3c3-978a1a636474', cultural_property_legal_corpus__universal_heritage_reading, coexists_with).
narrative_ontology:cs_axiom('43ec516c-675b-461a-a3c3-978a1a636474', foundational, successor_state_territorial_sovereignty).
narrative_ontology:cs_axiom_status(successor_state_territorial_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('43ec516c-675b-461a-a3c3-978a1a636474', successor_state_territorial_sovereignty, deontological).
narrative_ontology:cs_axiom('43ec516c-675b-461a-a3c3-978a1a636474', foundational, colonial_expropriation_illegitimate).
narrative_ontology:cs_axiom_status(colonial_expropriation_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('43ec516c-675b-461a-a3c3-978a1a636474', colonial_expropriation_illegitimate, deontological).
narrative_ontology:cs_axiom('43ec516c-675b-461a-a3c3-978a1a636474', secondary, repatriation_restores_justice).
narrative_ontology:cs_axiom_status(repatriation_restores_justice, holdable).
narrative_ontology:cs_axiom_grounding('43ec516c-675b-461a-a3c3-978a1a636474', repatriation_restores_justice, conventional).
narrative_ontology:cs_reference_frame('43ec516c-675b-461a-a3c3-978a1a636474', post_colonial_territorial_sovereignty_framework).
narrative_ontology:cs_drift_state('43ec516c-675b-461a-a3c3-978a1a636474', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('43ec516c-675b-461a-a3c3-978a1a636474', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states_claiming_continuity).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions_with_colonial_collections).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__sovereign_repatriation_reading, donor_base_of_holding_institutions).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__sovereign_repatriation_reading, donor_base_of_holding_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Successor states to colonial territories assert legal sovereignty over cultural artifacts removed during imperial occupation. They set the repatriation agenda through diplomatic claims, international legal petitions, and legislative action in their own jurisdictions. They benefit from restored symbolic capital, recovered national patrimony, and restored connection to cultural heritage. Costs are diplomatic friction with holding institutions and litigation expenses. Exit options include escalation (unilateral seizure, sanctions, international court action) or negotiation (bilateral repatriation agreements, long-term loans). Their time horizon is civilizational because national patrimony and historical justice claims operate on generational timescales.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states_claiming_continuity, agenda_setter,
    institutional, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states_claiming_continuity, beneficiary).

% Museums, universities, and cultural institutions in former colonial powers hold collections acquired through colonial-era expeditions and transactions. Under the sovereign repatriation reading, they are payers: they must defend legal title against repatriation claims, fund repatriation logistics and conservation during transfer, accept loss of collection prestige and research potential, and navigate donor agreements and institutional bylaws that may restrict or prohibit repatriation. Constrained exit reflects institutional inertia, endowment restrictions tied to collections, donor agreements with acquisition conditions, professional communities whose prestige is tied to holding rare pieces, and lack of alternative institutional models for responding to repatriation. Their biographical horizon reflects that individual institution leadership makes repatriation decisions; the institution's collection mission may predate current leadership.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, holding_institutions_with_colonial_collections, payer,
    institutional, biographical, constrained, global).

% Indigenous communities whose ancestors created the artifacts that are now subject to repatriation claims are structurally excluded from this reading's authority framework. The sovereign repatriation reading locates authority with the successor STATE, which may or may not represent indigenous communities faithfully or grant them decision-making power over repatriated artifacts. Indigenous voices that contest state ownership or demand community stewardship (the core of the indigenous_stewardship_reading) are outside the conversation this reading sets up. They are trapped: they cannot easily exit the state system or the territorial claim to their ancestors' artifacts. Their civilizational time horizon reflects that cultural continuity and spiritual responsibility operate across generations. Excluded from the agenda means they lack formal voice in how repatriated artifacts are used, stored, accessed, or interpreted.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, indigenous_communities_within_successor_states, excluded,
    moderate, civilizational, trapped, global).

% Scholars, curators, cultural researchers, and publics who value broad research access and universal exhibition are structurally excluded from the sovereign repatriation reading's authority framework. They would argue that repatriation to state vaults or restricted community control might fragment knowledge, restrict research access, and harm preservation through institutional fragmentation. Their objections are not seated at the table where this reading adjudicates legitimate authority — the reading prioritizes state sovereignty and historical restitution, not access optimization. Constrained exit reflects that they can pursue open-access scholarship and digital archiving as alternatives, but they cannot easily prevent repatriation or enforce universal access norms once repatriation is institutionalized. Their biographical horizon reflects that research careers and institutional practices operate on decade-scale timescales.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, universal_access_advocates, excluded,
    organized, biographical, constrained, global).

% UNESCO, UNIDROIT, regional human rights courts, and national courts are analytical observers reviewing competing repatriation claims and setting legal precedent. They do not directly benefit from or pay the constraint; they adjudicate the framework. Their role is to examine whether successor state sovereignty is the correct legal ground, whether international treaties support it, and whether repatriation obligations can be squared with other legal and ethical commitments (donor rights, institutional mission, universal access). Their generational horizon reflects that legal precedent and international treaty development operate on 20-50 year timescales. Analytical exit reflects that observers can refrain from adjudication or decline to recognize a reading's legitimacy, though the practical consequences of non-recognition are limited.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, international_legal_bodies, observer,
    institutional, generational, analytical, global).

% Collectors, collector families, and philanthropic foundations that donated or endowed collections tied to specific artifacts face reputational and material complications if repatriation proceeds. They may face criticism for having collected colonial-era pieces (legacy cost); they also benefit from the cultural prestige, tax deductions, and institutional naming rights that donations provide. Some donor constituencies pressure institutions to resist repatriation claims; others support repatriation on moral grounds. Mobile exit is available (redirect endowments to institutions or countries with different repatriation policies) but constrained by emotional investment in legacy institutions and by reputational considerations. Their biographical horizon reflects that major donors' philanthropic commitments operate on their lifetime timescale.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__sovereign_repatriation_reading, donor_base_of_holding_institutions, payer,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__sovereign_repatriation_reading, donor_base_of_holding_institutions, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__sovereign_repatriation_reading, successor_states_claiming_continuity).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__sovereign_repatriation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal and moral framework for recognizing successor state sovereignty over cultural artifacts expropriated during colonialism. Solves the coordination problem: which entity has the right to decide an artifact's location, interpretation, and disposition — the entity that currently holds it (the holding institution), the entity that originally possessed it (now expropriated), or the entity that now claims historical continuity with the expropriated possession? The sovereign repatriation reading coordinates on: successor states hold legitimate title by virtue of historical continuity, territorial succession, and the illegitimacy of colonial acquisition. This unifies what would otherwise remain contested and fragmented.
% TRANSFER_FUNCTION: Transfers custody, legal title, and symbolic capital (cultural authority, prestige as repository of national heritage) from holding institutions to successor states. Secondary transfers include: research access moves from universal to restricted (state-controlled or community-controlled); interpretation authority moves from holding institution curators to state/community representatives; prestige moves from 'world museum' to 'recovered patrimony.' Money flows through repatriation logistics, conservation during transfer, sometimes compensation for surrendered items, and dispute resolution (legal costs).
% ABSENT_VOICES: Indigenous communities whose ancestors created the artifacts are excluded from the sovereign repatriation reading's authority framework. They would object that the reading appropriates their cultural capital for state nationalism rather than granting communities decision-making authority and stewardship responsibility. Communities would argue that repatriation should go to them, not to states that may not represent their interests or grant them control. Universal access advocates and research communities are excluded; they would argue that repatriation fragments knowledge and restricts the research and educational value of artifacts. They would propose alternatives: shared governance, digital access, long-term loans, and partnership models that preserve both sovereignty restoration and universal access. The exclusion of these voices means the constraint's legitimacy is contested by real institutional actors who cannot defend their position within this reading's framework.
% DISAPPEARANCE_RATIONALE: If the sovereign repatriation reading and its enforcement mechanisms disappeared overnight: (1) holding institutions would retain colonial-era collections indefinitely, with no internationally recognized legal obligation to repatriate; (2) successor states would lose the primary diplomatic and legal framework for asserting repatriation claims (they could still claim, but without institutional support); (3) the symbolic reversal of colonial expropriation would not occur — the material and narrative order established by colonialism would persist; (4) indigenous communities would remain excluded from both holding institutions and from state-level repatriation decisions (no alternative framework would emerge automatically); (5) the global distribution of cultural artifacts would stabilize in holding institutions, reproducing the colonial-era asymmetry. The world would rearrange: successor states would face political pressure to develop alternative repatriation frameworks (perhaps grounded in indigenous rights or reparations law), or they would accept permanent loss of national patrimony and the symbolic cost thereof. Holding institutions would face reputational but not legal pressure. Research access would remain centralized. The constraint's disappearance would reset the baseline to pre-1970s conditions (before UNESCO conventions on repatriation began to emerge).
% FOUNDING_PROBLEM: During the colonial era (15th-20th centuries), European and other imperial powers expropriated cultural artifacts from colonized territories through military occupation, unequal exchange, confiscation, and acquisition under duress or without informed consent. After decolonization (1945 onward), successor states asserted sovereignty over their territories but discovered vast collections of their cultural patrimony held in foreign institutions in former colonial powers. These institutions claimed lawful possession under title they had asserted during colonialism. The founding problem: by what legal, moral, and political framework do successor states recover artifacts from institutions that claim lawful possession under colonial-era acquisition? Whose sovereign right applies — the current possessor or the successor to the original possessor?
% FOUNDING_PROBLEM_CORROBORATION: Multiple corroborating sources from outside the benefiting parties confirm the founding problem is live and ongoing: (1) Successor states themselves (Nigeria, Benin, Egypt, Greece, Peru, and dozens of others) maintain active repatriation campaigns and international appeals, indicating the problem is not resolved for them; (2) International legal scholarship on cultural property and post-colonial restitution (academic literature from 1980s onward, growing substantially after 2000) documents the founding problem as a live dispute with real institutional stakes; (3) UNESCO conventions (1954 Hague Convention, 1970 Convention on the Means of Prohibiting and Preventing Illicit Import, Export and Transfer of Ownership of Cultural Property) treat repatriation as an open legal and diplomatic problem; (4) Holding institutions publicly acknowledge repatriation pressure, indicating they experience it as a real enforcement problem, not a historical grievance; (5) Media coverage, litigation records, and political disputes over specific artifacts (British Museum's holdings from Egypt, Parthenon Marbles, Benin Bronzes, Nazi-looted art, indigenous artifacts) confirm the founding problem is actively contested, not resolved. Independent scholars and international bodies confirm that the colonial acquisition of these artifacts was materially coercive and that the question of rightful restitution remains genuinely contested (no consensus has emerged on whether sovereign repatriation, indigenous stewardship, or universal heritage should govern).
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__sovereign_repatriation_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__sovereign_repatriation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__sovereign_repatriation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate (0.58) because the constraint extracts custody and prestige from holding institutions but does so in service of restoring historic injustice, not pure rent-seeking. The extraction is partial: holding institutions can negotiate terms, some pieces remain unrepatriated, and the legal framework is still evolving. Suppression is lower than typical snare (0.42) because the constraint operates through open diplomatic and legal channels, not hidden coercion — successor states make public claims, holding institutions publicly resist or negotiate, courts adjudicate. The asymmetry is clear: successor states benefit from repatriation (symbolic restoration, cultural authority, national patrimony recovery); holding institutions bear costs (collection reduction, prestige loss, donor complications). Theater (0.28) reflects partial performativity: some repatriation ceremonies and institutional policy shifts are theatrical displays of commitment to the sovereignty principle, while real material transfer and custody change are genuine. The measurement series shows extractiveness rising from 1945 (immediate post-colonial, repatriation reading barely articulated) through 2026 (reading is normalized in law and diplomacy), while suppression falls (early colonialism required violent suppression of indigenous claims; today's repatriation happens through negotiation, courts, and precedent, not force). This inverse relationship is characteristic of a constraint shifting from coercive to institutionalized enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The successor state seat experiences the constraint as restitution and sovereignty restoration — the agenda-setter seat. The holding institution seat experiences it as loss of collection, prestige extraction, and enforced divestment — the payer seat. An international legal observer sees it as a new institutional framework for resolving title disputes, not as extractive at all. Indigenous communities (excluded from this reading) would experience it as state appropriation of their cultural capital — the state benefits from repatriation without granting communities decision-making authority. The engine will compute different types from different seats: the agenda-setter (successor state) might see rope-like coordination; the payer (holding institution) sees snare-like extraction; the observer (court) sees institutional scaffolding. The author's claim (Tangled Rope) reflects the actual structure: genuine coordination problem (who holds title?) plus asymmetric extraction (states benefit, institutions pay).
 *
 * DIRECTIONALITY LOGIC:
 *   Successor states are beneficiaries (low d, near 0.0) — they receive restored sovereignty, cultural authority, and symbolic restitution; they are the agenda-setters; they have mobile exit (they can escalate or negotiate). Holding institutions are targets/payers (high d, near 1.0) — they lose collection prestige and must fund repatriation; they have constrained exit (institutional bylaws, donor agreements, endowment structures lock them into collections). The asymmetry is reinforced by power (both institutional) but inverted by exit options: the state is mobile (can change strategy, escalate, negotiate), the institution is trapped in its own formal structure. Indigenous communities (excluded) would have high d if included (trapped, moderate power, but the reading denies them authority despite victimhood — a different extraction logic than the measured one). Universal access advocates (excluded) would have moderate d (their interests are overridden but they have exit through other venues — scholarship, open-access publishing, digital archiving). The directionality derivation from the structural data alone produces the right d for the two main seats (state near beneficiary, institution near target) without overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial expropriation of cultural artifacts) is live and contentious; the sovereign repatriation reading claims the problem is still present and unsolved because artifacts remain expropriated (held by foreign institutions without consent). This prevents mandatrophy: the arrangement is not persisting because its founding problem died and the arrangement doesn't know it. Instead, repatriation enforcement is rising (measured extractiveness increases over the interval) because the founding problem remains live and successor states are gaining diplomatic and legal power to enforce claims. However, the measure shows theater rising faster than extractiveness (theater_ratio grows from 0.05 to 0.28 while extractiveness grows from 0.15 to 0.58) — some repatriation is becoming ceremonial (symbolic restoration without material transfer, public commitments without full enforcement, donor-negotiated compromises). This rising theater is a mandatrophy signal worth tracking: if theater continues to rise and extractiveness plateaus, the constraint could degrade into performative sovereignty restoration while actual custody remains unchanged — a piton state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigenous_community_displacement_ambiguity,
    'Does successor state repatriation serve indigenous communities'' actual interests in cultural stewardship and spiritual responsibility, or does it appropriate indigenous cultural capital for state nationalism, displacing community authority with state authority?',
    'Longitudinal study of indigenous community experience post-repatriation: interviews, documentation of community access and decision-making influence over repatriated artifacts, comparison of community-controlled vs. state-controlled repatriated collections. Evidence of community benefit (spiritual practice resumption, educational control, decision-making authority) vs. appropriation (symbolic use by state, community exclusion from disposition decisions, reduced community access).',
    'If appropriation is confirmed, the constraint is partly extractive from indigenous communities (even as it extracts from holding institutions) — the beneficiary set is narrower than claimed (state, not community), and the victim set includes both holding institutions AND indigenous communities. This would lower the benignity of the repatriation claim and suggest a reading decomposition into separate community-stewardship and state-sovereignty constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_community_displacement_ambiguity, empirical, 'Whether successor state repatriation centers community stewardship or appropriates indigenous cultural capital for state nationalism.').

omega_variable(
    universal_heritage_vs_sovereign_stewardship_tradeoff,
    'Is centralized preservation and research access in holding institutions structurally incompatible with successor state custody and local stewardship, or can both be achieved through shared governance, digital access, and loan agreements?',
    'Comparative analysis of repatriation models: unilateral transfer (state takes custody, holding institutions lose access), partnership models (shared governance, long-term loans, digitization agreements), and hybrid arrangements. Outcome metrics: artifact preservation quality, community access, research continuity, knowledge fragmentation, dispute rates.',
    'If incompatibility is confirmed, repatriation requires genuine sacrifice of universal access for sovereignty — the extraction is real and substantial. If compatibility is achievable, the constraint can be redesigned to reduce extraction (shared governance reduces the zero-sum character) and move toward Rope-like coordination. The measurement of extractiveness depends on which model is actually implemented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_vs_sovereign_stewardship_tradeoff, conceptual, 'Whether state custody and universal access are fundamentally opposed or can coexist through governance redesign.').

omega_variable(
    colonial_title_legitimacy_ambiguity,
    'On what grounds is the colonial acquisition deemed illegitimate? Is it the process (force, absence of consent), the outcome (expropriation), the context (violation of indigenous property rights), or the theoretical justification (colonizers had no right to rule)?',
    'Legal and philosophical analysis: which theory of title (labor, conquest, consent, occupation) is invoked to judge colonial acquisition illegitimate? What makes successor state claims legitimate in the same framework? If different standards apply to colonial and post-colonial acquisition, does the asymmetry hold analytically?',
    'Different grounds for illegitimacy lead to different beneficiary sets and different legitimate repatriation endpoints: if process illegitimacy (force, absence of consent) is the issue, repatriation to any legitimate successor (state or community) would satisfy it; if outcome illegitimacy (expropriation of communal property) is the issue, repatriation to communities might be required; if theoretical illegitimacy (colonizers had no right to rule) is the issue, any post-colonial successor inherits the restitution claim. The constraint''s classification depends on this grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colonial_title_legitimacy_ambiguity, conceptual, 'The theoretical ground for treating colonial acquisition as illegitimate extraction.').

omega_variable(
    reading_kernel_contest_contestation,
    'Is this reading a genuine live position in international law and post-colonial discourse, or is it a constructed intermediate reading bracketing the real debate (which is between indigenous stewardship and universal heritage)?',
    'Manifest content analysis of post-colonial legal scholarship, UNESCO declarations, state repatriation claims, and international court proceedings: how frequently does state sovereignty emerge as the primary justification (vs. indigenous rights or universal access)? Who advocates for this reading institutionally? What political and economic interests support it?',
    'If this reading is genuinely live and defended on its own merits (state sovereignty as the legitimate ground), the constraint is a real kernel reading with institutional weight. If it is constructed (states use sovereignty language instrumentally, but the real dispute is elsewhere), the reading might be a misleading framing that obscures the genuine contest between communities and universal access advocates. This affects the kernel family''s configuration and whether this constraint should remain or be decomposed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_contestation, empirical, 'Whether the sovereign repatriation reading is a genuine institutional position or a constructed intermediate.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.42, falling over time) structural (legal barriers, enforcement mechanisms that could be removed) or internalized (holding institutions have internalized the sovereignty principle and voluntarily accept extraction)?',
    'Institutional study: if legal barriers to repatriation were removed (court rulings invalidate repatriation claims, new IP law allows indefinite possession), would holding institutions independently choose to return artifacts or resist? Do institutional policies reflect genuine commitment to repatriation or performative compliance? Post-repatriation institution behavior: do they continue to resist or accept the principle?',
    'If suppression is structural, removal of enforcement would restore the holding institution''s exit options and the constraint might revert to pure coordination (Rope). If suppression is internalized, the constraint is more stable even if enforcement weakens. Internalized suppression also suggests the constraint is approaching cultural legitimacy (holding institutions accept the principle as just) rather than remaining purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether measured suppression reflects structural legal barriers or internalized acceptance of the repatriation principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__sovereign_repatriation_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1945, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement_basis(cult_tr_t1945, projected).
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement_basis(cult_tr_t1970, observed).
narrative_ontology:measurement(cult_tr_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 1990, 0.16).
narrative_ontology:measurement_basis(cult_tr_t1990, observed).
narrative_ontology:measurement(cult_tr_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement_basis(cult_tr_t2010, observed).
narrative_ontology:measurement(cult_tr_t2020, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement_basis(cult_tr_t2020, observed).
narrative_ontology:measurement(cult_tr_t2026, cultural_property_legal_corpus__sovereign_repatriation_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(cult_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(cult_be_t1945, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement_basis(cult_be_t1945, projected).
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement_basis(cult_be_t1970, observed).
narrative_ontology:measurement(cult_be_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement_basis(cult_be_t1990, observed).
narrative_ontology:measurement(cult_be_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement_basis(cult_be_t2010, observed).
narrative_ontology:measurement(cult_be_t2020, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2020, 0.56).
narrative_ontology:measurement_basis(cult_be_t2020, observed).
narrative_ontology:measurement(cult_be_t2026, cultural_property_legal_corpus__sovereign_repatriation_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(cult_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1945, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1945, 0.72).
narrative_ontology:measurement_basis(cult_su_t1945, projected).
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1970, 0.65).
narrative_ontology:measurement_basis(cult_su_t1970, observed).
narrative_ontology:measurement(cult_su_t1990, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement_basis(cult_su_t1990, observed).
narrative_ontology:measurement(cult_su_t2010, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2010, 0.48).
narrative_ontology:measurement_basis(cult_su_t2010, observed).
narrative_ontology:measurement(cult_su_t2020, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement_basis(cult_su_t2020, observed).
narrative_ontology:measurement(cult_su_t2026, cultural_property_legal_corpus__sovereign_repatriation_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(cult_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__sovereign_repatriation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__sovereign_repatriation_reading, 0.18).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__sovereign_repatriation_reading, cultural_property_legal_corpus__universal_heritage_reading).

% DUAL FORMULATION NOTE:
% The cultural_property_legal_corpus kernel is decomposed into three constraint stories reflecting three structurally distinct readings: sovereign_repatriation_reading (this file, state sovereignty focus, moderate extractiveness), indigenous_stewardship_reading (community authority focus, potential high extractiveness if state appropriation occurs), and universal_heritage_reading (universal access focus, different victim/beneficiary set). Each reading instantiates different ε, beneficiary structure, and victim set because each reading instantiates different authority claims and different claims about what justice demands. They are linked via network.affects_constraints to signal they are siblings in the same kernel dispute, not competing claims on a single constraint. The three readings are not perspectives on one constraint; they are three distinct constraints arising from contention over a single kernel (the standing commitment about what cultural artifacts are and who legitimately controls them).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__sovereign_repatriation_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
