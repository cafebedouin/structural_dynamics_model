% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Te Tiriti o Waitangi Sovereignty Allocation - Rangatiratanga Reading
 *   domain: constitutional/post-colonial governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Treaty of Waitangi sovereignty
 *   kernel: the rangatiratanga reading, under which the Maori text of Article
 *   II guaranteed iwi and hapu full authority (tino rangatiratanga) over
 *   their lands, resources, and taonga while Article I granted the Crown only
 *   governorship (kawanatanga) over the settler population. Under this
 *   reading the standing arrangement under contest - plenary Crown
 *   legislative supremacy exercised over the whole realm, including Maori
 *   estates and taonga - is a standing breach of the guaranteed allocation,
 *   not its fulfillment. The epsilon referent is accordingly the existing
 *   Crown-supremacy arrangement assessed BY THIS READING'S OWN LIGHTS
 *   (near-total land alienation, expropriated authority, Crown-controlled
 *   resource consenting), never the co-governance structure this reading
 *   endorses. The claim/metric pair is authored independently: the claimed
 *   tangled_rope states my structural judgment (a real state performing real
 *   coordination while extracting asymmetrically through the same legal
 *   machinery, actively enforced), and the metrics state the operation as
 *   this reading descriptively assesses it; the engine computes per-seat
 *   classifications from the structural data. Family decomposition follows
 *   the epsilon-invariance principle: the colloquial label 'Treaty
 *   sovereignty' splits into three structurally distinct constraints (this
 *   reading, crown_sovereignty_reading, partnership_reading), linked via
 *   network.affects_constraints. Receipt surface: the gains demonstrably
 *   accrue to the state seat (land endowment, fisc, authority), so gain_flow
 *   names it; fixing the allocation for whoever could fix it (Parliament)
 *   requires constitutional-scale restructuring against entrenched title and
 *   fiscal interests, so fixing_cost is prohibitive.
 *
 * KEY AGENTS:
 *   - settler_state_institutions: agenda-holding beneficiary seat (institutional power, arbitrage-grade rule-rewriting exit) - receives the land endowment, the fiscal base, and plenary authority
 *   - iwi_hapu_collectives: primary target seat (organized power, trapped exit) - bears authority expropriation and land alienation; homeland cannot be exited
 *   - pastoral_commercial_landholders: derivative beneficiary (powerful, mobile) - holds appreciating titles tracing to the allocation
 *   - resource_extraction_licensees: derivative beneficiary (powerful, arbitrage, global scope) - operates under Crown permits over contested estates
 *   - maori_customary_title_holders: target seat (moderate power, trapped exit, regional) - holds remnants and unresolved claims below iwi corporate representation
 *   - urban_maori_unaffiliated: excluded voice (moderate power, trapped) - bears costs without settlement access or a negotiation seat
 *   - waitangi_tribunal: analytical observer (institutional) - documents conduct, recommends remedies, binds no one
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.8).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.62).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Te Tiriti o Waitangi Sovereignty Allocation - Rangatiratanga Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional/post-colonial governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, 'b828ae45-759c-4300-81a7-1438ff418cd9').
narrative_ontology:cs_kernel_codification('b828ae45-759c-4300-81a7-1438ff418cd9', fixed_text).
narrative_ontology:cs_authority_grounding('b828ae45-759c-4300-81a7-1438ff418cd9', lineage).
narrative_ontology:cs_interpretation_layer_present('b828ae45-759c-4300-81a7-1438ff418cd9').
narrative_ontology:cs_reading_relation('b828ae45-759c-4300-81a7-1438ff418cd9', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('b828ae45-759c-4300-81a7-1438ff418cd9', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('b828ae45-759c-4300-81a7-1438ff418cd9', foundational, tino_rangatiratanga_inherent_and_unceded).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_inherent_and_unceded, holdable).
narrative_ontology:cs_axiom_grounding('b828ae45-759c-4300-81a7-1438ff418cd9', tino_rangatiratanga_inherent_and_unceded, deontological).
narrative_ontology:cs_axiom('b828ae45-759c-4300-81a7-1438ff418cd9', secondary, kawanatanga_bounded_to_settler_governance).
narrative_ontology:cs_axiom_status(kawanatanga_bounded_to_settler_governance, holdable).
narrative_ontology:cs_axiom_grounding('b828ae45-759c-4300-81a7-1438ff418cd9', kawanatanga_bounded_to_settler_governance, conventional).
narrative_ontology:cs_reference_frame('b828ae45-759c-4300-81a7-1438ff418cd9', maori_text_dual_jurisdiction_settlement).
narrative_ontology:cs_drift_state('b828ae45-759c-4300-81a7-1438ff418cd9', contemporary_post_settlement_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('b828ae45-759c-4300-81a7-1438ff418cd9', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_state_institutions).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, pastoral_commercial_landholders).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, resource_extraction_licensees).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, iwi_hapu_collectives).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_customary_title_holders).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, urban_maori_unaffiliated).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_customary_title_holders).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_text_interpretive_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parliament and the Crown agencies hold lawmaking authority over the entire realm. They set the land-title rules, grant resource permits, run the treaty settlement process, and decide by ordinary majority how much weight treaty-derived obligations carry in any given statute. Revenues from rents, royalties, and the general tax base fund general government. Past practice includes legislating over judicial findings adverse to the allocation, most visibly the 2004 foreshore and seabed statute.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Farm and develop fee-simple titles acquired through Crown purchase, the aftermath of the 1860s conflicts, and Native Land Court conversion of communally held land. Titles trace to the allocation and appreciate under the legal order that issued them; owners can sell, and can move capital and households elsewhere while retaining the proceeds.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, pastoral_commercial_landholders, beneficiary,
    powerful, biographical, mobile, national).

% Run mines, quarries, forests, fisheries quota, and water consents issued under Crown permitting rather than under iwi or hapu authority. Access decisions rest with Crown ministers except where co-management has been negotiated case by case, and concession payments flow to the Crown. Firms can relocate operations and re-register offshore; permits are tradeable.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, resource_extraction_licensees, beneficiary,
    powerful, biographical, arbitrage, global).

% Represent the descendants of the 1840 signatories through post-settlement corporate entities and the hapu beneath them. Settlement packages delivered assets in exchange for agreements that close off further pursuit of historical grievances; entities administer those assets under Crown-chartered structures with audit and reporting duties. Membership lives on and near the ancestral estate; the collective cannot move its rohe, and descent ties people to it wherever individuals reside. Larger entities gained boardroom standing in the economy; smaller hapu inside their boundaries frequently did not.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, iwi_hapu_collectives, payer,
    organized, generational, trapped, national).

% Hold customary attachments to particular land blocks, waterways, burial grounds, and fishing grounds. Ancestors saw communal holdings converted to individual title through the Native Land Court and sold under debt and rating pressure; successors hold small remnants, papakainga, or unresolved claims. They receive ordinary citizen services and occasional settlement distributions but sit below iwi corporate representation in negotiations. Leaving the whenua would sever exactly the connection their standing rests on.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_customary_title_holders, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_customary_title_holders, beneficiary).

% Live predominantly in cities, often without registered affiliation or attached to iwi whose estates lie far away. Settlement assets distribute through iwi entities, so unaffiliated Maori receive little directly; urban Maori authorities secured a partial share only after litigation over the fisheries settlement. They appear in consultations and protests but hold no seat in the iwi-mandate negotiation structure.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, urban_maori_unaffiliated, excluded,
    moderate, biographical, trapped, national).

% Established in 1975 as a commission of inquiry into Maori grievances and Crown conduct measured against treaty principles. Takes testimony, publishes reports that anchor both official histories and claimant negotiations, and recommends remedies. Its findings bind neither Parliament nor the courts; governments have adopted or ignored reports at discretion.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_state_institutions).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one legal order, title registry, defense, currency, and public infrastructure across the islands - coordinating settler populations and day-to-day relations between settler and Maori communities after 1840, tasks that hapu had handled for themselves and that nobody had handled for the newcomers.
% TRANSFER_FUNCTION: Moves land, water and resource control, and governmental authority from iwi and hapu collectives to the Crown and to private holders under Crown-issued title; moves rates, royalties, and taxes from the whole population into the state fisc that administers the allocation.
% ABSENT_VOICES: Small hapu outside large settlement mandates, urban Maori without iwi registration, and the descendants of signatories who died before the grievance channel opened in 1975 or whose claims fell outside the 1992 fiscal-envelope cutoff would object to the allocation's terms. They stand outside the negotiating rooms, which are structured around iwi corporate mandates and Crown-defined settlement envelopes; contemporary objections surface in submissions, litigation, and protest rather than in decision seats.
% DISAPPEARANCE_RATIONALE: Most private and state titles in the country derive from the allocation; state revenue, local-government rating bases, and permit regimes all assume it. Overnight removal would activate iwi and hapu authority claims nationwide, break the chain of title, and force renegotiation of every resource consent - the constitutional order would rearrange immediately, not gracefully.
% FOUNDING_PROBLEM: Ordering coexistence in 1840: hundreds of autonomous hapu holding unquestioned authority, a rapidly growing settler population escaping effective chiefly or Crown control, mutual fear of lawlessness, and British imperial interest in a legitimate footing for colonization. The agreement sought orderly government for the newcomers under negotiated limits, with Maori authority and possessions expressly preserved in the Maori text.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the benefiting parties: William Colenso's contemporaneous printed account records rangatira objecting at Waitangi itself that chiefly authority would be diminished, and the Maori-language record of the signing-day debates survives independently of later Crown narratives. Subsequent attestation comes from Waitangi Tribunal evidential hearings (Crown-established, hence partially positioned) and from independent textual scholarship on the two versions (Orange, Fletcher, Mutu). No source outside the dispute certifies the founding problem as solved by the standing arrangement; the persistence of the contest is itself the corroborated state.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.80 at interval end) because, on this reading's accounting, roughly ninety-five percent of Maori land passed out of collective holding between 1840 and the late twentieth century, authority over taonga (freshwater, foreshore, seabed) remains Crown-consented, and settlement packages returned a fraction of losses under full-and-final closure clauses. Suppression (0.62) is lower than its historical peak: armed enforcement decayed after the nineteenth century, but legislative supremacy remains a standing override instrument (foreshore and seabed 2004) and funding leverage disciplines post-settlement entities. Theater ratio (0.48) is substantial: ceremony-heavy commemoration, apology choreography, and bicultural symbolism accompany comparatively thin authority transfer, and settlement signings are staged as closure while gag provisions bar relitigating apologies. Accessibility_collapse (0.66): Maori self-governance alternatives were forcibly collapsed (KINGITANGA invaded, Kotahitanga petitions dismissed), yet partial reopening exists at Crown discretion (co-governance appointments, legal-personhood arrangements), so alternatives are damaged, not annihilated. Resistance (0.70) is continuous and occasionally effective (1975 land march, Bastion Point, Whanganui River settlement). The temporal series runs on ONE shared nine-point grid (all three metrics authored at every point, endpoints matching base_properties) and traces a full accumulation-release cycle: coercive buildup to the 1860s-1890s peak, bureaucratic consolidation, mid-century plateau, liberalization and settlement-opening from 1975-1995, then renewed contestation (foreshore and seabed, freshwater and co-governance disputes) lifting enforcement intensity again. Part of the oscillation is the mechanism, not noise: packages arrive contingent on permanently closing grievances, then new fronts open - intermittent reinforcement by design.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the arrangement presents as the ordinary machinery of a functioning state it administers and profits from - coordination experienced as rope-like, with the treaty layer as a manageable obligations file. From the trapped target seats the same machinery arrives as enforced displacement: title conversion, consent regimes, and closure clauses experienced as coercion with no exit (snare-flavored experience). The engine computes per-seat classifications from power and exit atoms; the authored claim does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations plus exit atoms drive the derivation. settler_state_institutions sits nearest the beneficiary pole: it receives land endowment, fisc, and authority, and its ability to rewrite the rules by ordinary majority (arbitrage-grade exit from any given rule) damps effective pressure further. pastoral_commercial_landholders and resource_extraction_licensees sit low-d: titles and permits ride the allocation, and their relocation options insulate them. iwi_hapu_collectives and maori_customary_title_holders sit nearest the target pole, amplified by trapped exit - the rohe cannot be abandoned without forfeiting the very inheritance at stake - so effective extraction approaches the full-target end. urban_maori_unaffiliated inherits target-side position WITHOUT settlement offsets, the sharpest asymmetry in the set. No directionality_overrides are authored: unlike regulatory-capture cases, there is no captured intermediary whose derived d would invert (the Tribunal is authored as observer, not beneficiary), and no hidden indirect-beneficiary case distorts the payer seats; the structural derivation already reproduces the true positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (orderly joint government after 1840) is authentically contested: the ordering problem partly persists (a mixed polity still needs coordination) while the reading contends the specific bargain was broken almost immediately and what persists is the expropriation. Recording founding_problem_status as contested, combined with the world_rearranges verdict, routes this story to the capture/zombie cross-check rather than a dead-mandate declaration - correct, because the arrangement is neither inert (live extraction, live resistance) nor purely functional. The tangled_rope claim does the anti-mislabeling work in both directions: calling the whole arrangement a snare would erase the genuine coordination every seat consumes (courts, infrastructure, currency serve Maori citizens too); calling it a rope or mere partnership would erase identifiable victims and the enforcement machinery their exclusion requires. Mandatrophy is NOT declared resolved: the mandate has transformed rather than died, and declaring resolution would launder the contested genealogy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates one reading (rangatiratanga_reading) of the waitangi_sovereignty_allocation kernel; how would adoption of the sibling readings restructure the allocation and this story''s classification?',
    'Comparative adjudication across readings: crown_sovereignty_reading dissolves this constraint''s victim structure into settled-law compliance (targets collapse toward the beneficiary side); partnership_reading retains the victim structure but softens extraction through consultation and active-protection duties without transferring authority.',
    'Classification is reading-relative over the same standing arrangement: tangled_rope under this reading, nearer rope under the partnership reading, background constitutional fact under the crown-sovereignty reading. Cross-story comparison must join on the kernel, not the label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel-membership and sibling-delta routing for the sovereignty-allocation contest.').

omega_variable(
    textual_priority_translation_gap,
    'Which text governs the kernel - the English draft''s cession of complete sovereignty or the Maori text''s retention of tino rangatiratanga with only kawanatanga granted?',
    'Textual and archival scholarship on the two versions and the signing-day debates (Mutu, Fletcher, Orange; the Maori-language manuscript record), ultimately settling as a constitutional-framing choice by courts and legislature.',
    'If the English text governs, this constraint''s premise fails and the crown_sovereignty_reading stands; if the Maori text governs, this constraint''s victim structure is confirmed and the standing arrangement reads as breach rather than settlement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_priority_translation_gap, conceptual, 'The disagreement''s precise location: the two texts describe different transactions.').

omega_variable(
    transition_or_managed_containment,
    'Is the settlement architecture (Tribunal inquiries, negotiated asset packages, co-governance pockets) a transition toward restored rangatiratanga, or a containment mechanism that closes grievance channels while preserving the allocation?',
    'Track post-settlement authority trajectories: do co-governance arrangements expand toward resource veto and jurisdiction, or stall at advisory standing while full-and-final clauses bar reopening?',
    'Genuine transition supports a future scaffold-flavored phase for the settlement layer; containment confirms a durable tangled_rope with substantial theatrical reconciliation components and rising theater_ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transition_or_managed_containment, conceptual, 'Whether settlement-era accommodation is transitional support or managed closure.').

omega_variable(
    present_generation_benefit_distribution,
    'Do present-generation settler-descendants and licensees remain net beneficiaries of the allocation once settlements, taxation, and returned assets are counted, or have costs and benefits diffused across the population?',
    'Intergenerational accounting: capitalized land-endowment and permit-value gains versus settlement outlays and co-governance costs borne by current taxpayers and ratepayers.',
    'If net benefit has diffused, gain-flow concentration weakens and cross-class coalitions for restructuring become feasible; if it remains concentrated in the state and title-holding seats, capture persists and fixing_cost stays prohibitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(present_generation_benefit_distribution, empirical, 'Persistence or diffusion of the beneficiary structure into present generations.').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of the residual suppression keeping rangatiratanga claims from converting into jurisdiction is structural (legislative supremacy, funding leverage over post-settlement entities) versus internalized (acceptance of Crown-frame legitimacy within Maori institutions themselves)?',
    'Post-recognition trajectories: where judicial or Tribunal recognition occurred (Whanganui River, pre-2004 foreshore findings), did jurisdiction follow or dissipate at the legislative layer; internalization shows up as iwi institutions declining available jurisdictional tools or policing their own claimants toward closure.',
    'A high internalized share means removing structural barriers alone will not move the classification; targets carry the suppression with them into settlement structures, binding them to the arrangement that displaced them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of residual suppression between external barriers and absorbed legitimacy frames.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(waitangi_rr_tr_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(waitangi_rr_tr_t25, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement(waitangi_rr_tr_t50, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 50, 0.4).
narrative_ontology:measurement(waitangi_rr_tr_t75, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 75, 0.46).
narrative_ontology:measurement(waitangi_rr_tr_t100, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 100, 0.5).
narrative_ontology:measurement(waitangi_rr_tr_t125, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 125, 0.45).
narrative_ontology:measurement(waitangi_rr_tr_t150, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 150, 0.55).
narrative_ontology:measurement(waitangi_rr_tr_t175, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 175, 0.52).
narrative_ontology:measurement(waitangi_rr_tr_t185, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 185, 0.48).

% Extraction over time
narrative_ontology:measurement(waitangi_rr_be_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(waitangi_rr_be_t25, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(waitangi_rr_be_t50, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 50, 0.9).
narrative_ontology:measurement(waitangi_rr_be_t75, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 75, 0.87).
narrative_ontology:measurement(waitangi_rr_be_t100, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 100, 0.85).
narrative_ontology:measurement(waitangi_rr_be_t125, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 125, 0.84).
narrative_ontology:measurement(waitangi_rr_be_t150, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 150, 0.78).
narrative_ontology:measurement(waitangi_rr_be_t175, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 175, 0.77).
narrative_ontology:measurement(waitangi_rr_be_t185, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 185, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(waitangi_rr_su_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(waitangi_rr_su_t25, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(waitangi_rr_su_t50, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 50, 0.66).
narrative_ontology:measurement(waitangi_rr_su_t75, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 75, 0.6).
narrative_ontology:measurement(waitangi_rr_su_t100, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(waitangi_rr_su_t125, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 125, 0.5).
narrative_ontology:measurement(waitangi_rr_su_t150, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 150, 0.44).
narrative_ontology:measurement(waitangi_rr_su_t175, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 175, 0.56).
narrative_ontology:measurement(waitangi_rr_su_t185, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 185, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Treaty of Waitangi sovereignty' decomposes per the epsilon-invariance principle into three structurally distinct stories sharing one kernel (waitangi_sovereignty_allocation). crown_sovereignty_reading is the institutionalized upstream reading (operative law, highest formal confidence); partnership_reading is the court-developed middle reading (Principles framework); rangatiratanga_reading (this file) is the advocacy-downstream reading citing the Maori text against the English draft. The upstream reading is cited as settled ground AGAINST this one; this reading pressures the middle reading to stretch (personhood arrangements, co-governance) without resolving the contest. Each member carries its own epsilon over the same standing arrangement: negligible contest for the crown reading, moderated extraction for the partnership reading, severe breach-assessment for this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
