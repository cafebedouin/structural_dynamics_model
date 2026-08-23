% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__religious_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__religious_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__religious_zionist_reading
 *   human_readable: Divine-Title Territorial Claim over Eretz Yisrael (Religious Zionist Reading)
 *   domain: political philosophy / nationalism studies / religion-and-state
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the religious-zionist reading — of
 *   the contested kernel jewish_sovereignty_palestine. In this reading, the
 *   divine promise of the land to the Jewish people (Genesis 15, Deuteronomy
 *   30) grounds an inalienable territorial title over the whole of Eretz
 *   Yisrael; Jewish statehood is not ordinary politics but a stage of
 *   redemption (atchalta degeula), and the land itself is non-negotiable: no
 *   partition, no shared-sovereignty formula, no negotiated cession has
 *   standing, because the title's grantor is not a party to human bargains.
 *   The standing arrangement this reading constitutes and authorizes —
 *   sovereign control plus the settlement regime over the land between the
 *   Mediterranean and the Jordan — is the epsilon referent, assessed by the
 *   reading's own lights. Those lights are precisely what makes the measured
 *   extraction maximal rather than mild: the frame registers the covenant
 *   community's costs, gains, and obligations in full, while placing the
 *   non-covenant population outside the calculus altogether (subordination,
 *   not representation). A frame that cannot register its governed
 *   population's costs has no internal brake, so the arrangement it
 *   authorizes runs at the ceiling of what the territory's demography
 *   permits. The epsilon value is reading-indexed over this fixed referent
 *   (OQ-258/OQ-26): the liberal-nationalist sibling, whose frame admits
 *   bounded, negotiable self-determination, would author a materially lower
 *   epsilon for the same referent; the settler-colonial sibling would author
 *   comparable or higher epsilon with a different victim enumeration. This
 *   file authors only its own reading; the siblings are separate constraints
 *   linked through network.affects_constraints. The claim/metrics split is
 *   deliberate: claimed_type states what is structurally true (genuine
 *   covenant coordination fused with massive asymmetric extraction under
 *   active enforcement — a tangled rope, not a pure snare, because the
 *   coordination function for adherents is sincere and load-bearing), while
 *   the metric values describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - kook_lineage_rabbinic_authority: agenda-setter (institutional/identity_locked) — sets the covenant interpretation, ordains settlement as commandment, sanctifies state action, and defines the boundary of permissible concession (none touching the whole land)
 *   - west_bank_settlement_residents: primary beneficiary (organized/identity_locked) — hold and develop land under the divine-title frame; receive subsidy, infrastructure, and military protection; exit equals abandoning a life-defining covenantal vocation
 *   - jewish_diaspora_covenant_supporters: secondary beneficiary (moderate/constrained) — fund and politically shield the project; receive identity coordination and historical continuity in exchange
 *   - palestinian_west_bank_communities: primary target (powerless/trapped) — farm, build, and move under permit regimes; absorb land appropriation and infrastructure asymmetry; hold no title standing inside the frame
 *   - palestinian_refugee_diaspora: primary target (powerless/trapped) — carry pre-1948 deeds, keys, and claims that the frame categorically refuses standing; dispersed across neighboring states
 *   - palestinian_citizens_of_israel: target with partial inclusion (moderate/trapped) — hold citizenship and votes but sit outside the covenant that legitimates the state they live in
 *   - israeli_state_apparatus: dual-positioned administrator (institutional/arbitrage) — enforces the settlement regime while absorbing its diplomatic and security costs; oscillates between embracing and containing the frame
 *   - territorial_compromise_advocates: excluded voice (moderate/constrained) — accept partition in principle; marginalized as faithless or naive within the covenant conversation
 *   - anti_zionist_haredi_communities: excluded voice (organized/identity_locked) — reject theological fulfillment itself on messianic-timing grounds; object from inside Judaism, outside the settlement conversation
 *   - international_legal_order: analytical observer (institutional/analytical) — documents the regime's outputs (occupation status, settlement legality, annexation moves); cannot alter the frame, only record it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, 0.9).
domain_priors:suppression_score(jewish_sovereignty_palestine__religious_zionist_reading, 0.84).
domain_priors:theater_ratio(jewish_sovereignty_palestine__religious_zionist_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__religious_zionist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__religious_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__religious_zionist_reading, "Divine-Title Territorial Claim over Eretz Yisrael (Religious Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__religious_zionist_reading, "political philosophy / nationalism studies / religion-and-state").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__religious_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__religious_zionist_reading, 'e5a85efd-c86a-4189-bb1e-814ae5d842f1').
narrative_ontology:cs_kernel_codification('e5a85efd-c86a-4189-bb1e-814ae5d842f1', fixed_text).
narrative_ontology:cs_authority_grounding('e5a85efd-c86a-4189-bb1e-814ae5d842f1', lineage).
narrative_ontology:cs_interpretation_layer_present('e5a85efd-c86a-4189-bb1e-814ae5d842f1').
narrative_ontology:cs_reading_relation('e5a85efd-c86a-4189-bb1e-814ae5d842f1', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5a85efd-c86a-4189-bb1e-814ae5d842f1', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e5a85efd-c86a-4189-bb1e-814ae5d842f1', jewish_sovereignty_palestine__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('e5a85efd-c86a-4189-bb1e-814ae5d842f1', jewish_sovereignty_palestine__post_zionist_reading, forecloses).
narrative_ontology:cs_axiom('e5a85efd-c86a-4189-bb1e-814ae5d842f1', foundational, eretz_yisrael_divine_title).
narrative_ontology:cs_axiom_status(eretz_yisrael_divine_title, holdable).
narrative_ontology:cs_axiom_grounding('e5a85efd-c86a-4189-bb1e-814ae5d842f1', eretz_yisrael_divine_title, theological).
narrative_ontology:cs_axiom('e5a85efd-c86a-4189-bb1e-814ae5d842f1', secondary, territorial_partition_theologically_void).
narrative_ontology:cs_axiom_status(territorial_partition_theologically_void, holdable).
narrative_ontology:cs_axiom_grounding('e5a85efd-c86a-4189-bb1e-814ae5d842f1', territorial_partition_theologically_void, theological).
narrative_ontology:cs_reference_frame('e5a85efd-c86a-4189-bb1e-814ae5d842f1', whole_land_divine_grant).
narrative_ontology:cs_drift_state('e5a85efd-c86a-4189-bb1e-814ae5d842f1', contemporary_partial_fulfillment, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e5a85efd-c86a-4189-bb1e-814ae5d842f1', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, kook_lineage_rabbinic_authority).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, west_bank_settlement_residents).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__religious_zionist_reading, jewish_diaspora_covenant_supporters).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_west_bank_communities).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_citizens_of_israel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_apparatus).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Runs the yeshiva networks, rabbinic courts, and movement organizations that teach the covenant reading: the land was promised, sovereignty is a stage of redemption, settlement is a commandment. Supplies theological authorization for state action in the territories and draws its own standing from being the interpreter of the promise. Its authority exists only inside the frame it maintains; abandoning the frame would dissolve the institution itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, kook_lineage_rabbinic_authority, agenda_setter,
    institutional, civilizational, identity_locked, national).

% Live in communities built on land the frame holds as divinely granted. Housing subsidies, roads, water allocations, schooling, and army protection flow to them; they supply the facts-on-the-ground that make the claim physical. For many, residence is a religious vocation — leaving would mean abandoning what they understand as their assigned role in redemption, not merely changing addresses.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, west_bank_settlement_residents, beneficiary,
    organized, generational, identity_locked, regional).

% Contribute funding, political advocacy, and emigration to the project from communities across the diaspora. What flows back is identity coordination: belonging, historical continuity, and a answer to exile. Distancing is possible — donations stop, affiliation lapses — but it carries real social and self-understanding costs inside their communities, so exit is costly rather than blocked.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, jewish_diaspora_covenant_supporters, beneficiary,
    moderate, biographical, constrained, global).

% Farm terraces herded by settlement fences, queue at checkpoints, build under permits that are mostly refused, and draw water under allocation rules that favor the settlements uphill. Land their families worked for generations is taken for outposts and bypass roads. Their own deeds and claims have no standing in the frame that governs them, and leaving the territory means losing everything they hold.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_west_bank_communities, payer,
    powerless, generational, trapped, regional).

% Descendants of the 1948 and 1967 displacements, holding deeds, keys, and camp registrations across Lebanon, Jordan, Syria, and beyond. Return is the one outcome the frame cannot accommodate, because return would undo the title it asserts. They bear the arrangement's founding cost without inhabiting its territory, and no channel exists through which their claim could be priced in.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, continental).

% Vote, serve where required, and receive state services, but live inside a state whose deepest self-justification — the covenant reading as it saturates law and settlement policy — locates them outside the community that justification serves. Planning discrimination, land expropriations, and unequal municipal resourcing are the recurring texture. Citizenship binds them to the polity; nothing binds the polity's legitimating frame to them.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, trapped, national).

% Administers sovereignty and the occupied territories: the army protects settlements, the courts process land claims, the ministries fund infrastructure. The state also absorbs the arrangement's bills — international censure, UN resolutions, coalition politics distorted by settlement constituencies, and the manpower cost of permanent garrison duty. Unlike the believer seats, the state can hedge: it signed Oslo while building through Oslo, and it can reframe between the covenant story and a security story as diplomacy requires.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__religious_zionist_reading, israeli_state_apparatus, payer).

% Israelis and allies who accept partition, land swaps, or shared arrangements as the workable shape of peace. Inside the covenant conversation they appear as faithless or naive; their arguments are heard but carry no standing where the frame sets terms, because what they propose is precisely what the frame declares void. They organize, publish, and vote from outside the room where the title question is decided.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, territorial_compromise_advocates, excluded,
    moderate, biographical, constrained, national).

% Ultra-Orthodox communities, principally Satmar and allied circles, that reject the theological fulfillment claim itself: redemption comes with the Messiah, and human sovereignty over the land beforehand is presumption, not piety. They object from deep inside Judaism — citing the same texts the covenant reading cites — and are nonetheless entirely outside the settlement conversation their objection targets. Their identity is as bound to Torah as the adherents' is; they cannot exit the religion to make the point.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, anti_zionist_haredi_communities, excluded,
    organized, civilizational, identity_locked, global).

% UN bodies, the ICJ, and Geneva-law mechanisms that classify the territories as occupied, judge settlements illegal, and document annexation steps. They take testimony from all seats except the ones the frame excludes, issue findings, and impose soft costs. They cannot touch the covenant frame — its grantor is not a signatory to anything — so their function is limited to recording what the frame produces.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__religious_zionist_reading, international_legal_order, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__religious_zionist_reading, west_bank_settlement_residents).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__religious_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the Jewish covenant community across diaspora and homeland: a shared sacred geography, a migratory vocation (settlement as commandment), intergenerational transmission of purpose, and a mobilization structure that turns scattered populations into a directed collective. Stated without evaluation: this is what the frame does for those inside it.
% TRANSFER_FUNCTION: Moves land — title, access, habitation rights, and development — from non-covenant inhabitants to covenant members; moves water, mobility freedom, and permitting discretion from Palestinian communities to the settlement economy; moves money and political protection from diaspora supporters into the project; and moves meaning, status, and redemptive assurance to adherents and the rabbinic interpreters who certify them.
% ABSENT_VOICES: The people whose land, return, and equality are allocated by this frame have no seat in it: West Bank villagers, the refugee diaspora, and — in the frame's deeper structure — Palestinian citizens of the state itself. Their counterclaims are not debated inside the covenant conversation; they are categorized (trial, temporary condition, or category error). Also absent: anti-Zionist ultra-Orthodox voices whose objection is theological rather than national — they would contest the frame's core premise from within its own scripture, and are structurally kept out of the conversation that frame licenses.
% DISAPPEARANCE_RATIONALE: If the divine-title claim vanished overnight, the settlement enterprise loses its warrant and its volunteers; the state's governing coalition loses its right flank's glue; partition and shared-sovereignty formulas instantly regain standing as negotiable rather than theologically void; the refugee-return question converts from category error to bargaining chip; and diaspora support structures would re-route toward the liberal-nationalist sibling frame. Nearly every arrangement on the ground depends on the claim's continuing force.
% FOUNDING_PROBLEM: Securing Jewish collective existence through guarantee rather than human goodwill: after catastrophe demonstrated that emancipation and human institutions would not reliably protect the Jewish people, the reading answers exile's vulnerability with covenantal restoration — a title guaranteed by God cannot be revoked by men, and statehood becomes the visible down payment of redemption.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration is asymmetric and this is itself signal. Academic historiography of Religious Zionism (studies of the Kook lineage and of the post-1967 settlement movements) — scholarship produced largely from inside or adjacent to the Jewish frame — attests that the founding problem was real, widely felt, and the genuine driver of the reading's crystallization; that corroboration stands. No Palestinian source corroborates the founding problem's status as live in the frame's terms: from outside, the attestors (Palestinian historians, international jurists, the refugee communities themselves) describe the arrangement not as solving a problem but as manufacturing their exclusion — they corroborate the genealogy while disputing its license. The statement that corroboration of liveness exists only within the beneficiary-side frame is offered plainly rather than papered over.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__religious_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__religious_zionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__religious_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__religious_zionist_reading, 0.9, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__religious_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__religious_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.90 because the claim structure is absolute: a title that admits no negotiation converts every acre of inhabited land into covenant property and every non-covenant inhabitant into someone whose tenure is, at best, derivative tolerance. Suppression is authored at 0.84 and is a RAW STRUCTURAL PROPERTY — unscaled by power or scope in the engine's computation — reflecting the machinery the arrangement needs: permit regimes, military protection of settlements, judicial absorption of the maximalist frame, and the political disciplining of compromise positions inside the Jewish polity. Theater_ratio is low-to-moderate (0.24) and rising slowly: the core activity (building, settling, teaching) is functional, but each decade of deferred redemption increases the share of effort spent performing certainty about an outcome that keeps not arriving. Accessibility_collapse is 0.78 — very high WITHIN the frame, where partition and shared sovereignty are not merely unwise but theologically void, yet short of natural-law levels because the alternatives remain fully coherent outside the frame and the frame's reach is not universal. Resistance is 0.65: armed and civic Palestinian resistance, international legal and boycott pressure, and intra-Jewish dissent from both the liberal-left and the anti-Zionist ultra-Orthodox. Coalition potential among the powerless victims is real and partially realized (Palestinian political unification attempts, transnational boycott campaigns, state-level recognition pushes) and is what keeps resistance this high despite the victims' individually powerless positions. All three temporal series run on one shared grid (points 0, 10, 20, 30, 40, 50, 58 of a 1967-baselined interval ending 2025), so no metric borrows an end-state value at earlier times.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different constraints from identical structure. From the settlement-resident seat, the arrangement is covenant fulfillment — the felt experience is of restoring, not taking; perceived extraction approaches zero and perceived legitimacy is total. From the West Bank village seat and the refugee seat, the same structure is dispossession with a liturgy. The diaspora-supporter seat experiences primarily identity coordination: belonging, continuity, meaning — while the material and political costs land elsewhere. The state seat is genuinely torn: it administers and enforces the frame, collects legitimation from it, and simultaneously pays its bills (diplomatic isolation, coalition distortion, security burden) — a capture-and-cost simultaneity the derivation chain can only approximate. Inter-institutionally, the rabbinic authority, the state apparatus, and the international legal order hold the same nominal institutional power class but experience the constraint through completely different functions: authorship, administration, and documentation respectively. Same-level lateral divergence is sharpest between diaspora supporters and territorial-compromise advocates — same broad community, similar formal standing, differentiated by whether their identity is fused with the covenant frame (identity_locked) or merely adjacent to it (constrained). The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate among these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map to low-d seats: the rabbinic authority (d near 0.0 — the frame is its product and its authority flows from maintaining it), settlement residents (d near 0.0 on material flow, with identity lock pinning them at the subsidized end regardless of cost-bearing), and diaspora supporters (low d, moderately damped further by their constrained-but-real distance from the territory). The victim declarations map to high-d seats: West Bank communities and the refugee diaspora sit at or near full-target d, amplified by trapped exit — they cannot sell, relocate, or renegotiate their way out of the frame's jurisdiction, and identity is not theirs to shed because the frame assigns it to them. Palestinian citizens of Israel occupy an intermediate-high d: included in the polity's services, excluded from its covenant, unable to exit citizenship cheaply. Spatial scope amplifies effective extraction for the payer seats: the arrangement operates across a contested national-territorial space where verification of costs is difficult and accountability diffuse. No directionality overrides were used: the available override granularity is the power atom, and this story's three institutional-class seats (rabbinic authority, state apparatus, international legal order) would collide on a single override — the derivation chain's per-seat handling from roles plus exit options is closer to the truth than any atom-keyed correction available.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification as tangled_rope prevents two symmetrical mislabels. Mislabeling as pure snare would erase the genuine coordination function: for millions of adherents the covenant frame sincerely solves real collective-action problems — it coordinates diaspora identity across generations, motivates aliyah, funds institutions, supplies a shared sacred geography, and organizes sacrifice around a common purpose. That coordination is not cover; it is load-bearing and believed. Mislabeling as pure rope would erase the extraction: the same structure that coordinates the covenant community transfers land, water, mobility, and demographic futures from the non-covenant population, under enforcement, with no compensating channel. Mandatrophy status: the founding problem (securing Jewish collective existence through guarantee rather than human goodwill) remains LIVE inside the frame — redemption is definitionally incomplete — so status=live combined with disappearance_verdict=world_rearranges is internally consistent and raises no zombie flag. The forward-looking warning is theater drift: if redemption continues to defer, the rising theater_ratio series projects the reading's slow conversion from live coordination toward inertial, performance-maintained persistence — the classic piton pathway — with the identity lock (see omega identity_lock_break_pathway) determining whether belief's death produces decay, migration to the liberal-nationalist sibling frame, or collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the religious-zionist reading of the contested kernel jewish_sovereignty_palestine; four sibling readings (liberal_nationalist, settler_colonial, cultural_zionist, post_zionist) instantiate structurally different constraints over the same referent. How much of the computed classification is reading-indexed rather than kernel-level?',
    'Generate each sibling reading as its own epsilon-invariant story over the fixed referent and compare computed per-seat classifications; disagreement located in the grounding axiom (divine grant vs. universal self-determination right vs. displacement-regime diagnosis vs. spiritual-center minimalism vs. revisable civic framework).',
    'The settler-colonial sibling would likely author comparable or higher epsilon with a different victim enumeration; the liberal-nationalist sibling would author materially lower epsilon because its frame admits partition legitimacy. No kernel-level conclusion is valid until the cross-reading comparison is taken.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a contested kernel; classification is reading-indexed.').

omega_variable(
    divine_title_naturalness,
    'Within adherent epistemology the covenant title presents as objective and irreducible — a fact about the world, not a policy choice — while externally the claim is a constructed constraint with identifiable beneficiaries. Which description controls the classification?',
    'Behavioral test: does the claim survive falsification pressure (archaeological and demographic record, international legal rulings) or is it maintained by enforcement machinery (movement discipline, coalition politics, settlement facts-on-the-ground)? A claim held only under active maintenance is constructed.',
    'If read as natural law, extraction from non-adherents reads as the price of reality and the constraint approaches mountain immunity; read as constructed, the beneficiary structure becomes visible and the false-summit signature applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_title_naturalness, conceptual, 'Natural-law presentation inside the frame vs. constructed constraint outside it.').

omega_variable(
    victim_calculus_subordination,
    'Does the reading''s covenant calculus exclude Palestinian cost-bearing entirely, or register it in subordinate channels (resident-alien frameworks, human-rights-leaning religious currents, halakhic debates on treatment of non-Jews under sovereignty)?',
    'Survey internal rabbinic literature, movement platforms, and religious Zionist educational curricula for explicit registration of Palestinian costs and for institutional channels that translate that registration into policy pressure.',
    'Subordinate registration implies internal corrective pressure exists and effective extraction for payer seats is somewhat tempered by intra-frame contest; full absence places payer-seat extraction at the maximum the structure permits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_calculus_subordination, empirical, 'Whether Palestinian costs are invisible or merely subordinate in the covenant frame.').

omega_variable(
    messianic_deferral_theater,
    'Redemption has been declared imminent for nearly six decades while remaining deferred. Does each decade of deferral convert functional mobilization into performative certainty (rising theater_ratio), or does incrementalist doctrine keep the mobilization functional?',
    'Longitudinal comparison of movement rhetoric (certainty language, apocalyptic framing) against physical settlement-construction and institutional-building rates across the interval.',
    'Sustained theater_ratio climb past 0.5 would signal transition from live coordination toward inertial, performance-maintained persistence — a piton-direction warning for this reading''s long-run trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_deferral_theater, empirical, 'Deferred-messianism drift from functional mobilization to performative maintenance.').

omega_variable(
    identity_lock_break_pathway,
    'What event class could break the identity lock binding adherents to the covenant frame — messianic disillusion, state abandonment of settlements, major theological schism — and when the lock breaks, does the constraint decay into inertia, migrate to the liberal-nationalist frame, or collapse outright?',
    'Comparative study of prior messianic political movements that suffered disillusion (date-setting failures, defeated maximalisms) and tracked what happened to their institutional inheritances.',
    'Determines whether the disappearance verdict survives the death of belief: an identity-locked structure whose belief dies typically leaves an inertial remnant administered by those who cannot afford to admit the frame broke.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_break_pathway, conceptual, 'Exit-pathway contingency for the identity-locked adherent seats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__religious_zionist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jewi_tr_t10, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(jewi_tr_t20, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(jewi_tr_t30, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement(jewi_tr_t40, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(jewi_tr_t50, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(jewi_tr_t58, jewish_sovereignty_palestine__religious_zionist_reading, theater_ratio, 58, 0.24).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jewi_be_t10, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(jewi_be_t20, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(jewi_be_t30, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(jewi_be_t40, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 40, 0.8).
narrative_ontology:measurement(jewi_be_t50, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 50, 0.86).
narrative_ontology:measurement(jewi_be_t58, jewish_sovereignty_palestine__religious_zionist_reading, base_extractiveness, 58, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(jewi_su_t10, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(jewi_su_t20, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(jewi_su_t30, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(jewi_su_t40, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(jewi_su_t50, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 50, 0.8).
narrative_ontology:measurement(jewi_su_t58, jewish_sovereignty_palestine__religious_zionist_reading, suppression_requirement, 58, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__religious_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__religious_zionist_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Jewish sovereignty in Palestine.' The label conflates five structurally distinct claims that share one referent (the standing sovereignty-plus-settlement arrangement) and diverge on grounding and scope: cultural-zionist (spiritual center, sovereignty optional), liberal-nationalist (self-determination right, partition legitimate), religious-zionist (THIS FILE: divine grant, whole land, no partition legitimacy), settler-colonial (displacement regime regardless of intent), post-zionist (achieved statehood, now a revisable civic framework obstructing equality). Epsilon differs by reading over the fixed referent; each sibling gets its own file, its own beneficiary/victim structure, and its own classification. Reading-relation logic: foreclosure edges run from this reading toward the two validity-contesting siblings (settler_colonial, post_zionist) because their core premises deny what this reading's foundational axiom asserts — a single framework cannot simultaneously hold divine-grant inalienability and 'displacement-regime-regardless-of-intent' or 'founding-narrative-is-revisable.' The liberal-nationalist and cultural-zionist siblings differ on mechanism or scope, not validity, so those edges are coexistence: different parties hold each simultaneously. Upstream/downstream: the liberal-nationalist reading is the broadest-tent frame and shapes the resource environment in which this reading operates; this reading in turn pressures the liberal-nationalist sibling's legitimacy conditions (maximalism forces it to defend partition against its own camp).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
