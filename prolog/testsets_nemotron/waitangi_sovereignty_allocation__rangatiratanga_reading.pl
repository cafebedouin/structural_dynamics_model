% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-24
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Treaty of Waitangi Article II — Tino Rangatiratanga Reading
 *   domain: constitutional/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi (1840) has two texts. The English text Article I
 *   cedes 'sovereignty' to the Crown; the Māori text Article I grants
 *   'kāwanatanga' (governorship) and Article II guarantees 'tino
 *   rangatiratanga' (full authority/chieftainship) over 'ngā whenua me ngā
 *   taonga katoa' (all lands and treasures). This constraint story
 *   instantiates the rangatiratanga reading: the Māori text is authoritative,
 *   tino rangatiratanga was retained, and Crown jurisdiction is limited to
 *   settlers. This reading is one of three sibling readings of the
 *   waitangi_sovereignty_allocation kernel. The constraint operates as a
 *   tangled rope: it coordinates two peoples on one land base (genuine
 *   coordination function) while the Crown's historical suppression of
 *   rangatiratanga extracted land, resources, and decision-making from Māori
 *   (asymmetric extraction). Active enforcement is required — by the Crown
 *   (to implement), by Māori (to exercise), and by the Tribunal (to
 *   interpret). The measurement series tracks 185 years: initial low
 *   extraction (1840), peak extraction during confiscation and assimilation
 *   eras (1865–1900), partial reversal through Treaty settlements and
 *   recognition (1975–2000), and a recent plateau where structural
 *   suppression persists despite lowered extractiveness (2010–2025).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.15).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi Article II — Tino Rangatiratanga Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '52c5b8d1-9550-41d4-9786-230eca64d9cc').
narrative_ontology:cs_kernel_codification('52c5b8d1-9550-41d4-9786-230eca64d9cc', fixed_text).
narrative_ontology:cs_authority_grounding('52c5b8d1-9550-41d4-9786-230eca64d9cc', lineage).
narrative_ontology:cs_interpretation_layer_present('52c5b8d1-9550-41d4-9786-230eca64d9cc').
narrative_ontology:cs_reading_relation('52c5b8d1-9550-41d4-9786-230eca64d9cc', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('52c5b8d1-9550-41d4-9786-230eca64d9cc', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('52c5b8d1-9550-41d4-9786-230eca64d9cc', foundational, maori_text_article_ii_authoritative).
narrative_ontology:cs_axiom_status(maori_text_article_ii_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('52c5b8d1-9550-41d4-9786-230eca64d9cc', maori_text_article_ii_authoritative, conventional).
narrative_ontology:cs_axiom('52c5b8d1-9550-41d4-9786-230eca64d9cc', foundational, tino_rangatiratanga_retained_not_ceded).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_retained_not_ceded, holdable).
narrative_ontology:cs_axiom_grounding('52c5b8d1-9550-41d4-9786-230eca64d9cc', tino_rangatiratanga_retained_not_ceded, deontological).
narrative_ontology:cs_reference_frame('52c5b8d1-9550-41d4-9786-230eca64d9cc', maori_text_tino_rangatiratanga_1840).
narrative_ontology:cs_drift_state('52c5b8d1-9550-41d4-9786-230eca64d9cc', contemporary_settlement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('52c5b8d1-9550-41d4-9786-230eca64d9cc', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collectives).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_population).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collectives).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, tino_rangatiratanga_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, article_ii_maori_text_authority).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, kawanatanga_limited_to_settlers).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, inherent_maori_authority_pre_treaty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Iwi, hapū, and whānau who retain tino rangatiratanga over their lands, resources, and taonga under the Māori text. They are the primary beneficiaries of the reading's vindication — their authority is recognized. However, they also bear costs: the reading demands active exercise of rangatiratanga (which requires capacity they were stripped of), they face Crown resistance to implementation, and their identity is bound to the relationship with land — exit from the constraint means abandoning the constitutional basis of their collective existence. They pay through the labor of revival, litigation, and negotiation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collectives, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collectives, payer).

% The Crown administers the state apparatus and currently exercises de facto sovereignty. Under this reading, it retains kāwanatanga (governorship) over settlers and their affairs — a genuine coordination function managing the non-Māori population. It also benefits from the stability and legitimacy the Treaty provides to its governance. But it pays: implementing rangatiratanga requires transferring resources, decision-making power, and fiscal capacity. Its exit is constrained by constitutional continuity, international law, and the political cost of repudiation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government, beneficiary).

% Non-Māori New Zealanders whose presence and governance the Crown manages under kāwanatanga. They benefit from the stable legal order the Treaty enables, from public services, and from the legitimacy of the state they live under. They do not bear the extraction of the constraint — the reading does not extract from them. Their exit is mobile: individuals can emigrate, but collectively they are the population the Crown governs.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_population, beneficiary,
    organized, biographical, mobile, national).

% The standing commission of inquiry established to interpret the Treaty and recommend redress. It operates as an analytical seat: it hears evidence from all parties, applies the Māori text as authoritative, and its findings structure the negotiation space. It neither collects nor pays under the constraint; it interprets it.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Māori legal scholars (e.g., Moana Jackson, Carwyn Jones, Claire Charters) who articulate the rangatiratanga reading as constitutional theory. They provide the intellectual infrastructure for the reading's vindication. Analytical seat — no material stake in extraction.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, constitutional_scholars_maori, observer,
    analytical, generational, analytical, national).

% Non-Māori legal scholars who engage with the reading (supportive or critical). Analytical seat. Their work shapes the interpretive field but they do not bear costs or collect gains from the constraint's operation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, constitutional_scholars_non_maori, observer,
    analytical, generational, analytical, national).

% The Crown's internal legal advisors who historically advanced the Crown sovereignty reading and resisted Tribunal findings. They would object to the rangatiratanga reading's structural implications (transfer of decision-making, fiscal liability) but are excluded from the Treaty partnership as a party — they are the Crown's instrument, not a Treaty partner. Their exclusion is structural: the Treaty is between Crown and Māori, not between Crown lawyers and Māori.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_law_office, excluded,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional framework for two peoples to coexist on one land base: Māori exercise tino rangatiratanga over their territories and taonga; the Crown exercises kāwanatanga over settlers and their affairs. The coordination solves the problem of how authority is allocated between the pre-existing polity and the incoming one without either extinguishing the other.
% TRANSFER_FUNCTION: Moves decision-making authority over lands, resources, and taonga from unilateral Crown control to Māori collectives (restoring what Article II reserved). Moves fiscal resources from the Crown to Māori (via settlements, co-governance funding, resource revenue sharing). Moves the burden of proof: the Crown must justify any interference with rangatiratanga, rather than Māori proving their rights exist.
% ABSENT_VOICES: Māori who died in the wars and confiscations of the 1860s — their lands were taken, their authority suppressed, and they cannot be here to attest to the reading. Future generations of Māori whose rangatiratanga will be shaped by today's settlements — they are not in the room. The taonga themselves (rivers, forests, language, knowledge) — they have no voice in Westminster-derived legal processes.
% DISAPPEARANCE_RATIONALE: If the rangatiratanga reading vanished overnight, the constitutional basis for Māori authority over their territories would collapse to the Crown sovereignty reading by default. Settlements would lose their legal foundation. Co-governance arrangements (Whanganui River, Te Urewera, Waikato River) would revert to Crown management. The Waitangi Tribunal's jurisdiction would be fundamentally altered. The political order would rearrange around parliamentary supremacy.
% FOUNDING_PROBLEM: How to accommodate British settlement in Aotearoa while preserving Māori authority over their lands, resources, and taonga — the pre-existing polity's full authority — without either people extinguishing the other's existence.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Treaty text itself (Māori version Article II), by the recorded oral understandings of rangatira at signing (documented in Tribunal findings, e.g., Te Paparahi o Te Raki WAI 1040), by the Crown's own 1840 instructions to Hobson (which contemplated Māori retaining authority), and by the United Nations Declaration on the Rights of Indigenous Peoples (2007) — all sources outside the Māori beneficiary set.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).
:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) at the interval end because Treaty settlements have returned some assets and recognized some authority — the reading's vindication reduces extraction. But suppression remains high (0.78) because the Crown retains ultimate legislative supremacy, can override co-governance, and the structural machinery of the state (parliament, courts, bureaucracy) operates on the Crown sovereignty reading by default. Theater ratio is low-moderate (0.22): co-governance arrangements are functional, not performative, but the Crown's rhetorical commitment to 'partnership' often exceeds its structural willingness to cede decision-making. Accessibility collapse is moderate (0.45): alternatives (independent Māori governance, constitutional transformation) are thinkable and advocated but structurally difficult. Resistance is very high (0.82): Māori have resisted extinction of rangatiratanga for 185 years through war, petition, litigation, occupation, and cultural revival.
 *
 * PERSPECTIVAL GAP:
 *   From the Māori seat, the constraint is the lifeline — the constitutional text that affirms their existence as self-determining peoples. The extraction they experience is the gap between the text's promise and the state's performance. From the Crown seat, the constraint is a managed obligation — settlements are fiscal line items, co-governance is a delegation model, the Treaty is a 'partnership' that preserves Crown supremacy. From the settler seat, the constraint is background infrastructure — mostly invisible until a co-governance arrangement affects a local resource. The engine computes these divergences from the structural data; the claim (tangled_rope) reflects the reading's own assessment that both coordination and extraction are real and structurally entangled.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori collectives are dual-positioned: primary beneficiaries of the reading's vindication (their authority is recognized) but also payers — they bear the cost of exercising rangatiratanga after 185 years of suppression, and their identity is fused to the land (identity_locked exit). The Crown is the agenda setter (administers the state, controls the legislative calendar) and a beneficiary (gains legitimacy and stable governance from the Treaty). Settler population is a pure beneficiary — they receive governance and stability without bearing extraction. Crown Law is excluded: they would resist the reading's implications but have no Treaty standing. Observers (Tribunal, scholars) hold analytical seats. Directionality derives from this structure: Māori d is modulated by identity_locked (high target) despite beneficiary role; Crown d is low (beneficiary/agenda setter); settlers d near 0.5 (symmetric coordination).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (accommodating settlement while preserving Māori authority) remains live — settlement continues, resources remain contested, the constitutional order is unresolved. The constraint has not atrophied into a piton: the coordination function is actively exercised (co-governance, settlements, Tribunal process), extraction has declined but suppression persists, and the theater ratio has not collapsed into pure performance. The mandate is not resolved — the arrangement persists because the problem it was built to solve persists, and because Māori resistance prevents the Crown from treating it as obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maori_text_authority_vs_english_text,
    'Is the Māori text Article II legally and constitutionally authoritative over the English text Article I, or do both texts have equal standing?',
    'Waitangi Tribunal findings (WAI 1040 Te Paparahi o Te Raki) establish the Māori text as the one signed by the vast majority of rangatira; international law (Vienna Convention on the Law of Treaties, UN DRIP) favors the text in the language of the indigenous parties. A definitive court ruling or constitutional entrenchment would resolve it.',
    'If Māori text is authoritative, the rangatiratanga reading is the legally correct constraint and Crown sovereignty reading is a misreading. If equal standing, the ambiguity persists and partnership reading gains ground. If English text is authoritative (current de facto state), the rangatiratanga reading is aspirational, not operational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(maori_text_authority_vs_english_text, conceptual, 'Which Treaty text governs the sovereignty allocation — the structural axis of the kernel.').

omega_variable(
    rangatiratanga_vs_sovereignty_translation,
    'Does ''tino rangatiratanga'' equate to ''sovereignty'' in the Western sense, or does it denote a distinct Māori constitutional concept that cannot be mapped to Westminster categories?',
    'Comparative analysis of Māori political philosophy (whakapapa, mana, kaitiakitanga) with Western sovereignty theory; linguistic analysis of ''rangatira'' + ''tanga'' (authority/chieftainship quality) vs. ''sovereignty'' (supreme authority); the Tribunal''s finding that rangatiratanga is ''absolute'' and ''qualified only by the need to exercise it responsibly''.',
    'If distinct concept, the Crown sovereignty reading commits a category error — it translates a Māori concept into a Western one that distorts it. If equivalent, the two texts directly contradict (Article I cedes what Article II retains), making the kernel irresolvable without choosing a text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rangatiratanga_vs_sovereignty_translation, conceptual, 'Whether the core concept of the reading is translatable or incommensurable with the sibling''s core concept.').

omega_variable(
    settler_governance_scope,
    'What is the structural scope of kāwanatanga — does it govern only non-Māori individuals, or does it extend to governing the territory as a whole (including Māori lands) subject to rangatiratanga?',
    'Analysis of 1840 usage: ''kāwanatanga'' was a neologism for ''governorship'' (from ''kāwana'', governor). The Hobson instructions, the missionary translations, and the rangatira''s understanding all point to governance of British subjects. Constitutional practice since 1975 (Tribunal, settlements, co-governance) tests the boundary.',
    'If kāwanatanga is strictly over settlers, the two spheres are jurisdictionally separate (dual sovereignty). If it extends over territory, the Crown retains ultimate authority over land-use decisions affecting Māori lands — the coordination function becomes hierarchical. This determines whether the constraint is genuinely a rope (parallel spheres) or a tangled rope (nested spheres with extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_governance_scope, conceptual, 'The jurisdictional boundary between the two coordination spheres — the structural definition of the tangled rope''s coordination function.').

omega_variable(
    identity_locked_exit_mechanism,
    'Is Māori collectives'' identity_locked exit from the constraint a feature of whakapapa (genealogical belonging that cannot be exited), or is it produced by the Crown''s historical suppression (the constraint itself creates the lock)?',
    'Māori constitutional theory: whakapapa binds people to land and to each other — exit from rangatiratanga would mean exit from being Māori. But the Crown''s suppression (confiscation, native land court, urbanization) severed the material basis of that relationship, making the lock experientially tighter. Distinguish ontological lock from structural lock.',
    'If ontological, the identity_locked exit is a feature of the agent, not the constraint — the constraint cannot ''release'' them. If structural, the lock is partly the constraint''s own doing (suppression created dependency on the Treaty as the only remaining recognition), and full vindication of rangatiratanga would loosen it by restoring the material basis of independent existence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, conceptual, 'Whether the identity-locked exit of Māori collectives is inherent to their polity or produced by the constraint''s history.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1840, 0.05).
narrative_ontology:measurement(wait_tr_t1865, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1865, 0.18).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1900, 0.35).
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1975, 0.28).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(wait_tr_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(wait_tr_t2010, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(wait_tr_t2025, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.12).
narrative_ontology:measurement(wait_be_t1865, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1865, 0.28).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1900, 0.42).
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1975, 0.35).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(wait_be_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2000, 0.22).
narrative_ontology:measurement(wait_be_t2010, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2010, 0.18).
narrative_ontology:measurement(wait_be_t2025, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2025, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.15).
narrative_ontology:measurement(wait_su_t1865, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1865, 0.65).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1900, 0.82).
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1975, 0.75).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1985, 0.72).
narrative_ontology:measurement(wait_su_t2000, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(wait_su_t2010, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(wait_su_t2025, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.08).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, treaty_settlements_process).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, co_governance_arrangements).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_ward_representation).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, freshwater_management_reforms).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, resource_management_act_reform).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, te_urewera_governance).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, whanganui_river_legal_personhood).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waikato_river_co_governance).

% DUAL FORMULATION NOTE:
% This constraint is the rangatiratanga_reading of the waitangi_sovereignty_allocation kernel. The crown_sovereignty_reading claims English text Article I ceded complete sovereignty; the partnership_reading claims an ongoing partnership transcending textual conflict. The three readings share the same referent (the 1840 Treaty) but instantiate different constraints with different ε, different beneficiary/victim structures, and different classifications. This reading has lower extractiveness (0.15) because it measures the standing arrangement from the reading's vindicated state; the crown_sovereignty_reading would author higher extractiveness measuring the same arrangement from its contesting position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, organized, 0.65).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
