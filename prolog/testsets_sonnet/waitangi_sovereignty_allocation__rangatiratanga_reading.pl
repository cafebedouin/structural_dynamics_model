% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Te Tiriti Article II — Tino Rangatiratanga Reading of Sovereignty Allocation
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This story instantiates the rangatiratanga reading of the sovereignty
 *   allocation kernel established at te Tiriti o Waitangi (1840): that the
 *   Māori-language Article II retained full tino rangatiratanga — inherent,
 *   undiminished authority — over lands, resources, and taonga, and that the
 *   Crown gained only kāwanatanga, a limited right to govern the settler
 *   population. Under this reading the subsequent 19th and 20th century
 *   history of land confiscation (raupatu), the Native Land Court's
 *   individualization of communal title, and the exercise of unqualified
 *   parliamentary sovereignty over Māori people and resources are not lawful
 *   exercises of ceded authority but breaches of the actual bargain —
 *   extraction dressed as governance. This is a single, ε-stable reading: it
 *   does not average with or hedge against the sibling readings
 *   (crown_sovereignty_reading, holding the English text ceded complete
 *   sovereignty; partnership_reading, holding the Treaty requires ongoing
 *   good-faith co-governance despite textual ambiguity). Those are separate
 *   constraints with their own ε values, linked here only via network edges.
 *
 * KEY AGENTS:
 *   - maori_iwi_and_hapu: primary target/payer (organized/trapped) — bears the extraction of authority never ceded under this reading
 *   - maori_land_owners_dispossessed: concrete downstream target (powerless/trapped) — carries the generational cost of specific land loss
 *   - crown_settler_state_apparatus: primary beneficiary and agenda_setter (institutional/arbitrage) — exercises and enforces authority beyond kāwanatanga
 *   - settler_landholders_descendants: secondary beneficiary (organized/mobile) — holds title downstream of the disputed exercise of authority
 *   - waitangi_tribunal: analytical/institutional observer with persuasive but non-binding authority
 *   - maori_language_and_legal_scholars: excluded expert voices whose textual corroboration is heard but not binding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.62).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Te Tiriti Article II — Tino Rangatiratanga Reading of Sovereignty Allocation").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '409fc6fd-a7cc-4476-b87b-8f49f61e548c').
narrative_ontology:cs_kernel_codification('409fc6fd-a7cc-4476-b87b-8f49f61e548c', fixed_text).
narrative_ontology:cs_authority_grounding('409fc6fd-a7cc-4476-b87b-8f49f61e548c', lineage).
narrative_ontology:cs_interpretation_layer_present('409fc6fd-a7cc-4476-b87b-8f49f61e548c').
narrative_ontology:cs_reading_relation('409fc6fd-a7cc-4476-b87b-8f49f61e548c', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('409fc6fd-a7cc-4476-b87b-8f49f61e548c', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('409fc6fd-a7cc-4476-b87b-8f49f61e548c', foundational, maori_text_controls_over_english_text).
narrative_ontology:cs_axiom_status(maori_text_controls_over_english_text, holdable).
narrative_ontology:cs_axiom_grounding('409fc6fd-a7cc-4476-b87b-8f49f61e548c', maori_text_controls_over_english_text, conventional).
narrative_ontology:cs_axiom('409fc6fd-a7cc-4476-b87b-8f49f61e548c', foundational, sovereignty_never_ceded_only_governorship).
narrative_ontology:cs_axiom_status(sovereignty_never_ceded_only_governorship, holdable).
narrative_ontology:cs_axiom_grounding('409fc6fd-a7cc-4476-b87b-8f49f61e548c', sovereignty_never_ceded_only_governorship, deontological).
narrative_ontology:cs_axiom('409fc6fd-a7cc-4476-b87b-8f49f61e548c', secondary, kawanatanga_jurisdiction_limited_to_settlers).
narrative_ontology:cs_axiom_status(kawanatanga_jurisdiction_limited_to_settlers, holdable).
narrative_ontology:cs_axiom_grounding('409fc6fd-a7cc-4476-b87b-8f49f61e548c', kawanatanga_jurisdiction_limited_to_settlers, conventional).
narrative_ontology:cs_reference_frame('409fc6fd-a7cc-4476-b87b-8f49f61e548c', tino_rangatiratanga_1840_grant).
narrative_ontology:cs_drift_state('409fc6fd-a7cc-4476-b87b-8f49f61e548c', post_waitangi_tribunal_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('409fc6fd-a7cc-4476-b87b-8f49f61e548c', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_settler_state_apparatus).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_landholders_descendants).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_and_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_owners_dispossessed).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_textual_primacy_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, inherent_indigenous_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Signed te Tiriti o Waitangi in te reo Māori, understanding Article II as retaining tino rangatiratanga — full and undiminished authority — over their whenua (land), resources, and taonga, while ceding only kāwanatanga (a right to govern settlers) to the Crown. Under this reading they were never dispossessed of sovereignty; the subsequent history of land confiscation, the Native Land Court, and unilateral Crown legislative supremacy is a breach of the actual bargain, not an exercise of ceded authority. Their exit from the New Zealand state is not practically available; their leverage runs through Waitangi Tribunal claims, litigation, and political mobilization, none of which restore rangatiratanga as originally understood.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_and_hapu, payer,
    organized, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_and_hapu, excluded).

% Families and hapū whose specific blocks of land were confiscated, alienated through the Native Land Court's individualisation of communal title, or compulsorily acquired — losses that, under the rangatiratanga reading, were never authorized because Māori never ceded authority over land in the first place. They carry the concrete, generational cost of the gap between the textual promise and the Crown's exercised power, with no realistic path to reclaim specific whenua outside settlement processes that themselves proceed under Crown-defined rules.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_owners_dispossessed, payer,
    powerless, generational, trapped, local).

% Exercises full legislative, judicial, and executive sovereignty over the whole territory including Māori land and resources, grounded in the English-text Article I cession and in parliamentary supremacy doctrine. Under the rangatiratanga reading this exercise of authority over Māori affairs and resources exceeds what was actually granted (kāwanatanga over settlers only) and persists through statute, courts, and administrative machinery the Crown itself controls and can amend at will. The Crown sets the terms on which any redress (Tribunal settlements, co-governance arrangements) is offered, and can withdraw or limit that redress unilaterally.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_settler_state_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Hold title to land acquired through confiscation, questionable Crown purchase, or the Native Land Court process that the rangatiratanga reading treats as ultra vires — beyond what the Treaty actually authorized the Crown to enable. They benefit from a century-plus of settled title, mortgages, and inherited property built on that foundation, and have no structural exposure to having that title unwound; their exit options (sell, relocate, diversify) are unconstrained by the sovereignty question.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_landholders_descendants, beneficiary,
    organized, generational, mobile, national).

% A Crown-created body empowered to inquire into and make recommendations on Treaty breaches, including translation and meaning disputes between the English and Māori texts. It can find that the rangatiratanga reading is the more textually and historically defensible one, but its recommendations are not self-executing — the Crown decides whether and how to act on them, which limits the Tribunal to persuasive rather than binding authority over the sovereignty question itself.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Linguists, historians, and tikanga experts who can demonstrate the semantic gap between 'sovereignty' in the English text and 'kāwanatanga' in the Māori text, and who argue rangatiratanga was never surrendered. Their expert testimony features in Tribunal hearings and academic literature but has no direct constitutional force; the state's founding sovereignty claim does not require their concurrence to remain operative in law.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_language_and_legal_scholars, excluded,
    moderate, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the Treaty's genuine coordination function was narrow and real: it authorized the Crown to establish governance and rule of law specifically over the growing settler population, solving a real problem (lawless, unregulated settler conflict and land-grabbing) without requiring Māori to surrender authority over their own people, land, or resources.
% TRANSFER_FUNCTION: The arrangement as actually operated — rather than as textually authorized under this reading — transfers land, resource control, and jurisdictional authority from Māori iwi and hapū to the Crown and, downstream, to settler titleholders, well beyond the narrow governorship the Māori text granted.
% ABSENT_VOICES: Māori language and legal scholars whose textual analysis supports this reading are heard in Tribunal and academic fora but structurally excluded from the constitutional decision-making that determines how sovereignty is actually exercised; their corroboration is persuasive, not binding.
% DISAPPEARANCE_RATIONALE: If the Crown's exercised authority over Māori land and resources were withdrawn to match the kāwanatanga-only reading, the entire framework of New Zealand land title, resource management, and parliamentary jurisdiction over Māori affairs would require renegotiation — iwi and hapū would hold recognized inherent authority requiring either independent governance structures or formal co-governance, and a large share of settled land title would face legitimacy challenges.
% FOUNDING_PROBLEM: Settler colonization was producing unregulated land purchase, inter-settler conflict, and lawlessness that neither Māori nor the British government could tolerate; the Treaty was framed by the Crown as a mechanism to impose order on settlers while, in the Māori-text understanding, leaving Māori authority over their own people and resources intact.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal historical inquiries, independent te reo Māori translation scholarship, and comparative treaty linguistics from outside both the Crown and current Māori claimant groups corroborate that 'kāwanatanga' does not carry the full sovereignty meaning the English 'sovereignty' does — supporting the view that the founding problem (regulating settlers) was narrower than the authority the Crown subsequently exercised. The Crown's own constitutional and legal establishment continues to assert the founding problem was fully resolved by unified sovereignty from 1840, a position outside parties treat as self-serving.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high-moderate (0.62) reflecting that, under this reading, a substantial share of the authority the Crown has actually exercised over Māori land and resources exceeds the kāwanatanga grant — but not total, since some genuine settler-governance coordination function is real and textually authorized. Suppression is authored high (0.78) because maintaining the Crown's exercised sovereignty over Māori affairs required — and continues to require — active legal, legislative, and at points military suppression of the rangatiratanga claim (land wars, confiscation statutes, denial of Māori legal personality claims in early case law, ongoing limits on Tribunal binding force). Accessibility collapse is moderate (0.5): the rangatiratanga reading was never fully extinguished as a live claim — it persists in Tribunal jurisprudence, academic scholarship, and political movements — so alternatives to Crown-exercised sovereignty have not collapsed completely, unlike a genuine mountain. Resistance is authored high (0.82): this reading has been the basis of sustained, organized Māori political and legal resistance for over 180 years, which is precisely the resistance a pure extraction structure generates from those it targets.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown apparatus's seat, its exercised sovereignty over the whole territory is the settled, functioning constitutional order — coordination, not extraction. From the maori_iwi_and_hapu seat under this reading, the same exercised authority is precisely the breach: governance was authorized only over settlers, and everything beyond that is extraction riding on a genealogical claim to legitimacy the Māori text does not support. The engine should compute these as structurally different seat classifications from the same base data, which is the point of authoring per-seat structural facts rather than a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori iwi and hapū, and specifically dispossessed land owners, are declared victims: under the rangatiratanga reading, they are the parties from whom authority and resources were extracted beyond what was textually ceded, and their exit options are trapped (they cannot leave the jurisdiction their land sits within). The Crown apparatus is the primary beneficiary and agenda-setter: it exercises the disputed authority and administers the redress mechanisms (Tribunal settlements) that determine how much correction, if any, occurs. Settler landholders are a secondary, diffuse beneficiary class — they hold title built on the disputed exercise of Crown authority but did not themselves exercise it and have mobile exit options unconstrained by the sovereignty question. This asymmetry — concentrated, structural beneficiary with enforcement power vs. trapped, organized victim group — is why this reads as tangled_rope rather than pure snare: there is a genuine coordination function (settler governance) bundled with the extraction (unauthorized authority over Māori land and resources).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regulating a lawless, land-grabbing settler population — was real and, on this reading, was the actual scope of what Māori consented to address via Crown governance. That problem has long since been resolved (settler society is thoroughly governed), yet the Crown's exercised authority over Māori land and resources, which this reading holds was never part of the original grant, persists and has in some respects intensified through statute and case law rather than receding. This is close to a mandatrophy pattern — a mandate whose stated function (settler governance) is dead while the actual operative structure (comprehensive sovereignty over Māori affairs) continues — except that partial correction mechanisms (the Waitangi Tribunal, settlement processes) exist and are actively used, which is why founding_problem_status is authored as contested rather than flatly dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Is the rangatiratanga reading the historically and linguistically correct interpretation of te Tiriti''s Article II, or is it one contested reading among crown_sovereignty_reading and partnership_reading with no single determinate answer recoverable from the 1840 text and context?',
    'This is likely irreducible: the two-text problem (Māori and English versions saying materially different things, both signed) means no single ''correct'' reading can be recovered by textual analysis alone. Waitangi Tribunal historical inquiry, comparative treaty linguistics, and evolving case law (Court of Appeal, Privy Council, Supreme Court decisions) shift the balance of authority between readings but do not resolve it definitively.',
    'If the rangatiratanga reading were to become the controlling constitutional interpretation, Crown authority over Māori land and resources exercised since 1840 would require wholesale re-legitimation via co-governance or independent Māori governance structures. If crown_sovereignty_reading controls, the current arrangement requires no structural change. Partnership_reading occupies a middle position requiring enhanced consultation without full authority transfer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether one kernel reading is objectively correct or all three remain permanently contested.').

omega_variable(
    maori_beneficiary_absence_and_victim_status,
    'This story authors no beneficiary group among Māori parties — is that accurate, or do some Māori actors (e.g., iwi with successful, well-resourced Treaty settlements, or Māori elected representatives within the Crown apparatus) occupy a partial beneficiary position under the current arrangement even on the rangatiratanga reading?',
    'Disaggregate by iwi/hapū: examine whether post-settlement iwi entities with significant commercial assets (e.g., large fisheries or land settlement trusts) have structural interests more aligned with the status quo than with full rangatiratanga restoration, which could unsettle or redistribute existing settlement assets.',
    'If some post-settlement Māori entities function as partial beneficiaries of the current, less-than-full-rangatiratanga arrangement, the clean payer/beneficiary split authored here understates internal Māori heterogeneity of interest and the classification''s victim declaration would need qualification for those specific entities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maori_beneficiary_absence_and_victim_status, empirical, 'Whether Māori interests are uniform victims or internally differentiated by settlement status.').

omega_variable(
    naturalness_of_crown_sovereignty_claim,
    'Does the Crown''s exercised, comprehensive sovereignty present itself (in New Zealand constitutional and popular discourse) as a natural, settled fact akin to a mountain, when under this reading it is a constructed and contested extraction from an incompletely-ceded authority?',
    'Survey New Zealand constitutional law textbooks, school curricula, and popular civic discourse for the degree to which 1840 sovereignty is presented as an uncontested founding fact versus an acknowledged contested question.',
    'If Crown sovereignty is popularly naturalized despite being, on this reading, a constructed and contested claim, this constraint exhibits false-summit dynamics at the level of the sibling crown_sovereignty_reading constraint rather than this one — but the naturalization itself is part of what suppresses the rangatiratanga reading''s political traction, which bears on the suppression metric authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_crown_sovereignty_claim, conceptual, 'Whether Crown sovereignty is falsely naturalized against a contested constructed origin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 0, 185).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(wait_tr_t0, observed).
narrative_ontology:measurement(wait_tr_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement_basis(wait_tr_t30, observed).
narrative_ontology:measurement(wait_tr_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(wait_tr_t60, observed).
narrative_ontology:measurement(wait_tr_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 90, 0.35).
narrative_ontology:measurement_basis(wait_tr_t90, observed).
narrative_ontology:measurement(wait_tr_t130, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 130, 0.42).
narrative_ontology:measurement_basis(wait_tr_t130, observed).
narrative_ontology:measurement(wait_tr_t160, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 160, 0.4).
narrative_ontology:measurement_basis(wait_tr_t160, observed).
narrative_ontology:measurement(wait_tr_t185, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 185, 0.4).
narrative_ontology:measurement_basis(wait_tr_t185, observed).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(wait_be_t0, observed).
narrative_ontology:measurement(wait_be_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(wait_be_t30, observed).
narrative_ontology:measurement(wait_be_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement_basis(wait_be_t60, observed).
narrative_ontology:measurement(wait_be_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 90, 0.78).
narrative_ontology:measurement_basis(wait_be_t90, observed).
narrative_ontology:measurement(wait_be_t130, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 130, 0.68).
narrative_ontology:measurement_basis(wait_be_t130, observed).
narrative_ontology:measurement(wait_be_t160, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 160, 0.64).
narrative_ontology:measurement_basis(wait_be_t160, observed).
narrative_ontology:measurement(wait_be_t185, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 185, 0.62).
narrative_ontology:measurement_basis(wait_be_t185, observed).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(wait_su_t0, observed).
narrative_ontology:measurement(wait_su_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement_basis(wait_su_t30, observed).
narrative_ontology:measurement(wait_su_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement_basis(wait_su_t60, observed).
narrative_ontology:measurement(wait_su_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 90, 0.88).
narrative_ontology:measurement_basis(wait_su_t90, observed).
narrative_ontology:measurement(wait_su_t130, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 130, 0.82).
narrative_ontology:measurement_basis(wait_su_t130, observed).
narrative_ontology:measurement(wait_su_t160, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 160, 0.79).
narrative_ontology:measurement_basis(wait_su_t160, observed).
narrative_ontology:measurement(wait_su_t185, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 185, 0.78).
narrative_ontology:measurement_basis(wait_su_t185, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal_redress_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_court_title_conversion).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the waitangi_sovereignty_allocation kernel (crown_sovereignty_reading, partnership_reading, rangatiratanga_reading — this file). Each reading has its own ε, beneficiary/victim structure, and classification; they are not averaged or hedged into a single constraint. The rangatiratanga reading is authored here as tangled_rope (genuine settler-governance coordination bundled with authority extraction beyond the textual grant); crown_sovereignty_reading and partnership_reading are expected to authored separately with different ε and likely different claimed types given their different premises about what was actually ceded.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
