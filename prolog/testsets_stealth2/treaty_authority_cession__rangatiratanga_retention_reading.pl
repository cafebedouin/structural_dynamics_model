% ============================================================================
% CONSTRAINT STORY: treaty_authority_cession__rangatiratanga_retention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treaty_authority_cession__rangatiratanga_retention_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: treaty_authority_cession__rangatiratanga_retention_reading
 *   human_readable: Te Tiriti Partnership Compact — Rangatiratanga Retention Reading
 *   domain: constitutional/indigenous_rights/colonial_history
 *
 * SUMMARY:
 *   Te Tiriti o Waitangi (1840), the text most rangatira signed, controls the
 *   treaty's meaning via contra proferentem against the drafting Crown. On
 *   this reading Article 1 delegates kāwanatanga — governance, a scoped
 *   authority coined for the purpose — and Article 2 guarantees tino
 *   rangatiratanga: full chieftainship over lands, villages, and taonga. The
 *   treaty therefore founds a partnership in which Crown action touching
 *   Māori interests is legitimate only with ongoing hapū consent. The
 *   standing arrangement under contest — the treaty-conditioned authority
 *   relationship itself — is what epsilon is authored for, assessed by this
 *   reading's own lights. The claim/metric gap is deliberate: the reading
 *   CLAIMS a coordination partnership, while the authored metrics trace the
 *   arrangement's actual operation — degradation from mutual exchange toward
 *   one-way legitimation across 1860–1940, then partial revival after 1975.
 *   Sibling readings of the same kernel are separate files linked through the
 *   network block; their contest is not folded into this constraint. KEY
 *   AGENTS (by structural relationship): - crown_of_new_zealand:
 *   Agenda-setting party and bearer of the consent condition
 *   (institutional/identity_locked) — administers the order the compact
 *   conditions and collects its legitimation receipts - iwi_and_hapu: Primary
 *   protected party (organized/identity_locked) — hold the retained
 *   rangatiratanga the guarantee secures - british_settlers: Secondary
 *   beneficiary (moderate/mobile) — receive legitimated governance under the
 *   compact - non_signatory_hapu: Excluded voice (organized/identity_locked)
 *   — covered by presumption, never consenting - waitangi_tribunal:
 *   Analytical observer (institutional/analytical) — produces the
 *   interpretive record both principal seats argue from
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treaty_authority_cession__rangatiratanga_retention_reading, 0.28).
domain_priors:suppression_score(treaty_authority_cession__rangatiratanga_retention_reading, 0.15).
domain_priors:theater_ratio(treaty_authority_cession__rangatiratanga_retention_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(treaty_authority_cession__rangatiratanga_retention_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treaty_authority_cession__rangatiratanga_retention_reading, rope).
narrative_ontology:human_readable(treaty_authority_cession__rangatiratanga_retention_reading, "Te Tiriti Partnership Compact — Rangatiratanga Retention Reading").
narrative_ontology:topic_domain(treaty_authority_cession__rangatiratanga_retention_reading, "constitutional/indigenous_rights/colonial_history").

domain_priors:requires_active_enforcement(treaty_authority_cession__rangatiratanga_retention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(treaty_authority_cession__rangatiratanga_retention_reading, '3d1bb973-a9af-4408-9677-7e30de437aaa').
narrative_ontology:cs_kernel_codification('3d1bb973-a9af-4408-9677-7e30de437aaa', fixed_text).
narrative_ontology:cs_authority_grounding('3d1bb973-a9af-4408-9677-7e30de437aaa', lineage).
narrative_ontology:cs_interpretation_layer_present('3d1bb973-a9af-4408-9677-7e30de437aaa').
narrative_ontology:cs_reading_relation('3d1bb973-a9af-4408-9677-7e30de437aaa', treaty_authority_cession__crown_cession_reading, forecloses).
narrative_ontology:cs_reading_relation('3d1bb973-a9af-4408-9677-7e30de437aaa', treaty_authority_cession__biculturalism_reading, influences).
narrative_ontology:cs_reading_relation('3d1bb973-a9af-4408-9677-7e30de437aaa', treaty_authority_cession__retrospective_snare_exposure, influences).
narrative_ontology:cs_axiom('3d1bb973-a9af-4408-9677-7e30de437aaa', foundational, maori_text_controls_via_contra_proferentem).
narrative_ontology:cs_axiom_status(maori_text_controls_via_contra_proferentem, holdable).
narrative_ontology:cs_axiom_grounding('3d1bb973-a9af-4408-9677-7e30de437aaa', maori_text_controls_via_contra_proferentem, conventional).
narrative_ontology:cs_axiom('3d1bb973-a9af-4408-9677-7e30de437aaa', foundational, tino_rangatiratanga_never_ceased).
narrative_ontology:cs_axiom_status(tino_rangatiratanga_never_ceased, holdable).
narrative_ontology:cs_axiom_grounding('3d1bb973-a9af-4408-9677-7e30de437aaa', tino_rangatiratanga_never_ceased, deontological).
narrative_ontology:cs_axiom('3d1bb973-a9af-4408-9677-7e30de437aaa', secondary, crown_legitimacy_requires_hapu_consent).
narrative_ontology:cs_axiom_status(crown_legitimacy_requires_hapu_consent, holdable).
narrative_ontology:cs_axiom_grounding('3d1bb973-a9af-4408-9677-7e30de437aaa', crown_legitimacy_requires_hapu_consent, instrumental).
narrative_ontology:cs_reference_frame('3d1bb973-a9af-4408-9677-7e30de437aaa', maori_text_partnership_compact).
narrative_ontology:cs_drift_state('3d1bb973-a9af-4408-9677-7e30de437aaa', contemporary_post_tribunal_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3d1bb973-a9af-4408-9677-7e30de437aaa', '').
narrative_ontology:cs_kernel_id(treaty_authority_cession__rangatiratanga_retention_reading, treaty_authority_cession).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, iwi_and_hapu).
narrative_ontology:constraint_beneficiary(treaty_authority_cession__rangatiratanga_retention_reading, british_settlers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, crown_of_new_zealand).
narrative_ontology:constraint_victim(treaty_authority_cession__rangatiratanga_retention_reading, iwi_and_hapu).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, contra_proferentem_treaty_canon).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, tiriti_partnership_principle).
narrative_ontology:constraint_vindicates(treaty_authority_cession__rangatiratanga_retention_reading, hapu_rangatiratanga_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Successor to the Crown party that signed Te Tiriti in 1840. Administers the constitutional order the treaty conditions: Parliament, the courts, the settlement machinery, and the public service all operate under authority whose legitimacy, on this reading, is conditional on hapū consent being sought and honored. When it seeks and honors consent it collects legitimation and durable settlements; when it legislates over Māori objection it accumulates delegitimation and claim liability. It cannot walk away: the state's founding narrative, its settlement obligations, and its international standing are fused with the relationship.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, crown_of_new_zealand, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, crown_of_new_zealand, payer).

% The collectives holding tino rangatiratanga over their lands, villages, and taonga as guaranteed in Article 2 of the Māori text. Into the arrangement they paid a scoped delegation — kāwanatanga for governance — and, under the same article, a Crown pre-emption option on land sales. Out of it they receive the guarantee of continued authority and a seat of consent the Crown must consult. Exit is not available in any ordinary sense: whakapapa, whenua, and taonga are constitutive of who they are, so leaving would mean abandoning what the arrangement exists to protect.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, iwi_and_hapu, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(treaty_authority_cession__rangatiratanga_retention_reading, iwi_and_hapu, payer).

% The colonists whose subjection to lawful government is, on this reading, the principal content of the kāwanatanga delegation. They receive an ordered legal regime — courts, titles, commerce — legitimated by agreement rather than conquest. Their numbers and weight grew quickly after 1840, and their demand for land pressed hardest on the guarantee side of the bargain. Emigration remained a realistic alternative throughout, so their position was voluntary in a way the other parties' was not.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, british_settlers, beneficiary,
    moderate, biographical, mobile, national).

% Collectives such as Tūhoe and several Waikato iwi whose rangatira never signed Te Tiriti, yet who live inside the constitutional order the compact is read to found. The partnership's canonical story presumes their coverage; their consent was never sought, and they would contest both the presumption of coverage and any settlement ledger struck on their behalf. Their objection is structurally muted because the order's institutions speak for them by default.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, non_signatory_hapu, excluded,
    organized, generational, identity_locked, regional).

% Permanent commission of inquiry established in 1975 to hear claims that Crown action breached the treaty. It produces the authoritative interpretive record — on the meaning of kāwanatanga and rangatiratanga, on partnership and active protection — that both major parties argue from, and recommends remedies it cannot compel. It holds no land and collects no rents; its product is the written account of where honor and breach lie.
narrative_ontology:constraint_stakeholder(treaty_authority_cession__rangatiratanga_retention_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(treaty_authority_cession__rangatiratanga_retention_reading, crown_of_new_zealand).
narrative_ontology:fixing_cost_class(treaty_authority_cession__rangatiratanga_retention_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of legitimating two authority systems in one territory: how British governance of settlers can coexist with continuing hapū authority. Kāwanatanga is delegated for government of the new order, tino rangatiratanga is guaranteed to continue, and disputes between the two spheres are routed through negotiation and consent rather than conquest.
% TRANSFER_FUNCTION: Moves a scoped slice of governance authority (kāwanatanga) from hapū collectives to the Crown, in exchange for the guaranteed retention of lands, villages, and taonga; additionally moves, via the Article 2 pre-emption clause, a Crown monopoly option on purchasing Māori land.
% ABSENT_VOICES: Non-signatory hapū who never assented yet fall inside the order the compact is read to found; wāhine rangatira, largely absent from the formal deliberations despite a handful of signatories; and future generations whose continuing consent is presumed by inheritance of the compact rather than sought in their own right.
% DISAPPEARANCE_RATIONALE: If the partnership compact vanished overnight, the constitutional basis of Crown legitimacy alongside Māori authority collapses: the Tribunal and settlements regime lose their foundation, co-governance arrangements unwind, and the legitimacy question reverts to conquest-versus-cession — reopening the founding dispute the compact has framed since 1840.
% FOUNDING_PROBLEM: In 1840 Britain needed a lawful basis to govern its subjects in Aotearoa and to preempt private land-speculators and rival annexation; rangatira needed a counterweight to settler lawlessness and a guarantor of their authority over lands and taonga. The compact answered both problems at once.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Lord Normanby's 1839 Colonial Office instructions acknowledge Māori possession and the need for consent-based acquisition; He Whakaputanga (1835) records pre-treaty international recognition of rangatiratanga; and the independent historiography (Orange, Walker) and Waitangi Tribunal reports — a state-created but non-beneficiary commission — attest both the founding problem and its continuing live status in every co-governance dispute.
narrative_ontology:disappearance_verdict(treaty_authority_cession__rangatiratanga_retention_reading, world_rearranges).
narrative_ontology:founding_problem_status(treaty_authority_cession__rangatiratanga_retention_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(treaty_authority_cession__rangatiratanga_retention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(treaty_authority_cession__rangatiratanga_retention_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).
:- end_tests(treaty_authority_cession__rangatiratanga_retention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.28: the compact as operated after revival is a real exchange — governance delegated, guarantee honored through settlements and co-management — carrying residual asymmetries (the pre-emption legacy, unresolved scope disputes). Suppression is 0.15 as a raw structural property, unscaled by power or scope: the compact coerces no participant into remaining and forecloses few alternatives; only extractiveness is scaled by the engine. Theater ends at 0.18: partnership instruments are now mostly functional, with residual ceremonialism. Accessibility_collapse 0.50: unilateral paths are closed by design, while negotiated variation remains wide open — the signature of a working coordination device rather than a natural limit. Resistance 0.60: the arrangement met armed and political resistance across the nineteenth century from parties seeking unilateral authority, and meets sustained political resistance today. The measurement series run on ONE shared grid (eight points, two metrics); suppression is omitted as a series because its enforcement picture is static — the scalar carries it. The trajectory is not cyclical but inflected: monotone degradation to 1940, inflection at 1975 (Treaty of Waitangi Act), recovery thereafter. Identity-lock operates on both principal seats: the Crown's legitimacy narrative is fused with the treaty relationship, and hapū connection to whenua and taonga is constitutive — neither seat can exit without abandoning what the arrangement defines.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the Crown's seat the compact is a self-binding legitimation device: it pays in consent-seeking and collects durable legitimacy, so its experienced burden is real but reciprocated. From the hapū seat the same compact is a guarantee whose entire value depends on Crown honor — during 1860–1940 the guarantee was professed while its object was alienated, so that seat experienced the arrangement as performance over loss. The settler seat experiences pure subsidy: legitimated order at no structural cost, with mobility preserving voluntariness. The Tribunal's analytical seat sees the gap between text and practice directly, which is why its findings drive the revival phase.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: iwi_and_hapu sit near the beneficiary end (d ≈ 0.20 — pulled slightly off zero by the ceded governance slice and the pre-emption cost carried in their secondary payer position); british_settlers sit nearest zero (d ≈ 0.05, arbitrage-grade exit reinforcing subsidy). The Crown derives target-side from its non-beneficiary, identity-locked administration of the order, but its legitimation receipts pull it toward mid-range (d ≈ 0.45): it bears the consent condition AND collects the compact's principal intangible gain. That correction is recorded here as interpretive guidance rather than a directionality_overrides entry because the override surface keys on power_atom alone and would collide with the Tribunal's institutional seat. The Tribunal is analytical-neutral. Gain_flow names the Crown because the extraction historically operated — legitimation drawn while the guarantee went unhonored — accrued demonstrably to that seat, even though the Crown is not listed among the arrangement's beneficiaries: receipt of gain and benefit-from are different facts.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem is live and externally corroborated, so the arrangement's mandate has not outlived its function. The classification guards against both symmetrical errors. First, it prevents the historical extraction record from being read as proof that the compact IS an extraction mechanism — that attribution belongs to the sibling reading that treats the translation asymmetry itself as the mechanism; on this reading the record evidences dishonor of a functioning partnership, which is remediable. Second, it prevents the partnership claim from whitewashing the degradation arc — the metrics carry the 1860–1940 collapse in function and the theater peak at the 1940 centennial, when ceremony was maximal and substance minimal. The post-1975 data distinguish a revived coordination device from inertial performance: theater FELL as function returned, the opposite of the atrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading of kernel treaty_authority_cession; which structural features would change under the sibling readings?',
    'Comparative classification of the sibling files: crown_cession_reading flips the beneficiary/victim structure and raises epsilon; retrospective_snare_exposure reclassifies the translation asymmetry itself as the operative extraction mechanism.',
    'If the cession reading prevailed, this file''s protected parties become counterparties to a completed transfer and the consent requirement dissolves; if the snare-exposure reading prevailed, the arrangement classifies as extraction operating under mistranslation rather than partnership.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer-frame location: one reading among the declared siblings of the treaty-authority kernel.').

omega_variable(
    contra_proferentem_treaty_applicability,
    'Does the contra proferentem canon — ambiguity construed against the drafter — apply to a bilingual treaty between the Crown and rangatira, or is it confined to private contracts?',
    'Doctrinal analysis and appellate treatment: whether New Zealand courts and the Waitangi Tribunal treat the canon as applicable to the treaty''s two texts, and on what grounds.',
    'If the canon does not reach treaties, the retention reading loses its interpretive anchor and the cession reading''s textual premise strengthens; if it applies, Māori-text primacy is doctrinally secured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contra_proferentem_treaty_applicability, conceptual, 'Whether the interpretive canon anchoring Māori-text control extends to treaties.').

omega_variable(
    kawanatanga_scope_of_delegation,
    'Did the rangatira who signed understand kāwanatanga as governance over the incoming settlers only, or as governance over their own people as well?',
    'Oral tradition, William Colenso''s contemporaneous record of signing-day objections, and linguistic analysis of kāwanatanga as a neologism coined for governorship.',
    'If the delegation was understood as settler-government only, even this reading''s low epsilon overstates the cession and the partnership narrows further; if broader, the compact''s exchange was fuller than the narrow reading assumes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_scope_of_delegation, empirical, 'Historical scope of the authority actually delegated under Article 1 of the Māori text.').

omega_variable(
    ongoing_consent_institutionalizability,
    'Can ''ongoing consent'' be institutionalized as steady-state governance (e.g., Matike Mai-style models) without decision paralysis, or is the consent requirement workable only episodically?',
    'Comparative study of co-governance and consent-based arrangements here and abroad; piloted institutional designs and their decision throughput over time.',
    'If institutionalizable, the partnership is stable at low extraction; if only episodic, the arrangement behaves transitionally — closer to a transitional support whose endpoint is settled co-governance machinery.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ongoing_consent_institutionalizability, preference, 'Whether the partnership''s consent requirement can operate as permanent constitutional machinery.').

omega_variable(
    breach_vs_design_attribution,
    'Is the extraction visible in the land-alienation record attributable to dishonor of a good-faith compact, or to the compact''s own design — the translation asymmetry the chiefs could not see through?',
    'Counterfactual analysis: whether a rangatira fully informed of both texts would have signed on the terms the Crown later enforced; systematic comparison of outcomes under the Māori text''s terms versus enforced practice.',
    'If design, this reading''s coordination claim fails and the snare-exposure reading absorbs the classification; if dishonor, the compact stands as a genuine partnership whose breach history is remediable by revival — the position this file authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(breach_vs_design_attribution, conceptual, 'Whether the land-alienation record indicts the compact''s design or its dishonor.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treaty_authority_cession__rangatiratanga_retention_reading, 1840, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(retention_reading_tr_t1840, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement_basis(retention_reading_tr_t1840, observed).
narrative_ontology:measurement(retention_reading_tr_t1860, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1860, 0.2).
narrative_ontology:measurement_basis(retention_reading_tr_t1860, observed).
narrative_ontology:measurement(retention_reading_tr_t1885, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1885, 0.45).
narrative_ontology:measurement_basis(retention_reading_tr_t1885, observed).
narrative_ontology:measurement(retention_reading_tr_t1910, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1910, 0.55).
narrative_ontology:measurement_basis(retention_reading_tr_t1910, observed).
narrative_ontology:measurement(retention_reading_tr_t1940, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1940, 0.6).
narrative_ontology:measurement_basis(retention_reading_tr_t1940, observed).
narrative_ontology:measurement(retention_reading_tr_t1975, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 1975, 0.35).
narrative_ontology:measurement_basis(retention_reading_tr_t1975, observed).
narrative_ontology:measurement(retention_reading_tr_t2000, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(retention_reading_tr_t2000, observed).
narrative_ontology:measurement(retention_reading_tr_t2026, treaty_authority_cession__rangatiratanga_retention_reading, theater_ratio, 2026, 0.18).
narrative_ontology:measurement_basis(retention_reading_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(retention_reading_be_t1840, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1840, 0.3).
narrative_ontology:measurement_basis(retention_reading_be_t1840, observed).
narrative_ontology:measurement(retention_reading_be_t1860, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1860, 0.45).
narrative_ontology:measurement_basis(retention_reading_be_t1860, observed).
narrative_ontology:measurement(retention_reading_be_t1885, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1885, 0.6).
narrative_ontology:measurement_basis(retention_reading_be_t1885, observed).
narrative_ontology:measurement(retention_reading_be_t1910, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1910, 0.62).
narrative_ontology:measurement_basis(retention_reading_be_t1910, observed).
narrative_ontology:measurement(retention_reading_be_t1940, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1940, 0.55).
narrative_ontology:measurement_basis(retention_reading_be_t1940, observed).
narrative_ontology:measurement(retention_reading_be_t1975, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement_basis(retention_reading_be_t1975, observed).
narrative_ontology:measurement(retention_reading_be_t2000, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2000, 0.32).
narrative_ontology:measurement_basis(retention_reading_be_t2000, observed).
narrative_ontology:measurement(retention_reading_be_t2026, treaty_authority_cession__rangatiratanga_retention_reading, base_extractiveness, 2026, 0.28).
narrative_ontology:measurement_basis(retention_reading_be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(treaty_authority_cession__rangatiratanga_retention_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treaty_authority_cession__rangatiratanga_retention_reading, resource_allocation).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, crown_cession_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, biculturalism_reading).
narrative_ontology:affects_constraint(treaty_authority_cession__rangatiratanga_retention_reading, retrospective_snare_exposure).

% DUAL FORMULATION NOTE:
% Constraint family for kernel treaty_authority_cession. The colloquial label 'the Treaty' covers structurally distinct claims, decomposed per the epsilon-invariance principle: crown_cession_reading (English text controls; full cession; different beneficiary/victim structure, higher epsilon), this file (Māori text controls; partnership requiring ongoing consent; low-moderate epsilon), biculturalism_reading (institutionalized dual spheres), and retrospective_snare_exposure (translation asymmetry itself as the extraction mechanism; high epsilon). Each member authors its own epsilon, stakeholders, and classification. Upstream/downstream structure: the retention reading's textual premises supply the baseline that makes the snare-exposure reading's extraction legible, and supply the legitimacy conditions the biculturalism reading's institutions draw on; the cession reading stands in logical opposition to this file's interpretive premise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
