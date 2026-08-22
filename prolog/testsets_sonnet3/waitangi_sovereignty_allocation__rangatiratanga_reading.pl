% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Te Tiriti Article II — Tino Rangatiratanga Reading of Sovereignty Allocation
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This story instantiates the rangatiratanga reading of the Waitangi
 *   sovereignty-allocation kernel: the constraint the Crown-derived legal
 *   order actually enforces, evaluated against what the Māori-language text
 *   of Article II says it retained. Under this reading, iwi and hapu retained
 *   full authority (tino rangatiratanga) over lands, fisheries, forests, and
 *   taonga, and the Crown obtained only kāwanatanga — governorship over
 *   settlers. The ε authored here is for the STANDING ARRANGEMENT under
 *   contest (the Crown-sovereignty-premised legal and land-tenure system as
 *   it has actually operated since 1840), assessed by the rangatiratanga
 *   reading's own lights — not for the co-governance arrangement this reading
 *   would install if honored. That is why extraction is high: from this
 *   reading's premises, nearly every subsequent land alienation, resource
 *   concession, and jurisdictional assertion by the Crown over Māori
 *   territory is an act exceeding the authority actually granted.
 *
 * KEY AGENTS:
 *   - crown_government: agenda-setter administering the contested authority as though fully ceded
 *   - maori_iwi_and_hapu: primary payers under this reading, holding textually-retained authority that is not honored in practice
 *   - settler_land_title_holders and extractive_resource_industries: downstream beneficiaries of Crown-administered land and resource systems
 *   - waitangi_tribunal: Crown-created observer body with no power to bind, itself an artifact of the contest
 *   - future_maori_generations: bear the compounding cost of unresolved allocation across time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.81).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.78).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Te Tiriti Article II — Tino Rangatiratanga Reading of Sovereignty Allocation").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '6073802b-fc46-4627-adbf-93539439a8c3').
narrative_ontology:cs_kernel_codification('6073802b-fc46-4627-adbf-93539439a8c3', fixed_text).
narrative_ontology:cs_authority_grounding('6073802b-fc46-4627-adbf-93539439a8c3', extraction).
narrative_ontology:cs_interpretation_layer_present('6073802b-fc46-4627-adbf-93539439a8c3').
narrative_ontology:cs_reading_relation('6073802b-fc46-4627-adbf-93539439a8c3', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('6073802b-fc46-4627-adbf-93539439a8c3', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('6073802b-fc46-4627-adbf-93539439a8c3', foundational, authority_over_taonga_is_inherent_and_unceded).
narrative_ontology:cs_axiom_status(authority_over_taonga_is_inherent_and_unceded, holdable).
narrative_ontology:cs_axiom_grounding('6073802b-fc46-4627-adbf-93539439a8c3', authority_over_taonga_is_inherent_and_unceded, deontological).
narrative_ontology:cs_axiom('6073802b-fc46-4627-adbf-93539439a8c3', foundational, kawanatanga_is_limited_to_settler_administration).
narrative_ontology:cs_axiom_status(kawanatanga_is_limited_to_settler_administration, holdable).
narrative_ontology:cs_axiom_grounding('6073802b-fc46-4627-adbf-93539439a8c3', kawanatanga_is_limited_to_settler_administration, conventional).
narrative_ontology:cs_reference_frame('6073802b-fc46-4627-adbf-93539439a8c3', id_1840_maori_text_as_operative_agreement).
narrative_ontology:cs_drift_state('6073802b-fc46-4627-adbf-93539439a8c3', post_waitangi_tribunal_era_contemporary, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('6073802b-fc46-4627-adbf-93539439a8c3', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_land_title_holders).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, extractive_resource_industries).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_and_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_claimants).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, future_maori_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, non_maori_settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the constitutional order, courts, land registration, and resource management statutes as though the English text's cession of complete sovereignty were the operative agreement, treating kāwanatanga as synonymous with full governmental authority over Māori as well as settlers. Sets legislation, adjudicates disputes through Crown-created tribunals, and retains final say over remedy design and pace.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_government, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Hold, under the Māori text they signed, full unceded authority (tino rangatiratanga) over their lands, fisheries, forests, and taonga, with the Crown limited to governance of settler populations. In practice, iwi and hapu have had that authority progressively displaced by confiscation, forced sale, and Crown legislative override, and now must petition a Crown-run tribunal for partial, non-binding recognition rather than exercise the authority the text names as theirs. Exit is not available — the territory itself is the claim.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_and_hapu, payer,
    organized, civilizational, trapped, regional).

% Hold freehold title to land alienated from Māori ownership under the Crown-sovereignty operating assumption. Their security of title depends on the rangatiratanga reading NOT being operationalized as governing law; they can sell, subdivide, or leave the land without losing standing, unlike the iwi whose relationship to the whenua is constitutive of identity.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_land_title_holders, beneficiary,
    powerful, generational, mobile, national).

% Hold mining, forestry, and fishing concessions granted under Crown authority over resources the Māori text places under iwi rangatiratanga. Can relocate capital or divest if regulatory friction increases; bear none of the identity cost of contested authority.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, extractive_resource_industries, beneficiary,
    powerful, biographical, arbitrage, national).

% Inherit whatever remains of the land, fisheries, and taonga base after each generation's confiscations and settlements; the compounding effect of unresolved sovereignty allocation is that each generation negotiates from a smaller base than the rangatiratanga the Treaty text names as theirs by right, not by grant.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, future_maori_generations, payer,
    powerless, civilizational, trapped, regional).

% A Crown-created body empowered only to recommend, not bind; it can find that the Crown breached the principles of the Treaty (usually approximating the partnership reading) but has no jurisdiction to declare that rangatiratanga was retained in full or to order its restoration. Its findings are persuasive, not dispositive — the rangatiratanga reading itself has never been adopted as the operative constitutional premise by any court or legislature.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, excluded).

% Under the rangatiratanga reading, the Crown's kāwanatanga was meant to extend only over settlers, not over Māori and their territories. Settlers received the ordered civil administration this authority was meant to provide (courts, currency, land registries) without needing to fight for it, while the corresponding Māori half of the bargain — retained authority — was not honored to the same degree.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, non_maori_settler_population, beneficiary,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, diffuse).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Correctly implemented, the rangatiratanga reading would coordinate two separate governance domains under one Crown-Māori compact: Crown authority to organize settler civil life (kāwanatanga), and iwi/hapu authority to govern their own lands, resources, and people (tino rangatiratanga) — a genuine partition-of-authority solving the coordination problem of two populations sharing a set of islands without one having to fully absorb the other.
% TRANSFER_FUNCTION: As actually operated, the arrangement moves land, fisheries, forests, minerals, and political authority from iwi and hapu — who signed for retained rangatiratanga — to the Crown and to settler and corporate beneficiaries who obtained title and concessions under the competing (English-text) assumption of ceded sovereignty. The transfer runs from the Māori signatories to the government and downstream economic actors who benefit from Crown-administered land and resource systems.
% ABSENT_VOICES: Iwi and hapu who signed te Tiriti o Waitangi in 1840 are dead; their descendants speak through the Waitangi Tribunal, but the Tribunal cannot bind Parliament or the courts. Māori voices asserting the rangatiratanga reading as literal, operative law — not merely a 'principle' to be balanced — are structurally excluded from constitutional adjudication, which proceeds from the Crown-sovereignty premise as its starting assumption.
% DISAPPEARANCE_RATIONALE: If the current Crown-sovereignty operating premise were replaced overnight by the rangatiratanga reading as literal law, land title, resource management law, local government structures, and the entire architecture of Crown authority over Māori territories would need to be renegotiated or dissolved — iwi and hapu would resume direct governing authority over lands and taonga the Crown currently administers. This is not a symbolic change; it reallocates concrete governmental power and property.
% FOUNDING_PROBLEM: In 1840 the Crown needed a legal basis to control the growing, often lawless settler population and to forestall other imperial powers (particularly France) from claiming the islands, while iwi and hapu rangatira needed protection from settler encroachment and inter-iwi conflict exacerbated by muskets and land speculation. Te Tiriti was presented to rangatira, in Māori, as an arrangement granting the Crown only administrative authority over its own settlers in exchange for protection of Māori authority and possessions.
% FOUNDING_PROBLEM_CORROBORATION: Prominent Māori legal scholars (e.g. Moana Jackson, Ani Mikaere) and the Waitangi Tribunal's own Te Paparahi o Te Raki report (Ngāpuhi, 2014) — a body established by the Crown itself, not by Māori claimants — corroborate that the rangatira who signed the Māori text did not cede sovereignty and understood themselves to be retaining rangatiratanga. This corroboration comes from a Crown-created inquiry body examining historical evidence, not solely from Māori claimant testimony, though the Crown has not adopted the finding as binding constitutional premise.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction rises sharply from 1840 (0.35, when the text was newly signed and Crown capacity was weak) through the confiscation and native-land-court era to a peak around 1900-1950 (0.85-0.88, the height of land alienation and legislative suppression of Māori political authority), then eases somewhat post-1975 with the establishment of the Waitangi Tribunal (0.75 by 1990) before rising again as unresolved historical claims accumulate against a shrinking remaining land base and increasing pressure over freshwater and foreshore/seabed rights (0.81 by 2024). Suppression tracks similarly, peaking during the era of direct legislative override (Native Land Acts, confiscation legislation) and easing but not disappearing once formal apology and settlement processes began — Crown sovereignty over the disputed territory is still enforced by ordinary law, courts, and police, it is simply less visible. Theater ratio rises over time as Crown apologies, Treaty settlements, and Waitangi Day commemorations increase in volume without a corresponding restoration of the rangatiratanga this reading asserts was never ceded — commemorative and symbolic activity substituting for jurisdictional change.
 *
 * DIRECTIONALITY LOGIC:
 *   Maori iwi and hapu are the structural targets under this reading: the authority the text names as theirs is exercised instead by the Crown, and their exit options are trapped because the claim is inseparable from specific territory and identity (whenua, whakapapa). The Crown, settler titleholders, and extractive industries are beneficiaries: they possess, administer, or profit from land and resources that under the Māori text remained under iwi rangatiratanga, and unlike iwi they can exit specific parcels, capital positions, or (for settlers/industry) relocate without losing their claim to belong. The Waitangi Tribunal occupies an unusual analytical-but-excluded position: it can see and document the rangatiratanga reading's evidentiary basis but has no power to operationalize it, making it a structurally contained venting mechanism rather than a genuine remedy pathway.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing an orderly basis for settler governance while protecting Māori authority — has not disappeared; if anything it has intensified as resource scarcity, freshwater rights, and foreshore/seabed contests make the underlying sovereignty question more consequential, not less. This blocks any claim that the arrangement is obsolete infrastructure being maintained by inertia (a piton read); this reading instead identifies active, ongoing extraction under continuing enforcement, which is why tangled_rope (real coordination premise + asymmetric extraction + active enforcement) rather than snare is the claimed type — a genuine coordination function (dual governance) was contemplated by the text, but the actual operation systematically diverts the Māori half of that bargain to Crown and settler benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    translation_fidelity_ambiguity,
    'Does the semantic gap between ''kāwanatanga'' (governorship) in the Māori text and ''sovereignty'' in the English text reflect a deliberate mistranslation by Crown drafters, an unavoidable translation difficulty given no Māori equivalent for European sovereignty concepts existed in 1840, or a good-faith attempt at conceptual bridging that simply failed?',
    'Historical-linguistic analysis of contemporaneous missionary correspondence, drafting history of Henry Williams'' translation, and comparison to how ''kāwanatanga'' was used in other 1830s-40s Māori-language documents and missionary texts.',
    'Deliberate mistranslation would support treating the Māori text as the sole authentic expression of rangatira intent and the English text as void for fraud; good-faith translation failure would support the partnership reading''s search for a middle interpretation; unavoidable conceptual gap would support treating both texts as equally authoritative but structurally irreconcilable, which is itself the kernel''s central problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_fidelity_ambiguity, empirical, 'Whether the Māori/English textual divergence was deliberate, negligent, or an unavoidable translation gap.').

omega_variable(
    which_reading_is_the_kernel_baseline,
    'Should the rangatiratanga reading be treated as recovering the ORIGINAL agreement (making everything since 1840 a departure requiring correction) or as one CONTESTED interpretation with equal standing to crown_sovereignty and partnership readings (making the current arrangement a live three-way dispute rather than a corrected/uncorrected binary)?',
    'This is fundamentally a question of constitutional theory and legal philosophy (originalism vs. living-document vs. plurality-of-legitimate-readings), not resolvable by historical fact alone, though historical evidence about signatory understanding is relevant input.',
    'If rangatiratanga is the authoritative original agreement, the entire subsequent legal order is built on an ultra vires foundation and radical restructuring is compelled by fidelity to the actual bargain. If it is one contested reading among three, the current arrangement is properly characterized as an unresolved constitutional dispute rather than a violation with a determinate correct answer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(which_reading_is_the_kernel_baseline, conceptual, 'Whether this reading is privileged as textually original or symmetric with its siblings as one contested interpretation.').

omega_variable(
    co_governance_operationalization_uncertainty,
    'If the rangatiratanga reading were adopted as operative constitutional premise, what institutional form would restored Māori authority actually take — full independent jurisdiction, formal co-governance structures, or a confederation model — and would this eliminate or merely reconfigure the beneficiary/victim structure authored here?',
    'Comparative analysis of indigenous self-governance models internationally (e.g. Sámi Parliament, First Nations self-government agreements in Canada) combined with iwi/hapu-led constitutional proposals (e.g. Matike Mai Aotearoa report) to identify what operationalization Māori communities themselves have proposed.',
    'Different operationalizations would produce different beneficiary/victim structures in a hypothetical ''restored'' constraint; this omega does not affect the ε authored here (which is about the standing arrangement, not the endorsed alternative) but bears on any future story modeling a restored-rangatiratanga arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(co_governance_operationalization_uncertainty, preference, 'Uncertainty about the institutional form restored rangatiratanga would take, relevant to future stories but not to this one''s ε.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(wait_tr_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(wait_tr_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(wait_tr_t1990, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.35).
narrative_ontology:measurement(wait_be_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1870, 0.68).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1900, 0.85).
narrative_ontology:measurement(wait_be_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1950, 0.88).
narrative_ontology:measurement(wait_be_t1990, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2024, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.3).
narrative_ontology:measurement(wait_su_t1870, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1870, 0.75).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1900, 0.88).
narrative_ontology:measurement(wait_su_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1950, 0.82).
narrative_ontology:measurement(wait_su_t1990, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.1).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the waitangi_sovereignty_allocation kernel, each with its own ε, stakeholder set, and claimed type. crown_sovereignty_reading authors near-zero extraction for the identical historical arrangement because it treats Crown authority over Māori as legitimately ceded. partnership_reading authors moderate extraction, treating the arrangement as a breached fiduciary/partnership duty rather than an outright authority displacement. rangatiratanga_reading (this story) authors the highest extraction because it treats nearly the entire subsequent exercise of Crown authority over Māori lands and people as exceeding the kāwanatanga actually granted. The three stories are not measurements of one constraint from different angles — they are three structurally distinct constraints sharing a textual kernel, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
