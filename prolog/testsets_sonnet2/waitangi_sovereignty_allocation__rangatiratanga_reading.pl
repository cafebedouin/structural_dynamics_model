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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Treaty of Waitangi Article II — Tino Rangatiratanga Reading of Sovereignty Allocation
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   This story instantiates the rangatiratanga reading of the Treaty of
 *   Waitangi kernel: the Maori-language text of Article II, which the great
 *   majority of the roughly 500 rangatira who signed in 1840 actually read
 *   and agreed to, retained tino rangatiratanga (full
 *   chieftainship/authority) over lands, forests, fisheries, and other
 *   taonga, while granting the Crown only kawanatanga (governorship) —
 *   understood by signatories as authority over British subjects and the
 *   maintenance of order, not sovereign title over Maori territory. On this
 *   reading, the colonial and subsequent New Zealand state's assertion of
 *   full sovereignty over Maori land and resources was not authorized by the
 *   instrument Maori actually signed; the divergence between the Maori and
 *   English texts was exploited, not resolved, and the sovereignty the Crown
 *   has exercised since 1840 exceeds its textual grant under this reading.
 *   The extraction referent here is the standing arrangement under contest —
 *   the Crown's actual exercise of sovereign authority over Maori lands and
 *   taonga since 1840 — assessed by this reading's own lights, not the
 *   co-governance or restored-rangatiratanga arrangement this reading would
 *   install instead.
 *
 * KEY AGENTS:
 *   - crown_settler_state: primary agenda-setter and structural beneficiary (institutional/arbitrage) — exercises sovereignty beyond its textual grant on this reading
 *   - hapu_and_iwi: primary target and payer (organized/trapped) — the collective authority holders whose rangatiratanga was displaced
 *   - maori_land_owners: individual-level payer (moderate/trapped) — bear direct land loss through Crown-administered native land law
 *   - kaitiaki_of_taonga: powerless payer (trapped) — cultural guardianship role rendered unenforceable
 *   - pakeha_landholders and colonial_land_purchasers: downstream beneficiaries of the sovereignty claim converted into transferable title
 *   - waitangi_tribunal: analytical/observer seat — a Crown-created body whose findings substantially corroborate this reading's factual premises
 *   - future_maori_generations: excluded voice — bear compounding cost with no seat in the founding allocation
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
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, snare).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Treaty of Waitangi Article II — Tino Rangatiratanga Reading of Sovereignty Allocation").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '8690b535-84b9-406a-9cd7-0062f73a7ef9').
narrative_ontology:cs_kernel_codification('8690b535-84b9-406a-9cd7-0062f73a7ef9', fixed_text).
narrative_ontology:cs_authority_grounding('8690b535-84b9-406a-9cd7-0062f73a7ef9', extraction).
narrative_ontology:cs_interpretation_layer_present('8690b535-84b9-406a-9cd7-0062f73a7ef9').
narrative_ontology:cs_reading_relation('8690b535-84b9-406a-9cd7-0062f73a7ef9', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('8690b535-84b9-406a-9cd7-0062f73a7ef9', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('8690b535-84b9-406a-9cd7-0062f73a7ef9', foundational, maori_text_is_controlling_instrument).
narrative_ontology:cs_axiom_status(maori_text_is_controlling_instrument, holdable).
narrative_ontology:cs_axiom_grounding('8690b535-84b9-406a-9cd7-0062f73a7ef9', maori_text_is_controlling_instrument, conventional).
narrative_ontology:cs_axiom('8690b535-84b9-406a-9cd7-0062f73a7ef9', foundational, sovereignty_over_maori_territory_was_never_ceded).
narrative_ontology:cs_axiom_status(sovereignty_over_maori_territory_was_never_ceded, holdable).
narrative_ontology:cs_axiom_grounding('8690b535-84b9-406a-9cd7-0062f73a7ef9', sovereignty_over_maori_territory_was_never_ceded, empirically_contingent).
narrative_ontology:cs_axiom('8690b535-84b9-406a-9cd7-0062f73a7ef9', secondary, kawanatanga_limited_to_settler_jurisdiction).
narrative_ontology:cs_axiom_status(kawanatanga_limited_to_settler_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('8690b535-84b9-406a-9cd7-0062f73a7ef9', kawanatanga_limited_to_settler_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('8690b535-84b9-406a-9cd7-0062f73a7ef9', dual_text_1840_signing_moment).
narrative_ontology:cs_drift_state('8690b535-84b9-406a-9cd7-0062f73a7ef9', post_wi_parata_judicial_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('8690b535-84b9-406a-9cd7-0062f73a7ef9', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_settler_state).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, pakeha_landholders).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, colonial_land_purchasers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, hapu_and_iwi).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_owners).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, kaitiaki_of_taonga).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, te_reo_maori_textual_primacy).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__rangatiratanga_reading, doctrine_of_contra_proferentem_against_crown).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers land courts, native title legislation, and Crown purchasing agencies; on the rangatiratanga reading it was granted only kawanatanga (governorship over settlers) but has exercised full sovereign authority over Maori lands, resources, and taonga since the 1840s, treating the English-text cession as controlling and reducing the Maori text's tino rangatiratanga guarantee to a matter of policy discretion rather than binding law.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_settler_state, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_settler_state, beneficiary).

% Signed the Maori-language text believing they retained full authority (tino rangatiratanga) over their lands, forests, fisheries, and taonga while ceding only a form of governorship to the Crown over its own subjects. Subsequent confiscation, forced land sales, and the imposition of Crown law over Maori territory transferred effective control away from hapu and iwi despite the textual guarantee; exit from the jurisdiction is not available since the claim is to authority within their own rohe (territory), not departure from it.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, hapu_and_iwi, payer,
    organized, civilizational, trapped, national).

% Individual and whanau-level titleholders whose land was subjected to individualization, partition, and compulsory acquisition under Crown-administered native land law — a legal apparatus that presumes Crown sovereignty over the very land Article II (Maori text) guaranteed as remaining under Maori authority. They bear the direct, concrete cost of the sovereignty allocation dispute.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_land_owners, payer,
    moderate, generational, trapped, regional).

% Customary guardians of taonga (treasured resources, including language, waterways, and sacred sites) whose guardianship role assumed continuing tino rangatiratanga. Loss of effective authority over these resources to Crown-licensed use and extraction has degraded or extinguished many taonga; there is no exit from a guardianship relationship that is cultural rather than proprietary.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, kaitiaki_of_taonga, payer,
    powerless, generational, trapped, regional).

% Settler and subsequent-generation landholders who acquired title to land through Crown purchase, confiscation, or Land Court processes premised on Crown sovereignty over the whole territory. They hold secure, transferable title that depends on the Crown's sovereignty claim being upheld over the rangatiratanga reading.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, pakeha_landholders, beneficiary,
    powerful, generational, mobile, national).

% Historical land companies and syndicates (and their institutional successors) that purchased or were granted land on the assumption of Crown sovereign title, converting contested authority into freehold and leasehold assets that generated ongoing rents.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, colonial_land_purchasers, beneficiary,
    organized, generational, arbitrage, national).

% A Crown-created inquiry body empowered to hear claims against Crown breaches of Treaty principles, including textual divergence between the Maori and English versions. It can recommend but not compel remedy; its findings validate the rangatiratanga reading's factual premises in many reports while operating inside a Crown-sovereignty-assuming legal system.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Bear the compounding, intergenerational consequence of land loss and authority displacement but had no voice in the founding allocation or its subsequent judicial and legislative interpretation; their claim is transmitted only through present-day hapu and iwi advocacy.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, future_maori_generations, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_settler_state).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: On its own terms, the rangatiratanga reading describes an arrangement that would have coordinated two distinct jurisdictions within one territory: Crown authority over its own settler population (kawanatanga) alongside continuing Maori authority over Maori lands, resources, and taonga (tino rangatiratanga) — a genuine plural-sovereignty coordination structure, had it been honored.
% TRANSFER_FUNCTION: As actually operated, the arrangement transfers land, resource control, and governing authority from hapu, iwi, and individual Maori landowners to the Crown and, downstream, to settler landholders and land-purchasing interests — the opposite of what the Maori-text guarantee promised.
% ABSENT_VOICES: Maori signatories to the 1840 Maori-text version were not party to the drafting of the English version subsequently treated as controlling by colonial courts (notably in Wi Parata v Bishop of Wellington, 1877); their understanding of what they signed was excluded from the legal record for over a century. Future generations bearing compounding land loss have no seat in the founding allocation.
% DISAPPEARANCE_RATIONALE: If the Crown-sovereignty allocation as actually enforced were displaced by recognition of the rangatiratanga reading, land title, resource management, and governance authority across large parts of the country would revert toward hapu- and iwi-administered jurisdiction; freehold titles resting on Crown-sovereign purchase chains would become contestable, and settler-state governance structures would require fundamental renegotiation rather than incremental reform.
% FOUNDING_PROBLEM: In 1840 the Crown sought a legal instrument to establish authority sufficient to control settler land purchases and lawlessness among its own subjects, while Maori signatories sought protection of their existing authority, lands, and taonga against unregulated settler encroachment — two different founding problems collapsed into one text.
% FOUNDING_PROBLEM_CORROBORATION: The Waitangi Tribunal, an instrumentality created by the Crown itself, has repeatedly found (e.g. the Te Paparahi o Te Raki inquiry, 2014) that the Maori-text signatories did not cede sovereignty and understood themselves to retain tino rangatiratanga — corroboration from within a Crown-established body, not solely from hapu and iwi claimants. Independent historians and linguists analyzing the 1840 Maori-language text external to any settlement process have reached the same textual conclusion, providing corroboration outside both the Crown and the immediate claimant parties.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.81 at story-level scalar; measurement series shows a rise from 0.35 at signing to a peak near 0.82 during the New Zealand Wars and confiscation era of the 1860s-1900s, then a slow partial decline as Tribunal-era settlements began redistributing some resource control) because, on this reading, the entire post-1840 exercise of Crown sovereignty over Maori land and taonga is unauthorized by the actual instrument signed — every subsequent land transaction, confiscation, and resource regulation compounds a jurisdictional claim the Maori text does not support. Suppression is authored very high in the 1860s-1900s window (New Zealand Wars, raupatu/confiscation, Native Land Court individualization) and has fallen only partially since — Crown sovereignty is still actively maintained by ordinary legal and administrative machinery, not merely historical force. Theater ratio rises over the twentieth century (0.1 to 0.45) as the arrangement shifts from open military/legal suppression toward biculturalism rhetoric, Waitangi Day commemoration, and Tribunal processes that acknowledge historical grievance without restoring the sovereign authority this reading claims was never ceded. Accessibility collapse is moderate (0.4) rather than near-total because the Maori text itself remains available, contested, and increasingly judicially and academically vindicated — the alternative reading has not been fully suppressed from discourse, only from binding legal effect until recently.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown/settler-state sits at the beneficiary end: it exercises the disputed authority and has arbitrage-grade exit (it can adjust policy, litigate, or legislate its way around claims). Hapu and iwi, individual Maori landowners, and kaitiaki of taonga sit at the target end: authority and resources were transferred away from them under a sovereignty claim this reading holds was never granted, and none of them can exit the jurisdiction that exercises authority over their own ancestral territory — trapped exit reflects that the claim concerns authority WITHIN their home territory, not freedom to leave it. Pakeha landholders and colonial land purchasers are secondary beneficiaries: their title derives from and depends upon the Crown sovereignty claim being upheld, so their d sits nearer the beneficiary end despite not being the agenda-setter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem on the Crown's kawanatanga side (the need to regulate settler conduct and land purchasing) may be read as substantially resolved by ordinary statehood — yet the broader sovereign authority the Crown exercises over Maori territory persists at a scope well beyond that founding problem. This is not classified as mandatrophy in the sense of an obsolete-but-harmless leftover: this reading holds the excess authority was extractive from the outset, not merely that it has outlived a narrower original purpose. The Tribunal's corroboration (from within Crown-created machinery) that Maori signatories did not cede sovereignty distinguishes this from a purely self-serving grievance narrative — the founding-problem mismatch (status=contested, verdict=world_rearranges) signals a live capture question rather than a settled coordination story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_primacy_ambiguity,
    'Which text of the Treaty — the Maori-language version signed by the overwhelming majority of rangatira, or the English-language version — should govern the interpretation of what sovereignty was actually ceded?',
    'This is not fully empirically resolvable: it depends on principles of treaty interpretation (contra proferentem against the drafter, the doctrine that ambiguity should be construed against the party that drafted the instrument) that are themselves contested legal-philosophical commitments, though the historical-linguistic record of what signatories understood themselves to be agreeing to is empirically investigable and substantially documented (Ross 1972, Orange 1987, Waitangi Tribunal reports).',
    'If the Maori text is held controlling, the rangatiratanga reading''s factual premises are legally vindicated and the Crown''s exercise of resource and land sovereignty becomes a breach requiring remedy at a scale far exceeding current settlement policy. If the English text controls, this reading''s ε would need to be re-evaluated as a claim of principle rather than of established breach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_primacy_ambiguity, conceptual, 'Which language version of the Treaty is the authoritative referent for what sovereignty was ceded.').

omega_variable(
    kernel_reading_partition_location,
    'Where exactly does this reading diverge structurally from the partnership_reading, given that both readings hold the English-only-cession-of-full-sovereignty account is at least incomplete?',
    'The dividing line is whether the appropriate remedy for textual divergence is INSTITUTIONALIZED PARTNERSHIP within a continuing single Crown sovereignty (partnership_reading) or RESTORATION/RECOGNITION of continuing separate Maori authority over Maori lands and resources (this reading). Legislative and judicial developments — e.g., whether co-governance structures are framed as Crown delegation (partnership) or as recognition of pre-existing, never-ceded authority (rangatiratanga) — would evidence which framing is operative in a given instrument.',
    'If courts and legislation consistently frame Maori authority as Crown-delegated partnership rather than pre-existing and retained, this reading''s claim that authority was never ceded (rather than merely under-protected) becomes harder to sustain as the LIVE controlling legal doctrine, though it remains a defensible historical-textual reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_partition_location, conceptual, 'The precise structural boundary between the rangatiratanga reading and the partnership reading of the same textual divergence.').

omega_variable(
    remedy_scale_uncertainty,
    'If the rangatiratanga reading were adopted as controlling doctrine, what scale of land, resource, and jurisdictional restoration would be required to make it operative rather than merely declaratory?',
    'Comparative analysis of co-governance and land-restoration settlements already implemented (e.g. Te Urewera legal personhood, Whanganui River settlement, iwi-specific Treaty settlements) against the scale implied by full tino rangatiratanga recognition across all traditional territories.',
    'A wide gap between implemented settlements and full-scope recognition would support classifying current arrangements as largely theatrical accommodation of this reading rather than its substantive adoption — consistent with the rising theater_ratio trajectory authored above.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remedy_scale_uncertainty, empirical, 'Whether current partial settlements approximate or merely gesture toward what this reading would require if fully adopted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 1840, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1840, 0.1).
narrative_ontology:measurement(wait_tr_t1863, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1863, 0.15).
narrative_ontology:measurement(wait_tr_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement(wait_tr_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1950, 0.3).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 1985, 0.4).
narrative_ontology:measurement(wait_tr_t2010, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(wait_tr_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1840, 0.35).
narrative_ontology:measurement(wait_be_t1863, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1863, 0.68).
narrative_ontology:measurement(wait_be_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1900, 0.82).
narrative_ontology:measurement(wait_be_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1950, 0.79).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(wait_be_t2010, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(wait_be_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1840, 0.4).
narrative_ontology:measurement(wait_su_t1863, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1863, 0.9).
narrative_ontology:measurement(wait_su_t1900, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1900, 0.85).
narrative_ontology:measurement(wait_su_t1950, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(wait_su_t2010, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(wait_su_t2024, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, partnership_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the waitangi_sovereignty_allocation kernel, decomposed per the ε-invariance principle because the underlying natural-language concept ('what did the Treaty of Waitangi establish') covers structurally distinct claims with different beneficiary/victim structures and different epsilon values. crown_sovereignty_reading holds the English Article I text controls, establishing full Crown/Westminster sovereignty (a reading under which Crown authority is not extractive but foundational — low epsilon from that reading's own lights). partnership_reading holds the Treaty requires ongoing good-faith Crown-Maori partnership without resolving the sovereignty allocation question outright (moderate epsilon, tangled-rope-shaped: partnership genuinely coordinates but the Crown retains disproportionate enforcement power). This constraint (rangatiratanga_reading) holds the Maori text retained full Maori authority and treats the Crown's actual sovereign exercise as substantially unauthorized extraction (high epsilon, snare-shaped from hapu/iwi's position). The three are linked bidirectionally: none is the 'correct' background constraint against which the others are deviations; each is authored as its own constraint with its own stable epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
