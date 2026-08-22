% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [SUPERSEDED (2020 RECONVERSION)]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia Universal-Heritage Museum Regime (Universal Heritage Reading)
 *   domain: cultural heritage / sovereignty / religious authority
 *
 * SUMMARY:
 *   Between the 1934 Council of Ministers decree and the 2020 reconversion,
 *   the Hagia Sophia's legitimacy was constituted by this reading: the
 *   building as shared human cultural heritage transcending any confessional
 *   or national claim, administered as a state museum. The arrangement solved
 *   a real coordination problem — preserving a fragile, doubly-sacred
 *   monument while opening it to the world — and simultaneously operated a
 *   substantial extraction: it annulled the Ottoman waqf's worship rights,
 *   barred congregational prayer under police enforcement for 86 years,
 *   converted the site into a revenue engine for the state and tourism
 *   sector, and served as the secular republic's flagship ideological sign.
 *   Claim and metrics are authored independently: the claimed type
 *   (tangled_rope) states my structural judgment that genuine coordination
 *   and asymmetric extraction run through the same enforced structure; the
 *   metrics describe the arrangement's actual operation as even the reading's
 *   own critical tradition (critical heritage studies within the universalist
 *   camp) concedes it. KEY AGENTS (by structural relationship):
 *   turkish_state_secular_establishment — agenda-setter and primary
 *   beneficiary (institutional/arbitrage); muslim_worship_claimants — primary
 *   target (organized/trapped); global_tourism_sector — secondary beneficiary
 *   (powerful/mobile); heritage_scholarship_community — secondary beneficiary
 *   (moderate/mobile); international_visitors — net beneficiaries who pay
 *   admission (moderate/mobile);
 *   ecumenical_patriarchate_and_greek_orthodox_diaspora — excluded claimant
 *   (moderate/trapped); unesco_and_foreign_governments — analytical observer
 *   (institutional/analytical). This file instantiates ONLY the
 *   universal_heritage_reading; the sibling readings are separate constraints
 *   with their own epsilon, beneficiary, and victim structures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.78).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.75).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia Universal-Heritage Museum Regime (Universal Heritage Reading)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural heritage / sovereignty / religious authority").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '7408e030-de10-4dfe-9995-a7dcdedba045').
narrative_ontology:cs_kernel_codification('7408e030-de10-4dfe-9995-a7dcdedba045', formalized).
narrative_ontology:cs_authority_grounding('7408e030-de10-4dfe-9995-a7dcdedba045', expertise).
narrative_ontology:cs_interpretation_layer_present('7408e030-de10-4dfe-9995-a7dcdedba045').
narrative_ontology:cs_reading_relation('7408e030-de10-4dfe-9995-a7dcdedba045', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('7408e030-de10-4dfe-9995-a7dcdedba045', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('7408e030-de10-4dfe-9995-a7dcdedba045', foundational, no_confessional_claim_grounds_legitimacy).
narrative_ontology:cs_axiom_status(no_confessional_claim_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('7408e030-de10-4dfe-9995-a7dcdedba045', no_confessional_claim_grounds_legitimacy, deontological).
narrative_ontology:cs_axiom('7408e030-de10-4dfe-9995-a7dcdedba045', secondary, neutral_custodial_stewardship).
narrative_ontology:cs_axiom_status(neutral_custodial_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('7408e030-de10-4dfe-9995-a7dcdedba045', neutral_custodial_stewardship, conventional).
narrative_ontology:cs_reference_frame('7408e030-de10-4dfe-9995-a7dcdedba045', confession_neutral_heritage_stewardship).
narrative_ontology:cs_drift_state('7408e030-de10-4dfe-9995-a7dcdedba045', post_2020_reconversion, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7408e030-de10-4dfe-9995-a7dcdedba045', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, turkish_state_secular_establishment).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, heritage_scholarship_community).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_visitors).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, muslim_worship_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, international_visitors).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, kemalist_secularization_doctrine).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, unesco_universal_value_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored the 1934 conversion decree, administered the building through the museums directorate, collected admission revenue, and drew symbolic capital from presenting the republic as custodian of world patrimony rather than heir to conquest. Its position carried a standing option the other seats lacked: the same state power that created the museum regime could dissolve it, as the 2020 reconversion demonstrated.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_state_secular_establishment, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, turkish_state_secular_establishment, beneficiary).

% Heirs of the Ottoman endowment (waqf) and successors to the congregation that had prayed in the building continuously from 1453 to 1934. The conversion decree annulled their use-rights; organized associations petitioned courts, demonstrated annually on the conquest anniversary, and attempted prayer inside, meeting police enforcement each time. They could worship in the city's other mosques, but the specific claim is bound to this building and could not be exercised anywhere else; the campaign persisted across three generations until the 2020 reversal.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, muslim_worship_claimants, payer,
    organized, generational, trapped, national).

% Tour operators, hotels, guides, and cruise lines built Istanbul itineraries around guaranteed ticketed access to the monument. Revenue flowed whenever the site operated as a museum; their exposure is commercial, and they can reroute product to other destinations if access terms change.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    powerful, biographical, mobile, global).

% Byzantinists, art historians, archaeologists, and conservators gained sustained research access, restoration programs, and publication subject matter under curatorial management. Their stake is epistemic and professional; they work across many sites and institutions and face no lock-in to this one.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, heritage_scholarship_community, beneficiary,
    moderate, generational, mobile, global).

% Pay admission to see the building's full layered history — mosaics, structure, and imperial past — in a single visit, an access configuration unavailable under any single-community use. They bear the ticket price and crowding costs but receive the access the regime exists to provide; individually they hold substitutes among the world's monuments.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_visitors, beneficiary,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, international_visitors, payer).

% Holds the founding-ecclesial claim: the building was the Great Church of Constantinople before 1453. Under the universal-heritage regime its restitution claims had no domestic forum — the community that would press them dwindled to a few thousand in Istanbul, and raising the claim invited suspicion of irredentism. It objects through international channels and is heard nowhere that decides anything; its see cannot relocate away from the building its claim attaches to.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, ecumenical_patriarchate_and_greek_orthodox_diaspora, excluded,
    moderate, civilizational, trapped, global).

% Monitor the World Heritage inscription (1985), issue statements when the site's status changes, and weigh the universal-value doctrine their own instruments helped codify. They observe, document, and remonstrate, but hold no enforcement power over a sovereign state's disposition of the building.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco_and_foreign_governments, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, turkish_state_secular_establishment).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralized conservation and managed public access to a structurally fragile, doubly-sacred monument: preservation science, visitor-flow control, and open access for all comers regardless of faith — notably including non-Muslims, who had been excluded from the interior under mosque exclusivity — solved once, centrally, instead of left to whichever community held the building.
% TRANSFER_FUNCTION: Moves admission revenue and symbolic capital: money from millions of international visitors to the state treasury and the tourism sector; prestige and ideological vindication to the secularist establishment; and worship access away from confessional claimants — chiefly the Muslim congregation whose endowment was annulled — converting prayer space into purchasable heritage experience.
% ABSENT_VOICES: The waqf heirs and worship claimants petitioned and protested but held no seat in any decision forum: their use-rights had been annulled by the very act under contest, so the party with the most direct claim entered the conversation only as an object of administration. The Ecumenical Patriarchate's restitution claim had no domestic forum at all. Had either held standing in 1934, the conversion's uncontested adoption would have been impossible.
% DISAPPEARANCE_RATIONALE: The museum economy around the site (millions of annual visitors and the tour infrastructure serving them), the secularist establishment's flagship symbol, scholarly access programs, and the bar on congregational prayer all depended on the arrangement. When its legal basis was annulled in 2020, each of these rearranged within weeks — the century itself ran the counterfactual.
% FOUNDING_PROBLEM: After the Ottoman collapse, the new republic inherited a conquered cathedral-mosque that anchored rival confessional narratives. The regime was built to disarm the site as a confessional flashpoint: sever it from the waqf, place it under confessionally neutral custody, and recast it as evidence that the new order belonged to universal civilization rather than to any creed or conquest.
% FOUNDING_PROBLEM_CORROBORATION: The 2020 Council of State annulment reasoned from the original waqf deed that the 1934 conversion exceeded executive authority — a judicial organ outside the tourism and secularist beneficiary set attesting the founding mandate had been superseded. Independent Byzantine-Ottoman historiography likewise treats the museum regime as a nation-building artifact of the 1930s rather than a response to a persisting problem. No source outside the beneficiary set attests that the founding problem remained live.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the arrangement's costs fell asymmetrically: worship rights were extinguished outright while revenue and prestige accrued to identifiable seats, and the transfer grew as mass tourism scaled after 1970. Suppression is high (0.75) because persistence depended on active enforcement — police removal of worshippers, judicial refusal of waqf petitions — not on participant preference. Theater is moderate (0.42): conservation and curation were genuinely performed throughout, but a growing share of late-period activity defended the framing itself (narrative maintenance, symbolism, protest policing) rather than the fabric; the value stays below piton territory because the underlying function never atrophied. Accessibility_collapse is 0.6: alternative worship sites abounded and alternative arrangements stayed imaginable internationally, but the specific alternative — worship at this site — was domestically foreclosed for the entire interval. Resistance is 0.7: three generations of organized petitioning, annual demonstrations, and litigation culminating in success. The measurement series run on one shared seven-point grid (1934–2020) so every tracked metric is authored at every examined time point. The suppression_requirement series is deliberately non-monotonic: enforcement intensity spiked at the secularizing founding (0.80), relaxed through the Democrat Party era and the long plateau of routine administration (trough 0.58 in 1990), then re-ratcheted as contestation rose (0.75 by 2020) — an enforcement cycle tracking the strength of challenge, not a steady ratchet.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the arrangement is enlightened stewardship: a fragile monument saved from confessional tug-of-war and gifted to humanity, with the state as disinterested custodian. From the payer seat the identical structure is expropriation: an endowed congregational mosque seized by administrative fiat, its worshippers criminalized for praying in it, its doors priced for foreigners. From the beneficiary seats it is simply reliable access — the precondition of an industry and a discipline. The engine computes these divergent types from the structural data; the divergence between the custodian narrative and the expropriation experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular establishment sits nearest the beneficiary pole (d low): it authored the rules, collected the revenue, banked the ideological signal, and held arbitrage-grade exit — it could and finally did rewrite the arrangement at will. Tourism, scholarship, and visitors derive low-to-moderate d as declared beneficiaries with mobile exit; visitors carry a small payer component through admission and crowding. Muslim worship claimants sit nearest the target pole (d high): they bore the arrangement's defining cost (extinguished worship), their exit is trapped because the claim attaches to this building alone, and their organized resistance met active enforcement. The Patriarchate is excluded rather than seated — its claim was suppressed in principle by the same transcendence axiom but dormant in effect, which the omega variables flag rather than resolve.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — disarming a confessional flashpoint during 1930s nation-building — died with the era that produced it, probably by mid-century; the arrangement persisted four further decades on revenue and ideology. The R5 mismatch (founding_problem_status=dead x disappearance_verdict=world_rearranges) is the zombie signature, and the 2020 annulment confirmed it: a single court ruling and decree dismantled what eight decades of enforcement had held, revealing that the arrangement's late-period persistence rested on captured benefit, not on any live function. The tangled_rope classification prevents mislabeling in both directions: against the snare reading, it preserves the record that conservation was real, universal access was real, and non-Muslims gained entry they had lacked since 1453 — goods a pure-extraction label would erase; against the rope reading, it registers that the same structure extinguished a living worship right and routed the site's yield to seats that never bore its costs. Neither half of the record may be deleted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading of the hagia_sophia_substrate kernel governs the site''s legitimacy — this universal-heritage reading, islamic_sovereignty_reading, or orthodox_restitution_reading?',
    'Sovereign political and judicial decision — resolved de facto by the 2020 Council of State annulment and the presidential reconversion decree; formally reopenable only by future Turkish state action.',
    'Each reading swaps the entire structural signature: this reading''s primary victims (muslim_worship_claimants) become the primary beneficiaries under islamic_sovereignty_reading, while global_tourism_sector and the secular establishment lose their guaranteed access; epsilon, enforcement requirements, and scope all re-derive from scratch.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer-frame allocation: this constraint is one reading of the hagia_sophia_substrate kernel; the sibling readings instantiate structurally different constraints.').

omega_variable(
    symmetric_principle_asymmetric_effect,
    'Did the reading''s bar on confessional claims bind all communities symmetrically in effect, or did it fall almost entirely on the community with live worship claims, given the dormancy of the Orthodox claim?',
    'Compare enforcement and adjudication records across both confessional claimant populations: who was removed for prayer attempts, whose petitions reached court, whose access actually changed over the interval.',
    'If effect-symmetric, the victim set is broader and the neutrality claim carries real weight; if effect-asymmetric, the arrangement''s extraction concentrated on one community and the transcendence framing functioned as selective suppression wearing a universal mask.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetric_principle_asymmetric_effect, empirical, 'Whether the neutrality principle produced symmetric or asymmetric suppression across confessional claimants.').

omega_variable(
    revenue_preservation_cost_ratio,
    'Did admission revenue exceed the site''s conservation and operating costs — pure rent above coordination cost — and by how much?',
    'Audited accounts of the museums directorate and published conservation budgets across the interval.',
    'Determines how much of the measured epsilon is rent versus the genuine price of preservation coordination: a thin margin strengthens the rope component, a wide margin the extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_preservation_cost_ratio, empirical, 'Rent-versus-cost composition of the museum regime''s revenue.').

omega_variable(
    ideological_signal_weight,
    'How much of the arrangement''s persistence was sustained by its value as an ideological signal of secular modernity rather than by revenue or preservation need?',
    'Discourse analysis of elite justification across the interval, correlated with periods when revenue and preservation arguments were weakest yet defense of the regime intensified.',
    'If ideological weight dominates, the late-period profile approaches pure extraction with theatrical maintenance; if revenue dominates, the arrangement is closer to commercial coordination with ideological garnish.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ideological_signal_weight, conceptual, 'Relative weight of ideological signaling in sustaining the regime past its founding purpose.').

omega_variable(
    counterfactual_access_parity,
    'Could equivalent universal access and preservation have been achieved under a shared-use or rotative regime — worship plus museum hours — making the worship ban gratuitous rather than necessary?',
    'Compare functioning shared-use heritage sites of comparable sanctity and visitor volume, and model visitor-flow and conservation outcomes under rotative regimes.',
    'If parity was achievable, the ban was extraction riding on preservation rhetoric; if not, part of the measured extraction is the irreducible price of the coordination actually achieved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_access_parity, empirical, 'Whether the worship ban was necessary to the access-and-preservation coordination the regime delivered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 1934, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t1934, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1934, 0.2).
narrative_ontology:measurement_basis(hagi_tr_t1934, observed).
narrative_ontology:measurement(hagi_tr_t1950, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1950, 0.22).
narrative_ontology:measurement_basis(hagi_tr_t1950, observed).
narrative_ontology:measurement(hagi_tr_t1970, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement_basis(hagi_tr_t1970, observed).
narrative_ontology:measurement(hagi_tr_t1990, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement_basis(hagi_tr_t1990, observed).
narrative_ontology:measurement(hagi_tr_t2005, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2005, 0.33).
narrative_ontology:measurement_basis(hagi_tr_t2005, observed).
narrative_ontology:measurement(hagi_tr_t2015, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(hagi_tr_t2015, observed).
narrative_ontology:measurement(hagi_tr_t2020, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 2020, 0.42).
narrative_ontology:measurement_basis(hagi_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(hagi_be_t1934, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1934, 0.55).
narrative_ontology:measurement_basis(hagi_be_t1934, observed).
narrative_ontology:measurement(hagi_be_t1950, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement_basis(hagi_be_t1950, observed).
narrative_ontology:measurement(hagi_be_t1970, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement_basis(hagi_be_t1970, observed).
narrative_ontology:measurement(hagi_be_t1990, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement_basis(hagi_be_t1990, observed).
narrative_ontology:measurement(hagi_be_t2005, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2005, 0.73).
narrative_ontology:measurement_basis(hagi_be_t2005, observed).
narrative_ontology:measurement(hagi_be_t2015, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2015, 0.76).
narrative_ontology:measurement_basis(hagi_be_t2015, observed).
narrative_ontology:measurement(hagi_be_t2020, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 2020, 0.78).
narrative_ontology:measurement_basis(hagi_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t1934, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1934, 0.8).
narrative_ontology:measurement_basis(hagi_su_t1934, observed).
narrative_ontology:measurement(hagi_su_t1950, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1950, 0.7).
narrative_ontology:measurement_basis(hagi_su_t1950, observed).
narrative_ontology:measurement(hagi_su_t1970, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1970, 0.62).
narrative_ontology:measurement_basis(hagi_su_t1970, observed).
narrative_ontology:measurement(hagi_su_t1990, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement_basis(hagi_su_t1990, observed).
narrative_ontology:measurement(hagi_su_t2005, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2005, 0.66).
narrative_ontology:measurement_basis(hagi_su_t2005, observed).
narrative_ontology:measurement(hagi_su_t2015, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement_basis(hagi_su_t2015, observed).
narrative_ontology:measurement(hagi_su_t2020, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement_basis(hagi_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, resource_allocation).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the hagia_sophia_substrate kernel per the epsilon-invariance principle: one physical substrate, three legitimacy regimes, each a separate constraint story with its own epsilon, beneficiary/victim structure, and enforcement profile. The colloquial label 'the Hagia Sophia question' conflates them; measuring the substrate through one observable (tourism revenue) yields different epsilon than measuring through another (worship rights), which is the signature that decomposition is required. This story is the universal-heritage member; the islamic_sovereignty_reading and orthodox_restitution_reading files carry their own structural deltas. Family members are linked bidirectionally through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
