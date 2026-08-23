% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrow Affirmative Defense (Property-Preservation Reading)
 *   domain: legal/intellectual_property/economic
 *
 * SUMMARY:
 *   This story authors the narrow-defense reading of fair use as an operating
 *   arrangement: copyright treated as property in expressive works,
 *   unauthorized use presumptively actionable, the statutory exception
 *   administered as an affirmative defense the user must prove, with market
 *   substitution the controlling consideration and commercial character
 *   weighing heavily against the use. Under this reading the practical
 *   universe of uncompensated use shrinks to what a defendant can afford to
 *   argue for; everything else routes through licensing. The arrangement
 *   coordinates genuinely - exclusivity funds expensive creative production -
 *   and it also transfers steadily: presumption plus pleading burden converts
 *   legal ambiguity into licensing leverage and settlement revenue. Claim and
 *   metrics are authored independently: the claimed type states the structure
 *   I judge true (real coordination fused with asymmetric transfer, held up
 *   by active enforcement); the metrics describe how the arrangement actually
 *   operates at the end of the observed interval. Per the epsilon-referent
 *   rule, extractiveness is assessed on the standing arrangement - this
 *   reading's own regime - by this reading's own lights: licensing of
 *   genuinely substitutive uses counts here as legitimate property
 *   transaction, while leverage-pricing of uses the doctrine itself concedes
 *   (criticism, commentary, scholarship) and litigation-cost transfers count
 *   as extraction even on the reading's internal accounting.
 *
 * KEY AGENTS:
 *   - major_content_publishers: Primary beneficiary (institutional/arbitrage) - collects licensing and settlement flows, co-writes the enforcement agenda
 *   - rights_management_collectives: Secondary beneficiary (organized/mobile) - blanket-licensing universe scales with the exception's narrowness
 *   - clearance_litigation_complex: Ancillary beneficiary (organized/mobile) - fee volume tracks enforcement volume and ambiguity
 *   - federal_courts_copyright_office: Agenda setter (institutional/analytical) - administers the four factors, reweights the doctrine at landmarks
 *   - independent_creators_documentarians: Primary target (powerless/trapped) - bears presumption and pleading burden at full weight
 *   - digital_archives_libraries: Institutional target (organized/constrained) - corpus-scale copying presumptively actionable, missions non-negotiable
 *   - ai_developers_search_engines: Powerful target (powerful/constrained) - corpus dependence meets licensing demands; size dampens the burden
 *   - downstream_audiences_public: Diffuse beneficiary-payer (powerless/trapped) - receives funded output, absorbs priced-out works
 *   - unrepresented_user_communities: Excluded voice (powerless/trapped) - the doctrine's conceded use classes, absent from the table
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.72).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.71).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrow Affirmative Defense (Property-Preservation Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "legal/intellectual_property/economic").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '87975e76-905c-4f08-98cb-5b791d438252').
narrative_ontology:cs_kernel_codification('87975e76-905c-4f08-98cb-5b791d438252', fixed_text).
narrative_ontology:cs_authority_grounding('87975e76-905c-4f08-98cb-5b791d438252', lineage).
narrative_ontology:cs_interpretation_layer_present('87975e76-905c-4f08-98cb-5b791d438252').
narrative_ontology:cs_reading_relation('87975e76-905c-4f08-98cb-5b791d438252', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('87975e76-905c-4f08-98cb-5b791d438252', fair_use_statutory_exception__market_licensing_reading, coexists_with).
narrative_ontology:cs_axiom('87975e76-905c-4f08-98cb-5b791d438252', foundational, unauthorized_use_presumptive_infringement).
narrative_ontology:cs_axiom_status(unauthorized_use_presumptive_infringement, holdable).
narrative_ontology:cs_axiom_grounding('87975e76-905c-4f08-98cb-5b791d438252', unauthorized_use_presumptive_infringement, conventional).
narrative_ontology:cs_axiom('87975e76-905c-4f08-98cb-5b791d438252', foundational, market_substitution_controls_adjudication).
narrative_ontology:cs_axiom_status(market_substitution_controls_adjudication, holdable).
narrative_ontology:cs_axiom_grounding('87975e76-905c-4f08-98cb-5b791d438252', market_substitution_controls_adjudication, empirically_contingent).
narrative_ontology:cs_axiom('87975e76-905c-4f08-98cb-5b791d438252', secondary, commercial_character_weights_against_use).
narrative_ontology:cs_axiom_status(commercial_character_weights_against_use, holdable).
narrative_ontology:cs_axiom_grounding('87975e76-905c-4f08-98cb-5b791d438252', commercial_character_weights_against_use, empirically_contingent).
narrative_ontology:cs_reference_frame('87975e76-905c-4f08-98cb-5b791d438252', property_defense_four_factor_frame).
narrative_ontology:cs_drift_state('87975e76-905c-4f08-98cb-5b791d438252', contemporary_ai_licensing_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('87975e76-905c-4f08-98cb-5b791d438252', '2026-08-12T17:04:00Z').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, major_content_publishers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, rights_management_collectives).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, clearance_litigation_complex).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, independent_creators_documentarians).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, digital_archives_libraries).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, ai_developers_search_engines).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, downstream_audiences_public).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, downstream_audiences_public).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, copyright_incentive_theory).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, expressive_works_property_analogy).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, market_substitution_test).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and administer large catalogs of books, recordings, films, and journalism. Every unauthorized use of catalog material opens a licensing negotiation they control, and because the user must prove a defense rather than the owner disprove one, most approaches arrive as offers rather than assertions. Trade associations they fund lobby legislatures and commission the litigation that sets enforcement norms. Revenue arrives as license fees, settlements, and statutory-damage awards. Exit is easy in practice: catalogs can be domiciled anywhere, sold, or moved onto direct-to-consumer platforms.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, major_content_publishers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, major_content_publishers, agenda_setter).

% Blanket-licensing bodies, reproduction rights organizations, and stock-image agencies pool member rights and sell access to them. The narrower the space for uncompensated use, the larger the universe of acts that must route through a license; member distributions scale accordingly. They operate across borders and can shift repertoire between jurisdictions when enforcement conditions change.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, rights_management_collectives, beneficiary,
    organized, generational, mobile, continental).

% Rights-clearance firms, specialist law practices on both sides of infringement disputes, and anti-piracy vendors. Fee volume tracks enforcement activity and legal ambiguity: a regime in which users must prove their case generates steady demand for clearance services, defense work, and demand-letter campaigns. Practitioners are credentialed specialists whose professional norms treat exhaustive clearance as diligence rather than overhead.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, clearance_litigation_complex, beneficiary,
    organized, biographical, mobile, national).

% Circuit courts and the Supreme Court apply the four statutory factors case by case; the Copyright Office issues studies and guidance. Their rulings determine what the exception covers in practice, and each landmark decision reweights the factors for the following decade. They sit outside the payment flows and can in principle reweight the doctrine unilaterally, though landmark reweightings reliably invite legislative and lobbying response.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, federal_courts_copyright_office, agenda_setter,
    institutional, generational, analytical, national).

% Essayists, documentary filmmakers, sampling musicians, and fan-scale creators whose work quotes, clips, or builds on existing material. Each project lives or dies on whether its borrowings survive scrutiny; the cost of proving a defense routinely exceeds project budgets, so the practical choice is a license priced beyond reach or abandonment of the scene. There is no comparable channel that accepts the same borrowings at lower risk, and leaving the field means not making the work.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, independent_creators_documentarians, payer,
    powerless, biographical, trapped, national).

% Research libraries, mass-digitization projects, and web archives copy at corpus scale for preservation, access, and computational analysis. Each act of copying is presumptively actionable, so they litigate defensively and carry settlement and risk costs as a standing budget line. Their governing institutions are durable and their missions are non-negotiable, but the copying itself depends on continued judicial tolerance.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, digital_archives_libraries, payer,
    organized, generational, constrained, continental).

% Search index builders, large-model developers, and dataset companies ingest copyrighted text and images at scale. Ingestion is presumptively actionable under the prevailing reading, so they face licensing demands, litigation, and product-delay risk. The largest firms negotiate site-wide licenses and multi-jurisdiction structures that convert the exposure into a manageable operating cost; smaller labs meet the same demands without the balance sheet.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, ai_developers_search_engines, payer,
    powerful, generational, constrained, global).

% Readers, viewers, listeners, and researchers receive the stream of funded creative work that the exclusivity system pays for, and pay for it again inside retail prices and platform subscriptions. They also absorb the works that never get made or never reach them because a quotation, clip, or training example was priced out of existence. They cannot opt out of the information environment and hold no organized seat in policy negotiation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, downstream_audiences_public, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, downstream_audiences_public, payer).

% Educators assembling course materials, disability-access advocates adapting formats, oral historians, fan-fiction writers, and hobbyist archivists. Their uses are the ones the doctrine's own commentary-and-scholarship language describes, yet they hold no seat in the negotiations among rights holders, platforms, and courts; their objections surface only when a proxy litigant happens to carry them.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, unrepresented_user_communities, excluded,
    powerless, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, major_content_publishers).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Funds and stabilizes creative production: enforceable exclusivity over expressive works makes investment in expensive-to-produce works recoverable, and confining the exception to a provable defense keeps the property signal legible while conceding the minimal exceptions (criticism, commentary, scholarship) the system needs to stay politically sustainable.
% TRANSFER_FUNCTION: Moves licensing fees, settlement payments, and litigation costs from unauthorized users (independent creators, archives, research labs) toward rights holders and the enforcement industry; moves discretion over cultural reuse from users to rights holders; and places the risk and expense of proving entitlement onto the defendant side of every dispute.
% ABSENT_VOICES: Unrepresented user communities - educators, disability-access advocates, oral historians, fan creators - would object that the burden structure prices out precisely the uses the doctrine's own text concedes; they are absent from policy negotiation, represented only indirectly through library and technologist proxies when a lawsuit happens to touch their use class.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight - courts tomorrow treating the exception as a user-side facilitation right with the burden shifted to objectors - licensing markets would contract sharply, demand-letter leverage would evaporate, whole categories of quotation, clipping, and corpus-scale reuse would proliferate without negotiation, and rights-holder revenue models would reorganize around voluntary and open licensing within a few years.
% FOUNDING_PROBLEM: The 1976 codification had to reconcile two pressures: give creators and investors bankable exclusivity in a broadcast-and-photocopy economy, while keeping the statute compatible with First Amendment criticism and commentary. Section 107's four-factor test was built to carve minimal, provable exceptions without threatening the incentive core.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: federal appellate and Supreme Court opinions (the 1994 Campbell decision and the 2015 Authors Guild v. Google line) reason explicitly about the founding balance and attest that the market-preservation weighting has been judicially demoted; academic intellectual-property scholarship documents the same shift. The beneficiary-aligned trade press denies any demotion and attests the problem is permanently live. External judicial and scholarly attestation exists; the parties dispute its weight.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends the interval at 0.72. Presumption of infringement plus defendant-side burden means the price of any use is effectively set by the rights holder's alternative of suing; even categories the doctrine concedes in principle cost real money to assert, and the largest catalogs can price licenses just below the cost of defense. Suppression is 0.71 and predominantly structural: notice-and-takedown automation, platform filtering, and litigation exposure close the practical alternatives to license-or-abstain; a minority share is internalized as clearance culture and creator self-censorship (see the suppression-mechanism omega). Theater is 0.33: enforcement does real work - licenses are issued, injunctions granted - but a substantial fraction of visible activity (mass notices, anti-piracy campaigns, value-gap rhetoric) performs protection more than it produces it. Accessibility collapse is 0.60: once a user understands the exposure, alternatives collapse toward licensing, though open-licensed material, the public domain, and original composition remain real if imperfect substitutes. Resistance is 0.55 and recurrent: the Campbell line, the Google Books and HathiTrust outcomes, and library-and-technologist coalitions have repeatedly reweighted the doctrine against this reading, and each reweighting has been followed by enforcement build-outs that restore it. The three measurement series share one eight-point grid and trace roughly two full doctrinal cycles: ascendance (T0-T14), the Campbell-era trough (T21), the DMCA-era counter-ratchet peaking in enforcement capacity and notice theater (T28), the text-and-data-mining-era relief (T35-T42), and the AI-licensing resurgence (T50). The oscillation is not noise: each loosening phase induces reliance - archives digitize, labs build corpora - and the subsequent tightening converts that reliance into licensing demand, so intermittent reinforcement is itself part of the transfer mechanism. Base properties are measured at T50, the resurgence phase, near the cycle's extractive peak.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats compute different types from identical doctrine. From the publisher seat the arrangement is the incentive system working: exclusivity is recovered, unauthorized use is disciplined, and the defense's narrowness is fidelity to property. From the independent-creator seat the same structure is a toll gate with the toll set by the opposing party's litigation budget. Courts occupy an administrative middle: they apply the factors without sitting on either flow. Same-level divergence matters inside the payer class: large AI developers and search firms face the same presumption as independent creators but hold negotiating power - site licenses, jurisdictional structures, litigation reserves - that converts the exposure into a cost of doing business, while small labs and solo creators face a binary of license-or-abandon. Equal nominal position, unequal effective burden, driven entirely by exit options. A secondary identity lock operates on the clearance profession: risk-averse diligence norms are professionally constitutive, so practitioners defend the burden structure as competence itself; if that professional frame broke, demand-side support for narrowness would thin considerably.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive directionality near the subsidized end: publishers collect the flows and help set the rules; collectives and the clearance industry collect without administering. Targets derive high directionality, modulated by exit: trapped users (independent creators whose scenes cannot be rebuilt elsewhere, archives whose missions require the copying) sit nearest the full-target end; powerful-but-constrained corporate ingesters sit somewhat lower because negotiated licenses and jurisdictional structure dampen their effective burden; the diffuse public sits near symmetric - it receives the funded creative stream and pays embedded licensing costs plus the loss of priced-out works. The agenda-setting courts are near-symmetric administratively: they neither collect nor pay, and their decisions move the whole distribution. No directionality overrides were needed: the beneficiary and victim declarations plus the exit atoms produce the correct ordering without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - reconciling bankable exclusivity with room for criticism and commentary - is contested rather than dead: every new reproduction technology reopens it, which makes this mandate unusually self-renewing. That cuts against classic mandatrophy (the function has not quietly atrophied; enforcement is vigorous and the coordination side still funds production) but creates a ratchet risk in the opposite direction: each technological cycle lets enforcement scope expand past the founding balance before the judicial branch reweights, so the arrangement oscillates around its mandate rather than decaying from it. The classification prevents two mislabels: reading the arrangement as pure extraction ignores that abolishing it would collapse funding for expensive-to-produce work - the coordination half is load-bearing; reading it as pure coordination ignores that the same presumption-and-burden structure prices out uses the doctrine itself concedes, which is transfer, not incentive. The mismatch consumer will find founding_problem_status 'contested' paired with disappearance_verdict 'world_rearranges' - no zombie flag: persistence tracks a disputed-but-live function, and the quantity to watch is ratchet amplitude, not atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the narrow_defense_reading of kernel fair_use_statutory_exception; how would instantiating a sibling reading change the structural data and classification?',
    'Author the sibling stories (transformative_right_reading, market_licensing_reading) and compare computed classifications. The disagreement locates in burden allocation (defense versus right), factor weighting (market substitution versus transformativeness), and the default verdict for commercial uses.',
    'Under transformative_right_reading the burden shifts toward objectors and effective extraction falls toward rope-like coordination; under market_licensing_reading the licensable universe expands and extraction rises toward snare territory. This story''s tangled_rope classification holds only for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega: one reading of the fair-use kernel; sibling readings would restructure burden, factor weighting, and epsilon.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (takedown automation, litigation exposure, platform filtering) or internalized (clearance culture and creator self-censorship that persist after barriers fall)?',
    'Post-relaxation suppression trajectory: compare use rates in domains where enforcement loosened (the post-Google-Books text-and-data-mining window) against domains where enforcement held constant; persistent depression of use after barrier removal indicates internalization.',
    'If substantially internalized, effective suppression exceeds the structural measure - creators carry the chill with them after exit, and reform underestimates the residual constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression in the chilling-effect channel; rough authoring estimate is seventy percent structural, thirty percent internalized.').

omega_variable(
    market_substitution_elasticity,
    'Do the unauthorized uses this reading presumes harmful actually substitute for licensed consumption - the empirical premise on which the reading''s market-value justification rests?',
    'Economic studies of substitution elasticities across use classes (model training, corpus analysis, clipping, sampling), plus natural experiments from jurisdictions with broader user rights.',
    'If substitution is systematically weaker than presumed, the reading''s justification fails on its own terms and the measured extraction resolves as rent rather than incentive protection; if substitution is strong, part of the measured extraction is the price of the incentive the reading exists to protect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_substitution_elasticity, empirical, 'Empirical status of the substitution premise underlying the reading''s market-preservation axiom.').

omega_variable(
    fragmented_user_coalition_potential,
    'Can the powerless fragmented user seats (independent creators, fan communities, educators) convert latent coalition capacity into organized resistance capable of moving the doctrine?',
    'Track diffusion of the documentary best-practices model and collective legal-defense arrangements into other user communities, and measure whether organized-user filings shift judicial factor weighting.',
    'Successful coalition formation raises organized resistance and could push periodic renegotiation toward durable rebalancing; failure leaves the burden asymmetry opposed only by deep-pocketed proxies whose interests diverge from small users.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fragmented_user_coalition_potential, empirical, 'Coalition-power question for the powerless seats; bears on whether resistance can grow beyond its current proxy-mediated form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fu_narrow_defense_tr_t0, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fu_narrow_defense_tr_t7, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 7, 0.21).
narrative_ontology:measurement(fu_narrow_defense_tr_t14, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 14, 0.24).
narrative_ontology:measurement(fu_narrow_defense_tr_t21, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 21, 0.27).
narrative_ontology:measurement(fu_narrow_defense_tr_t28, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 28, 0.37).
narrative_ontology:measurement(fu_narrow_defense_tr_t35, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 35, 0.31).
narrative_ontology:measurement(fu_narrow_defense_tr_t42, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 42, 0.28).
narrative_ontology:measurement(fu_narrow_defense_tr_t50, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 50, 0.33).

% Extraction over time
narrative_ontology:measurement(fu_narrow_defense_be_t0, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(fu_narrow_defense_be_t7, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 7, 0.65).
narrative_ontology:measurement(fu_narrow_defense_be_t14, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 14, 0.67).
narrative_ontology:measurement(fu_narrow_defense_be_t21, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 21, 0.58).
narrative_ontology:measurement(fu_narrow_defense_be_t28, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 28, 0.69).
narrative_ontology:measurement(fu_narrow_defense_be_t35, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 35, 0.63).
narrative_ontology:measurement(fu_narrow_defense_be_t42, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 42, 0.61).
narrative_ontology:measurement(fu_narrow_defense_be_t50, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fu_narrow_defense_su_t0, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(fu_narrow_defense_su_t7, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 7, 0.47).
narrative_ontology:measurement(fu_narrow_defense_su_t14, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 14, 0.52).
narrative_ontology:measurement(fu_narrow_defense_su_t21, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 21, 0.58).
narrative_ontology:measurement(fu_narrow_defense_su_t28, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 28, 0.71).
narrative_ontology:measurement(fu_narrow_defense_su_t35, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 35, 0.68).
narrative_ontology:measurement(fu_narrow_defense_su_t42, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 42, 0.65).
narrative_ontology:measurement(fu_narrow_defense_su_t50, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__narrow_defense_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, dmca_safe_harbor_regime).

% DUAL FORMULATION NOTE:
% The colloquial label 'fair use' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one statutory kernel: this narrow-defense reading (high extraction for most unauthorized uses; burden on the defendant), transformative_right_reading (lower extraction; burden shifts toward objectors; transformativeness central), and market_licensing_reading (highest extraction; the exception confined to marketless uses). They form a constraint family: the shared fixed text feeds all three, and each reading's litigation record changes the operating environment of the others - a Campbell-line win drains this reading's leverage, an AI-era licensing win restores it. The dmca edge records the enforcement-infrastructure coupling: the takedown machinery built for this reading's enforcement now disciplines use classes across all three readings. Per-reading classification lives in each file; nothing here averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
