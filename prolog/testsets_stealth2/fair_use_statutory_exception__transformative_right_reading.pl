% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__transformative_right_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use as Transformative-Reuse Mandate (Transformative Right Reading)
 *   domain: legal/intellectual_property/information_economics
 *
 * SUMMARY:
 *   Fair use — 17 U.S.C. § 107, descended from Folsom v. Marsh — permits
 *   unlicensed reuse of copyrighted works under a four-factor judicial
 *   balancing. This story models the doctrine as instantiated by the
 *   transformative-right reading: transformative purpose dominates the
 *   analysis, the existence of a licensing market does not decide outcomes,
 *   courts owe innovation a facilitation duty, and the fairness burden is
 *   shared between accuser and accused. The epsilon referent is the standing
 *   arrangement under contest — the doctrine as actually administered, with
 *   fair use pleaded as affirmative defense and paid for in litigation —
 *   assessed by this reading's own lights: genuinely load-bearing for
 *   criticism, scholarship, search, and machine learning, yet taxing its own
 *   beneficiaries through process and denying substitutive reusers by design.
 *   Claim and metrics are authored independently: the claim states the
 *   structural type this arrangement actually exhibits; the metrics describe
 *   observed operation without tuning toward any computed verdict.
 *
 * KEY AGENTS:
 *   - - article_iii_judiciary: Agenda setter (institutional/constrained) — administers the doctrine case by case; its opinions are the arrangement's moving parts
 *   - - institutional_transformative_reusers: Primary beneficiary with payer overlay (powerful/constrained) — wins judgments, pays litigation
 *   - - individual_transformative_creators: Beneficiary with payer overlay (moderate/identity_locked) — creative practice constituted by reuse
 *   - - substitutive_reusers: Primary target (moderate/constrained) — denied the privilege and channeled to licensing
 *   - - rights_holders: Payer with beneficiary overlay (institutional/constrained) — bears uncompensated uses, collects channeled licensing revenue
 *   - - licensing_market_intermediaries: Excluded voice (organized/trapped) — business model premised on the rival market-centric reading
 *   - - ip_law_scholarship: Analytical observer (analytical/analytical) — maps trajectories across cases and decades
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.49).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.62).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.39).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.49).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.39).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative-Reuse Mandate (Transformative Right Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "legal/intellectual_property/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '4e473e1e-46be-4b19-9d85-372c53a817d7').
narrative_ontology:cs_kernel_codification('4e473e1e-46be-4b19-9d85-372c53a817d7', fixed_text).
narrative_ontology:cs_authority_grounding('4e473e1e-46be-4b19-9d85-372c53a817d7', lineage).
narrative_ontology:cs_interpretation_layer_present('4e473e1e-46be-4b19-9d85-372c53a817d7').
narrative_ontology:cs_reading_relation('4e473e1e-46be-4b19-9d85-372c53a817d7', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e473e1e-46be-4b19-9d85-372c53a817d7', fair_use_statutory_exception__market_licensing_reading, forecloses).
narrative_ontology:cs_axiom('4e473e1e-46be-4b19-9d85-372c53a817d7', foundational, transformative_purpose_carries_primary_weight).
narrative_ontology:cs_axiom_status(transformative_purpose_carries_primary_weight, holdable).
narrative_ontology:cs_axiom_grounding('4e473e1e-46be-4b19-9d85-372c53a817d7', transformative_purpose_carries_primary_weight, instrumental).
narrative_ontology:cs_axiom('4e473e1e-46be-4b19-9d85-372c53a817d7', foundational, licensing_market_presence_not_dispositive).
narrative_ontology:cs_axiom_status(licensing_market_presence_not_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('4e473e1e-46be-4b19-9d85-372c53a817d7', licensing_market_presence_not_dispositive, instrumental).
narrative_ontology:cs_axiom('4e473e1e-46be-4b19-9d85-372c53a817d7', secondary, judicial_facilitation_duty).
narrative_ontology:cs_axiom_status(judicial_facilitation_duty, holdable).
narrative_ontology:cs_axiom_grounding('4e473e1e-46be-4b19-9d85-372c53a817d7', judicial_facilitation_duty, conventional).
narrative_ontology:cs_reference_frame('4e473e1e-46be-4b19-9d85-372c53a817d7', transformative_reuse_priority_framework).
narrative_ontology:cs_drift_state('4e473e1e-46be-4b19-9d85-372c53a817d7', contemporary_ai_litigation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e473e1e-46be-4b19-9d85-372c53a817d7', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, institutional_transformative_reusers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, individual_transformative_creators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, substitutive_reusers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, rights_holders).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, institutional_transformative_reusers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, individual_transformative_creators).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, transformative_use_centrality).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, first_amendment_safety_valve).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, innovation_facilitation_mandate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides fair use questions case by case under Section 107's four statutory factors: purpose and character of the use, nature of the copied work, amount taken, and market effect. Under the transformative-purpose line of cases the purpose factor dominates, and the mere existence of a licensing market does not settle the question. Precedent-bound; cannot decline fair use disputes once filed; shapes the doctrine's reach through each opinion.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, article_iii_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Large-scale reusers — search engines, library digitization projects, AI developers, archival repositories — copy millions of works to build new products and services. They prevail in most fair use cases litigated to judgment but spend years and tens of millions of dollars getting there, and settlement pressure pushes them toward licensing deals that erode the precedents they won. Leaving the reuse economy means abandoning their product lines; per-work licensing at bulk scale is rarely workable.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, institutional_transformative_reusers, beneficiary,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, institutional_transformative_reusers, payer).

% Parodists, critics, essayists, video essayists, documentary editors, remix musicians. Their creative practice consists precisely of reworking existing works; abandoning reuse means abandoning the art form. Most cannot fund a single infringement suit, so the protection reaches them mainly through platforms, insurers, or pro bono counsel; otherwise it operates as background reassurance. Many carry takedown-driven self-censorship habits even where the law would protect them.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, individual_transformative_creators, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, individual_transformative_creators, payer).

% Reprint houses, abridgment publishers, anthology compilers, clip-licensing operations — reusers whose output competes with the original or its customary markets. Courts routinely refuse them the privilege and send them to the licensing table; where owners refuse to license, the use dies. Their options are redesigning the product until it stops substituting, absorbing license fees, or exiting.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, substitutive_reusers, payer,
    moderate, biographical, constrained, national).

% Publishers, studios, record labels, photo agencies, authors and estates. They lose control over some uses of their catalogs without compensation — quotation, parody, indexing, model training — and they finance the lobbying and litigation that contests the doctrine's breadth. At the same time they collect licensing revenue from every reuser the doctrine sends their way, and the doctrine shields their works' market value from outright substitution. They cannot opt their works out; their practical levers are contract terms, technical protection, and selective enforcement.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, rights_holders, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, rights_holders, beneficiary).

% Stock-photo agencies, collective rights organizations, reprint clearinghouses, and emerging AI-licensing brokers. Their entire business is selling permissions; a doctrine that treats the existence of a licensing market as beside the point threatens the premise of their pricing. They appear in fair use disputes only as witnesses and amici, never as decision-makers.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, licensing_market_intermediaries, excluded,
    organized, biographical, trapped, global).

% Academic commentators who map the doctrine's trajectories, propose tests, and supply the vocabulary courts cite. They observe the whole structure across cases and decades; their stake is reputational and intellectual rather than financial.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, ip_law_scholarship, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, rights_holders).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the clearance hold-up problem in expressive reuse: owners rationally refuse or overprice licenses for uses they dislike (parody, criticism, negative review, competitive indexing), so a case-by-case judicial privilege lets valuable reuse proceed without owner consent while keeping outright substitution priced.
% TRANSFER_FUNCTION: Moves litigation risk and defense costs onto every challenged reuser regardless of eventual outcome; moves licensing revenue from denied substitutive reusers to rights holders; moves uncompensated use-value (quotation, indexing, training corpora) from rights holders to transformative reusers and their audiences.
% ABSENT_VOICES: Licensing intermediaries and rights-holder trade associations would insist market evidence be near-dispositive; they sit outside the doctrinal conversation except as amici. Unrepresented also are the never-litigating creators deterred before filing — the arrangement's silent casualties, visible only as absent works.
% DISAPPEARANCE_RATIONALE: Search indexes, scholarly quotation, parody, video criticism, documentary reuse, and large-scale AI training would each need per-work licenses or cessation; owners would gain veto power over criticism and indexing; entire product categories — web search as practiced, model training on scraped text — would become legally impossible at current scale. The expressive economy would reorganize around whatever licensing regimes emerged.
% FOUNDING_PROBLEM: Nineteenth- and twentieth-century courts confronted copyright enforced as an absolute veto: biographers could not quote letters, critics could not reproduce passages, and the incentive rationale plainly did not extend to suppressing commentary. Folsom v. Marsh articulated the equitable balance; Congress codified it in Section 107 after the 1909 Act's rigid formalities proved unworkable.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: Golan v. Holder (2012), where the Supreme Court described fair use as copyright's built-in First Amendment accommodation; the 1976 House Report's statement of purpose; and rights-holder amicus filings that concede the doctrine's constitutional function while disputing its proper breadth. No party with standing denies the founding problem existed; the contest is over its present severity.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.49, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.49 is reading-indexed over the standing arrangement — the doctrine as administered, not the reading's endorsed burden-shared version. The doctrine subsidizes transformative reuse (its beneficiaries usually win when they reach judgment) but taxes every challenged reuser with full defense costs ex ante, and it denies substitutive reusers by design; netting subsidy against process tax and denial yields moderate extraction. Suppression 0.62 is unscaled structural force: statutory damages and injunctions, plus the surrounding apparatus — anti-circumvention law, clickwrap overrides, takedown defaults — that this reading watches contract around the doctrine. Theater 0.39: four-factor balancing is increasingly announced rather than performed, with outcome-driven reasoning dressed in factor language, though landmark opinions still do genuine analytic work. Accessibility collapse 0.42: licensing, public domain, and original creation remain real alternatives for many uses, but for parody, criticism, and bulk indexing they are largely closed. Resistance 0.72: near-continuous opposition — trade-group litigation, treaty pressure, recurring narrowing bills — marks this as constructed and defended, nothing natural. All three tracked series share one seven-point grid (1994–2026) so no metric row borrows another's timeline; the rising suppression series tracks enforcement-capacity buildup (DMCA implementation, contract override, technical enforcement), which is the dynamic this story traces.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the agenda-setting bench the doctrine is a working balancing mandate refined case by case. From the institutional transformative reuser's seat it is a shield that arrives only after years of litigation spending — protection priced like a luxury good. From the individual creator's seat it is a shadow right, real when an institution or insurer stands behind them, theoretical alone. From the substitutive reuser's seat it is a wall that delivers them to a licensing desk that may refuse them. From the rights holder's seat it is simultaneously an uncompensated loss of catalog control and a rent-collecting gatekeeper for everyone it turns away. Same text, four different operating realities; the engine computes per-seat classifications from the structural data rather than averaging them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place both reuser classes near the subsidized end. Institutional reusers carry partial arbitrage-grade exits (product redesign, negotiated bulk deals) that pull them further toward the beneficiary pole; individual creators are identity-locked — their practice IS reuse — which holds them nearer the middle despite nominal beneficiary status, since they collect the privilege's value but pay the process tax on every challenge. Substitutive reusers sit near the full-target end: denials bind them directly and their exit runs through the very licensing desk the denial creates. Rights holders are genuinely dual-positioned — bearers of uncompensated use and collectors of channeled licensing revenue — so their derived directionality lands mid-high rather than at the target pole; the secondary beneficiary role encodes that. No directionality overrides were needed: exit-option differentiation already separates the seats the derivation would otherwise conflate.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline prevents two opposite errors. Labeling the doctrine pure extraction would mistake a structure whose intended beneficiaries win on the merits for one whose coordination story is cover — the transformative-user win rate and the doctrine's survival of repeated legislative attack both cut against capture. Labeling it pure coordination would ignore the measurable burden: the process tax on its own beneficiaries, the designed denial of substitutive reusers, and the enforcement apparatus growing around it. The hybrid holds both truths. On obsolescence: the founding problem — copyright enforced as an absolute veto suppressing commentary — is live, reignited by every new copying technology from photocopiers to foundation models; nothing here is vestige, and the R5 mismatch consumer finds status=live paired with world_rearranges, so no zombie flag arises.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positionality,
    'This story instantiates the transformative_right_reading of the fair_use_statutory_exception kernel; how would instantiating narrow_defense_reading or market_licensing_reading instead restructure the victim set, burden allocation, and epsilon?',
    'Track Supreme Court holdings rather than commentary: an express majority adoption of a sibling premise (for example, holding that proof of an available licensing market presumptively defeats fair use) would swap the instantiated constraint; the sibling files are the alternate instantiations.',
    'Under narrow_defense_reading the victim set expands to nearly all reusers and epsilon rises sharply; under market_licensing_reading every reuser with a hypothesizable market becomes a target and the transformative-user beneficiary class shrinks to marketless niches.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positionality, conceptual, 'Committer-frame omega: this constraint is one reading of the fair use kernel; sibling readings constitute different constraints.').

omega_variable(
    litigation_tax_vs_privilege_value,
    'Is the cost this reading measures on transformative reusers a contingent feature of the affirmative-defense posture (removable by burden-shifting or wider declaratory relief), or inherent to any court-adjudicated privilege?',
    'Cross-regime comparison: jurisdictions or procedural reforms that shift burdens to plaintiffs, or expanded declaratory-judgment usage, would reveal whether the process cost falls with posture or persists.',
    'If contingent, the standing arrangement''s epsilon is overstated relative to the reading''s endorsed arrangement and the structure sits closer to pure coordination; if inherent, part of the cost is the irreducible price of judicial boundary-drawing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(litigation_tax_vs_privilege_value, empirical, 'Whether the process burden on beneficiaries is posture-contingent or structural.').

omega_variable(
    chilled_use_denominator,
    'How much transformative reuse never occurs because potential reusers self-censor before any court rules — the invisible denominator beneath every measured win rate?',
    'Takedown-appeal reversal rates, pre-publication clearance-abandonment studies, and comparisons of identical uses across jurisdictions with and without a robust fair use equivalent.',
    'A large chilled denominator means the doctrine''s realized coordination function is smaller than litigated outcomes suggest and the standing arrangement''s effective burden on its intended beneficiaries exceeds what courtroom records show.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilled_use_denominator, empirical, 'Unobservable deterred reuse as a hidden component of the measured burden.').

omega_variable(
    ai_licensing_settlement_drift,
    'Does the wave of AI-era licensing settlements reflect a transient bargaining asymmetry (unclear law favoring owners during pending litigation) or a durable displacement of the transformative frame by licensing-first norms?',
    'Track whether post-settlement doctrine reasserts transformative-use primacy (as the API holding did) or whether licensing-first becomes the compliance default regardless of holdings.',
    'Durable displacement would push this instantiation toward the market-licensing sibling''s shape — rising burden on bulk reusers, shrinking beneficiary class — without any formal overruling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_licensing_settlement_drift, empirical, 'Whether current licensing-first practice is cyclical or a regime change.').

omega_variable(
    cs_framing_doctrine_vs_amendment,
    'Is the constraint best framed as the statutory doctrine itself, or as the First Amendment accommodation layered above copyright that the doctrine merely implements? The two framings yield different classifications.',
    'Framing-sensitivity test: if the accommodation framing is adopted, the structure inherits constitutional entrenchment (harder to remove, less responsive to legislative preference) and its persistence question changes character; signals guiding the choice here are the statutory form of Section 107 and Congress''s retained amendment authority, which favor the doctrine framing.',
    'Accommodation framing raises persistence and lowers fixability, approaching structural status; doctrine framing keeps it a revisable statutory construct. Classification follows the chosen frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_doctrine_vs_amendment, conceptual, 'CS-framing under-determination: doctrine versus constitutional accommodation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 1994, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_tr_tr_t1994, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 1994, 0.2).
narrative_ontology:measurement_basis(fair_use_tr_tr_t1994, observed).
narrative_ontology:measurement(fair_use_tr_tr_t2000, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(fair_use_tr_tr_t2000, observed).
narrative_ontology:measurement(fair_use_tr_tr_t2006, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2006, 0.26).
narrative_ontology:measurement_basis(fair_use_tr_tr_t2006, observed).
narrative_ontology:measurement(fair_use_tr_tr_t2012, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2012, 0.3).
narrative_ontology:measurement_basis(fair_use_tr_tr_t2012, observed).
narrative_ontology:measurement(fair_use_tr_tr_t2018, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2018, 0.34).
narrative_ontology:measurement_basis(fair_use_tr_tr_t2018, observed).
narrative_ontology:measurement(fair_use_tr_tr_t2024, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2024, 0.37).
narrative_ontology:measurement_basis(fair_use_tr_tr_t2024, observed).
narrative_ontology:measurement(fair_use_tr_tr_t2026, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 2026, 0.39).
narrative_ontology:measurement_basis(fair_use_tr_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(fair_use_tr_be_t1994, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 1994, 0.3).
narrative_ontology:measurement_basis(fair_use_tr_be_t1994, observed).
narrative_ontology:measurement(fair_use_tr_be_t2000, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement_basis(fair_use_tr_be_t2000, observed).
narrative_ontology:measurement(fair_use_tr_be_t2006, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2006, 0.36).
narrative_ontology:measurement_basis(fair_use_tr_be_t2006, observed).
narrative_ontology:measurement(fair_use_tr_be_t2012, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2012, 0.4).
narrative_ontology:measurement_basis(fair_use_tr_be_t2012, observed).
narrative_ontology:measurement(fair_use_tr_be_t2018, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2018, 0.44).
narrative_ontology:measurement_basis(fair_use_tr_be_t2018, observed).
narrative_ontology:measurement(fair_use_tr_be_t2024, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2024, 0.47).
narrative_ontology:measurement_basis(fair_use_tr_be_t2024, observed).
narrative_ontology:measurement(fair_use_tr_be_t2026, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 2026, 0.49).
narrative_ontology:measurement_basis(fair_use_tr_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(fair_use_tr_su_t1994, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 1994, 0.32).
narrative_ontology:measurement_basis(fair_use_tr_su_t1994, observed).
narrative_ontology:measurement(fair_use_tr_su_t2000, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement_basis(fair_use_tr_su_t2000, observed).
narrative_ontology:measurement(fair_use_tr_su_t2006, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2006, 0.44).
narrative_ontology:measurement_basis(fair_use_tr_su_t2006, observed).
narrative_ontology:measurement(fair_use_tr_su_t2012, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement_basis(fair_use_tr_su_t2012, observed).
narrative_ontology:measurement(fair_use_tr_su_t2018, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2018, 0.56).
narrative_ontology:measurement_basis(fair_use_tr_su_t2018, observed).
narrative_ontology:measurement(fair_use_tr_su_t2024, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2024, 0.61).
narrative_ontology:measurement_basis(fair_use_tr_su_t2024, observed).
narrative_ontology:measurement(fair_use_tr_su_t2026, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 2026, 0.62).
narrative_ontology:measurement_basis(fair_use_tr_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'fair use' covers three structurally distinct arrangements depending on which reading of the Section 107 kernel a court instantiates. This file is the transformative_right_reading (moderate burden concentrated on substitutive reusers and on process costs borne by transformative reusers). The narrow_defense_reading file models a property-preserving defense construed against the user (higher burden across the reuser population); the market_licensing_reading file models a regime where hypothesized licensing markets defeat nearly every use (highest burden, smallest beneficiary class). The readings are linked, not merged: each carries its own beneficiaries, victims, and classification, and upstream holdings in any one reshape the operating environment of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
