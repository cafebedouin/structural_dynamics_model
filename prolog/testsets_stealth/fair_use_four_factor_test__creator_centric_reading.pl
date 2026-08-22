% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use as Narrow Exception to Copyright Property (Creator-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story instantiates the creator-centric reading of the fair-use
 *   kernel: fair use as a narrow, grudging exception to a strong property
 *   right, with the four statutory factors weighed to preserve creator
 *   incentives. The interval 0-40 maps approximately to 1985-2025, from the
 *   Harper & Row-era high-water mark of the reading through the
 *   takedown-statute era to the post-Warhol transformativeness ascendancy.
 *   Epsilon is authored over the standing arrangement under contest as this
 *   reading sees it — a clearance-and-takedown economy in which unauthorized
 *   use is presumptively infringing, the user bears the litigation risk, and
 *   market-harm reasoning reaches uses that add new meaning — never over the
 *   user-rights arrangement this reading opposes. Rights holders sit at the
 *   beneficiary pole; transformative creators, remix communities, and the
 *   future public sit at the target pole. The sibling readings (user-centric,
 *   transformative-use) are separate constraints with their own epsilon,
 *   beneficiary sets, and victim sets, linked through the network block; the
 *   contest between readings is routed to the omega variables, not averaged
 *   into this classification. The claim and the metrics are independent
 *   authored facts: the reading asserts a real coordination function, and the
 *   metrics describe substantially extractive, actively enforced operation on
 *   top of it.
 *
 * KEY AGENTS:
 *   - copyright_holder_publishers: agenda-setter and primary beneficiary (institutional/arbitrage) — sets the doctrine's practical reach through litigation selection and lobbying; collects licensing revenue and damages
 *   - legacy_content_estates: secondary beneficiary (powerful/arbitrage) — collects from catalogs they did not create
 *   - collective_licensing_bodies: secondary beneficiary (organized/arbitrage) — fee base grows as the exception narrows
 *   - platform_intermediaries: delegated enforcer and secondary beneficiary (institutional/arbitrage) — operates takedown at scale, monetizes both sides
 *   - federal_courts: administering authority (institutional/analytical) — weighs the factors; collects and pays nothing
 *   - transformative_creators: primary target (moderate/constrained) — bears clearance costs and litigation risk; the source material is their subject
 *   - documentary_filmmakers: organized target (organized/constrained) — contests via best-practices codes but pays through insurance clearance
 *   - remix_artists: primary target (powerless/trapped) — bound at full strength by automated enforcement regardless of merit
 *   - future_creators_and_audiences: diffuse target (powerless/trapped) — inherit a thinner commons; hold no seat and cannot identify the loss
 *   - user_rights_advocates: analytical observer (organized/analytical) — litigates the opposing readings in the same courts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.72).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.75).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use as Narrow Exception to Copyright Property (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, '0b71cb5f-176a-4349-af61-6865f6d6a9dc').
narrative_ontology:cs_kernel_codification('0b71cb5f-176a-4349-af61-6865f6d6a9dc', fixed_text).
narrative_ontology:cs_authority_grounding('0b71cb5f-176a-4349-af61-6865f6d6a9dc', lineage).
narrative_ontology:cs_interpretation_layer_present('0b71cb5f-176a-4349-af61-6865f6d6a9dc').
narrative_ontology:cs_reading_relation('0b71cb5f-176a-4349-af61-6865f6d6a9dc', fair_use_four_factor_test__user_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('0b71cb5f-176a-4349-af61-6865f6d6a9dc', fair_use_four_factor_test__transformative_use_reading, coexists_with).
narrative_ontology:cs_axiom('0b71cb5f-176a-4349-af61-6865f6d6a9dc', foundational, fair_use_exception_not_user_right).
narrative_ontology:cs_axiom_status(fair_use_exception_not_user_right, holdable).
narrative_ontology:cs_axiom_grounding('0b71cb5f-176a-4349-af61-6865f6d6a9dc', fair_use_exception_not_user_right, conventional).
narrative_ontology:cs_axiom('0b71cb5f-176a-4349-af61-6865f6d6a9dc', foundational, creator_incentives_balance_pole_star).
narrative_ontology:cs_axiom_status(creator_incentives_balance_pole_star, holdable).
narrative_ontology:cs_axiom_grounding('0b71cb5f-176a-4349-af61-6865f6d6a9dc', creator_incentives_balance_pole_star, instrumental).
narrative_ontology:cs_reference_frame('0b71cb5f-176a-4349-af61-6865f6d6a9dc', property_baseline_narrow_exception).
narrative_ontology:cs_drift_state('0b71cb5f-176a-4349-af61-6865f6d6a9dc', post_warhol_transformativeness_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0b71cb5f-176a-4349-af61-6865f6d6a9dc', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holder_publishers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, legacy_content_estates).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, collective_licensing_bodies).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, platform_intermediaries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, remix_artists).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, future_creators_and_audiences).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, incentive_theory_of_copyright).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__creator_centric_reading, copyright_as_property_analogy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major publishers, studios, labels, and their trade associations set the practical agenda through litigation selection, statutory lobbying (term extension, takedown statutes), and standardized licensing offers. They collect licensing revenue and statutory damages, and they choose which unauthorized uses to sue — a choice that shapes how far the exception reaches in practice far beyond the cases actually filed.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holder_publishers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, copyright_holder_publishers, beneficiary).

% Heirs and holding companies of mid-century creators license catalogs they did not create, collecting from works whose authors are dead. Their interest is in the exception staying narrow and the term staying long; they bear none of the arrangement's administrative burden.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, legacy_content_estates, beneficiary,
    powerful, generational, arbitrage, global).

% Performance rights organizations and reprographic rights collectives run blanket-license schemes. Every use pushed from 'unauthorized but arguably excepted' into 'license required' enlarges their fee base; they fund rights-holder-side scholarship and amicus work out of collections.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, collective_licensing_bodies, beneficiary,
    organized, biographical, arbitrage, global).

% User-generated content platforms operate the takedown and content-identification machinery the narrow reading runs on. They monetize both sides — hosting licensed and unlicensed use alike — while over-removing to protect safe-harbor positions. They did not write the statute, but they decide, algorithmically and at scale, what the exception means in practice for the seats without counsel.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, platform_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, platform_intermediaries, agenda_setter).

% Federal judges weigh the four statutory factors case by case and write the doctrine's operative meaning; no factor carries statutory weight, so each opinion reallocates the balance. They collect nothing and pay nothing; their seat is the administering one the reading's lineage authority runs through.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Biographers, video essayists, critics, and sampling musicians build new work out of existing culture. Each unauthorized use carries litigation risk they cannot price in advance; many buy licenses they believe they do not need, or cut sequences rather than fight. Exit means abandoning the source material — which, for criticism and documentary work, is the subject itself.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_creators, payer,
    moderate, biographical, constrained, global).

% Working documentarists clear clips through errors-and-omissions insurers whose clearance practices price in litigation risk regardless of merit. They pioneered fair-use best-practices codes to push back, and their organized position buys contestable process that individual creators lack — but the insurance market still converts doctrine into clearance fees on every production.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, documentary_filmmakers, payer,
    organized, biographical, constrained, national).

% Individual creators in platform economies sample, remix, and react without counsel or budgets. Automated takedown and content identification operate against them at scale; their remedy is deletion. They cannot litigate a single factor analysis, so the arrangement binds them at full strength regardless of the merits of any particular use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, remix_artists, payer,
    powerless, biographical, trapped, global).

% The people who would have made and seen the works that were never made: audiences for criticism and collage that stayed unproduced, and creators who inherit a commons thinner than the one their sources drew on. They hold no seat in any litigation and cannot identify what they lost.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, future_creators_and_audiences, payer,
    powerless, generational, trapped, global).

% Public-interest legal organizations and fair-use scholars litigate test cases, file amicus briefs, and publish factor-by-factor critiques. They hold the analytical seat opposing this reading inside the same courts; they collect no licensing revenue and bear only the costs of the fights they choose.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, user_rights_advocates, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__creator_centric_reading, copyright_holder_publishers).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__creator_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The copyright system coordinates creative production by letting authors and their assignees capture value from works, funding creation that unpriced markets might underprovide; the fair-use carve-out, read narrowly, preserves a minimal margin for criticism and commentary so the property regime does not suppress the discourse it exists to feed. Under this reading the four factors are the mechanism that keeps that margin minimal and predictable enough to license around.
% TRANSFER_FUNCTION: Moves licensing revenue, statutory damages, and litigation leverage from unauthorized and would-be unauthorized users to rights holders; moves risk — takedown exposure, clearance cost, abandoned projects — onto transformative creators; and pushes back the date at which source material becomes freely usable by downstream culture.
% ABSENT_VOICES: Audiences and future creators have no seat: the public domain cannot litigate for itself, and the works deterred into nonexistence leave no plaintiff. Informal remix communities appear in the process only as defendants. Their objections surface secondhand, through advocates and scholars holding the observer seat.
% DISAPPEARANCE_RATIONALE: If this reading's arrangement vanished overnight — if fair use were tomorrow administered as an affirmative user right — licensing markets would reprice: clearance practices, errors-and-omissions underwriting, automated takedown thresholds, and blanket-license fee bases all price in the narrow reading. Transformative production would expand where takedown risk fell; rights holders would lose a leverage stream they actively maintain through litigation selection and lobbying. The arrangements of every seated party depend on the constraint holding.
% FOUNDING_PROBLEM: The arrangement descends from the Statute of Anne and the U.S. IP clause's bargain: grant authors exclusive rights long enough to incentivize creation, then release works to the public. Folsom v. Marsh (1841) added fair use so criticism and commentary would not be suppressed by the very right meant to enrich them. The founding problem: calibrate exclusivity so creation is funded without foreclosing the follow-on creativity the exclusivity is supposed to serve.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders attest the incentive problem is permanently live, citing publisher-commissioned economic studies; user-side litigants and independent empirical work on creative motivation attest the funding problem is substantially solved for incumbent catalogs and that the arrangement now over-serves it. The constitutional purpose clause is the non-party benchmark both sides invoke. No attestation of the founding problem's current shape comes from a seat outside the contest; the closest to neutral corroboration is the copyright office's own studies and the independent economics literature, both of which cut against the reading's strong form.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored from this reading's own lights over the standing arrangement, not tuned to any target. Extractiveness 0.72: under this reading unauthorized use is presumptively infringing, the user bears the risk, and market-harm reasoning reaches meaning-adding uses, so a large class of would-be lawful activity is priced into licenses or abandoned; the transfer is decoupled from marginal service cost in the way rent is — a clearance fee prices litigation risk, not service. Suppression 0.75 is structural, not internalized: burden placement, platform takedown defaults, and errors-and-omissions insurance practices suppress use before any court weighs a factor; the mechanism operates on projects, not psyches. Theater_ratio 0.52: the four factors are a genuine analytical framework, but their recited even-handedness increasingly performs neutrality while transformativeness case law and burden placement do determinate work. Accessibility_collapse 0.55: alternatives persist (licensing, best-practices documentation, jurisdictional differences) but are unevenly available and priced for the organized. Resistance 0.6: test-case litigation, amicus campaigns, and fair-use best-practices codes are real organized pushback. Claimed type tangled_rope is stated from the structure the reading itself asserts: a genuine coordination function (exclusive rights funding production) with asymmetric extraction running through the same enforcement machinery — beneficiaries and victims named, active enforcement required. The measurement series runs on one shared six-point grid with every tracked metric authored at every point. Extractiveness rises from the 1985 baseline through the takedown-statute machinery, plateaus, and eases slightly late as transformative-use jurisprudence cuts against the reading's market-harm reach — that late decline is the sibling reading's structural pressure registering, not this constraint's decay. Suppression_requirement is authored because enforcement capacity genuinely changed over the interval: notice-and-takedown and automated content identification built suppression infrastructure that did not exist at t0. Theater rises monotonically as the gap between stated balancing and operative outcomes widens. Coalition note: the powerless victim seats (remix artists, future creators) currently lack coalition infrastructure — unlike documentarians, who converted organization into best-practices leverage; a remix-community coalition is the live possibility that would move their effective position.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is the incentive engine that built and maintains the catalog — every narrowing of the exception reads as protecting the funders of culture. From the constrained and trapped payer seats the same structure operates as a toll gate on culture: clearance fees, takedown risk, abandoned sequences. The administering seat experiences the four factors as neutral craft; the trapped seats experience their operation as deterrence machinery, because the factor that decides their cases is decided before filing — by insurance practices and platform defaults. Same-level divergence: documentary filmmakers and remix artists face the same doctrine at the same nominal legal position, but organized clearance practice gives one seat contestable process and leaves the other automated deletion. Per-seat classification is the engine's computation from this structural data, not something the authored claim adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the low-d pole: copyright_holder_publishers (collect revenue and set the doctrine's practical reach), legacy_content_estates and collective_licensing_bodies (collect without administering), platform_intermediaries (collect ad revenue and safe-harbor protection while administering takedowns — a dual position carried as beneficiary with secondary agenda_setter). Victims sit at the high-d pole: transformative_creators (constrained exit — the source material is their subject), documentary_filmmakers (organized but priced through insurance), remix_artists (trapped — automated enforcement binds at full strength regardless of merit), and future_creators_and_audiences (trapped at generational depth — they bear the cost with no exit and no seat, the deepest target position in the story). Courts administer without collecting and take the near-symmetric fallback. Suppression is a raw structural property here and is not scaled by scope; extractiveness is — the arrangement's global scope amplifies effective extraction most where exit is weakest.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — calibrating exclusivity so creation is funded without foreclosing the follow-on culture that exclusivity is meant to enrich — is contested rather than dead: the incentive argument retains live force for new production even as its force for incumbent catalogs decays. Reading the structure as tangled_rope preserves that distinction: the coordination function is real, and the extraction riding on it is measured rather than assumed away. Calling it rope would launder the clearance-and-takedown economy as pure coordination; calling it snare would erase the funding function new creation still draws on. The mandatrophy hazard runs both directions: an analyst anchored on the incentive story reads every user-side challenge as freeloading; an analyst anchored on the victim set reads the whole structure as pure extraction and loses the genuine coordination the reading was built on. The R5 mismatch consumer should watch the contested founding status against the world_rearranges verdict: if the incentive problem were judged dead while the arrangement persists and rearrangement still follows, that is the capture signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is the creator-centric reading of the fair_use_four_factor_test kernel; how much of the measured extraction is a property of the kernel itself versus of this reading''s instantiation?',
    'Compare the sibling stories (user_centric_reading, transformative_use_reading) computed from the same structural surface: if epsilon and victim sets diverge sharply across readings, the measured extraction is reading-contingent; if they converge, the kernel''s structure dominates.',
    'A high reading-contingency verdict locates the reform target in interpretive practice (which reading courts adopt) rather than in the statute''s design; a low verdict locates it in the statutory structure itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'The extraction measured is contingent on which reading of the four-factor kernel is instantiated.').

omega_variable(
    incentive_empirics,
    'Does narrowing fair use actually increase creative production, or does it primarily protect incumbent catalog revenue?',
    'Independent empirical work correlating enforcement intensity and clearance costs with new-production rates, separated from catalog revenue; natural experiments from jurisdictions with broader user rights.',
    'If production does not track enforcement, the coordination justification decays and the constraint drifts toward snare; if it does, part of the measured extraction is the price of the funding function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_empirics, empirical, 'Whether the incentive-preservation coordination story is empirically true of this reading''s operation.').

omega_variable(
    chilling_effect_counterfactual,
    'How large is the set of transformative works deterred into nonexistence — works no court ever weighed because the risk was never priced as worth taking?',
    'Comparison of pre-clearance project pipelines against released works; surveys of projects abandoned at clearance stages; difference-in-differences around takedown-machinery deployments.',
    'A large deterred set raises the true victim count beyond litigated cases and raises effective extraction on the trapped seats; a small set confines the harm to contested cases.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(chilling_effect_counterfactual, empirical, 'The chilling effect is a counterfactual victim set invisible to case-level measurement.').

omega_variable(
    burden_allocation_ambiguity,
    'Is the disagreement between this reading and its siblings located in the statutory text (which is silent on factor weight and burden) or in the interpretive tradition layered above it — and does fixing the location change which reading a court can adopt?',
    'Doctrinal analysis: if the statute''s silence genuinely underdetermines weight and burden, the readings are interpretive choices over the same text; if the text''s structure (the preamble''s illustrative list, the factors'' ordering) favors one allocation, the contest is resolvable at the text.',
    'If underdetermined, the sibling readings are stable equilibria and the corpus should track which seats adopt which; if resolvable, foreclosure of one reading by another is a live possibility the engine should compute from axiom contradiction rather than assume.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_allocation_ambiguity, conceptual, 'Where the inter-reading disagreement is located: the text or the interpretive tradition above it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fair_tr_t8, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(fair_tr_t16, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(fair_tr_t24, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(fair_tr_t32, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 32, 0.49).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(fair_be_t8, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(fair_be_t16, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement(fair_be_t24, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement(fair_be_t32, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(fair_su_t8, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(fair_su_t16, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(fair_su_t24, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(fair_su_t32, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(fair_su_t40, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, user_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, transformative_use_reading).

% DUAL FORMULATION NOTE:
% Constraint family: 'fair use' is one contested kernel (the four-factor provision), instantiated as three structurally distinct constraints by three readings. This story is the creator-centric reading: high extraction on unauthorized use, rights holders as primary beneficiaries, victims = transformative users and the future commons. The user-centric reading relocates extraction onto enforcement against users and shrinks the victim set; the transformative-use reading lowers extraction specifically on meaning-adding uses and subordinates the market-harm factor. Contamination propagates across the family through doctrinal precedent rather than shared administration: each reading cites the same canonical opinions as warrant, so a shift in one reading's holdings changes the resource base and legitimacy conditions of the others. The creator-centric reading is upstream (older lineage, Folsom-to-Harper-&-Row), the transformative-use reading downstream (Campbell-to-Warhol), with the user-centric reading running as a persistent minority current beneath both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
