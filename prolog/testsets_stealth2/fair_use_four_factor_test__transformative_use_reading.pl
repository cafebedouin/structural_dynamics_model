% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__transformative_use_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__transformative_use_reading, []).

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
 *   constraint_id: fair_use_four_factor_test__transformative_use_reading
 *   human_readable: Transformative-Use-Dominant Fair Use Balancing (Reading of the Four-Factor Test)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   A judge-made allocation rule sits inside United States copyright law:
 *   when a court finds that a new work adds new meaning, message, or purpose
 *   to the material it draws on, that finding dominates the statutory
 *   four-factor balance, and evidence of harm to the original's licensing
 *   market recedes accordingly. Operationally, the question of
 *   transformativeness is asked first and usually decides the outcome; the
 *   remaining factors are argued and written down but seldom overturn the
 *   initial finding. The rule is maintained case by case — no registry
 *   defines the threshold, no fee schedule prices it, and each opinion
 *   adjusts the line the next litigants argue under. Creators of commentary,
 *   parody, mashups, documentaries, and computational research rely on it to
 *   use existing works without clearing rights; rights holders whose works
 *   feed such uses bear uncompensated appropriation whenever the threshold is
 *   crossed, and the size of that paying set moves with where courts place
 *   the line. Enforcement runs entirely through litigation, which
 *   concentrates the rule's maintenance in a small set of well-funded repeat
 *   players on both sides. KEY AGENTS (by structural relationship): -
 *   remix_and_commentary_creators: primary beneficiary (moderate/constrained)
 *   — makes meaning-adding reuse lawful without a license - ugc_platforms:
 *   primary beneficiary and gain recipient (institutional/arbitrage) —
 *   monetizes reuse at scale - licensing_photographers: primary target
 *   (powerless/trapped) — bears uncompensated appropriation when the
 *   threshold is crossed - stock_image_agencies: organized target
 *   (organized/constrained) — loses licensable universe incrementally -
 *   music_rights_holders: institutional target (institutional/constrained) -
 *   federal_judiciary: agenda setter (institutional/trapped) — places the
 *   threshold case by case - congress: excluded seat (institutional/mobile) —
 *   holds statutory authority, absent from the doctrinal conversation -
 *   digital_libraries: secondary beneficiary (organized/constrained) -
 *   documentary_filmmakers: secondary beneficiary (moderate/constrained) -
 *   ai_developers: prospective beneficiary (institutional/arbitrage) -
 *   legal_academy: analytical observer (moderate/analytical)
 *
 * KEY AGENTS:
 *   - remix_and_commentary_creators: primary beneficiary (moderate/constrained)
 *   - ugc_platforms: primary beneficiary and gain recipient (institutional/arbitrage)
 *   - licensing_photographers: primary target (powerless/trapped)
 *   - stock_image_agencies: organized target (organized/constrained)
 *   - music_rights_holders: institutional target (institutional/constrained)
 *   - federal_judiciary: agenda setter (institutional/trapped)
 *   - congress: excluded seat (institutional/mobile)
 *   - digital_libraries: secondary beneficiary (organized/constrained)
 *   - documentary_filmmakers: secondary beneficiary (moderate/constrained)
 *   - ai_developers: prospective beneficiary (institutional/arbitrage)
 *   - legal_academy: analytical observer (moderate/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, 0.57).
domain_priors:suppression_score(fair_use_four_factor_test__transformative_use_reading, 0.48).
domain_priors:theater_ratio(fair_use_four_factor_test__transformative_use_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, extractiveness, 0.57).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(fair_use_four_factor_test__transformative_use_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__transformative_use_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__transformative_use_reading, "Transformative-Use-Dominant Fair Use Balancing (Reading of the Four-Factor Test)").
narrative_ontology:topic_domain(fair_use_four_factor_test__transformative_use_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__transformative_use_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__transformative_use_reading, 'c6f08118-592a-4a23-8443-dea7a82990ce').
narrative_ontology:cs_kernel_codification('c6f08118-592a-4a23-8443-dea7a82990ce', formalized).
narrative_ontology:cs_authority_grounding('c6f08118-592a-4a23-8443-dea7a82990ce', lineage).
narrative_ontology:cs_interpretation_layer_present('c6f08118-592a-4a23-8443-dea7a82990ce').
narrative_ontology:cs_reading_relation('c6f08118-592a-4a23-8443-dea7a82990ce', fair_use_four_factor_test__creator_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6f08118-592a-4a23-8443-dea7a82990ce', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_axiom('c6f08118-592a-4a23-8443-dea7a82990ce', foundational, new_meaning_confers_reuse_privilege).
narrative_ontology:cs_axiom_status(new_meaning_confers_reuse_privilege, holdable).
narrative_ontology:cs_axiom_grounding('c6f08118-592a-4a23-8443-dea7a82990ce', new_meaning_confers_reuse_privilege, instrumental).
narrative_ontology:cs_axiom('c6f08118-592a-4a23-8443-dea7a82990ce', secondary, market_harm_subordinate_where_meaning_added).
narrative_ontology:cs_axiom_status(market_harm_subordinate_where_meaning_added, holdable).
narrative_ontology:cs_axiom_grounding('c6f08118-592a-4a23-8443-dea7a82990ce', market_harm_subordinate_where_meaning_added, conventional).
narrative_ontology:cs_reference_frame('c6f08118-592a-4a23-8443-dea7a82990ce', campbell_transformative_primacy).
narrative_ontology:cs_drift_state('c6f08118-592a-4a23-8443-dea7a82990ce', post_warhol_recalibration, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c6f08118-592a-4a23-8443-dea7a82990ce', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, remix_and_commentary_creators).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ugc_platforms).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, documentary_filmmakers).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, digital_libraries).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, licensing_photographers).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, stock_image_agencies).
narrative_ontology:constraint_victim(fair_use_four_factor_test__transformative_use_reading, music_rights_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__transformative_use_reading, ai_developers).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, transformative_use_doctrine).
narrative_ontology:constraint_vindicates(fair_use_four_factor_test__transformative_use_reading, first_amendment_safety_valve_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Make videos, mashups, parodies, essays, and memes that build on existing films, songs, photographs, and texts. Their work proceeds without a license when a court finds it adds new meaning or purpose; if the finding goes the other way they face takedowns and damages. Clearing every underlying work is impossible at their budgets, so their chosen mode of making depends on the defense staying available.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, remix_and_commentary_creators, beneficiary,
    moderate, biographical, constrained, global).

% Host and monetize billions of user uploads that quote, sample, and remix copyrighted material. Advertising and subscription revenue flows on that content; they absorb takedown volume under notice regimes and fund litigation defending broad reuse defenses. They can adjust terms of service, jurisdiction, and product design faster than rights holders can litigate.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ugc_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Use archival footage, news clips, and popular culture in nonfiction film under finite clearance budgets. Brief third-party material stays in the cut when it serves commentary; disputes surface as insurance negotiations and festival takedowns rather than courtroom losses.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, documentary_filmmakers, beneficiary,
    moderate, biographical, constrained, national).

% Scan, index, and provide computational access to book collections for search and research. Mass digitization was found defensible where the use serves discovery rather than reading substitution; they operate under standing litigation exposure from publisher plaintiffs.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, digital_libraries, beneficiary,
    organized, generational, constrained, continental).

% Earn income by licensing images. When editorial or artistic reuse of a photograph is found to add new meaning, they receive nothing and cannot withdraw the image from circulation; suing costs more than most licenses earn. Some respond by shifting to contract-bound commission work.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, licensing_photographers, payer,
    powerless, biographical, trapped, global).

% Aggregate and license image catalogs at scale. Each ruling blessing transformative reuse shrinks the licensable universe; they counter with stricter license terms, watermarking, litigation funding for photographer plaintiffs, and subscription models that price reuse in bulk.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, stock_image_agencies, payer,
    organized, biographical, constrained, global).

% Control composition and master-recording rights. Sampling and synchronization uses found transformative escape per-use fees; they respond with catalog acquisitions, blanket-license arrangements with platforms, and test-case litigation over borderline samples.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, music_rights_holders, payer,
    institutional, generational, constrained, global).

% Decides case by case whether a use adds new meaning and how much weight market harm receives. Each opinion adjusts the threshold the next litigants argue under; the court cannot decline the question when a defense is properly raised and is bound by its own prior formulations.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, trapped, national).

% Holds statutory authority over the four-factor text and has not substantively amended it since 1976. Members periodically introduce bills touching reuse and licensing, but the operative rules evolve in opinions rather than statutes; committee staff track the case law without steering it.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, congress, excluded,
    institutional, generational, mobile, national).

% Produces the treatises, empirical studies, and restatements both sides cite; maps how often each factor decides outcomes and documents the gap between the stated balancing method and actual results.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, legal_academy, observer,
    moderate, generational, analytical, global).

% Train generative models on corpora containing copyrighted works and argue that statistical learning adds new purpose. Whether their products fit the defense is unsettled; they fund test litigation and can relocate compute and release strategies across jurisdictions while cases pend.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__transformative_use_reading, ai_developers, beneficiary,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_four_factor_test__transformative_use_reading, ugc_platforms).
narrative_ontology:fixing_cost_class(fair_use_four_factor_test__transformative_use_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the clearance impossibility: criticism, commentary, parody, news reporting, teaching, scholarship, search indexing, and computational research all require using existing works at volumes where case-by-case licensing cannot function. The rule gives courts a flexible standard for separating culturally productive reuse from mere substitution without requiring legislative permission for each use.
% TRANSFER_FUNCTION: Moves control over new expressive uses — and the licensing revenue attached to that control — from rights holders to users and platforms whenever a court finds new meaning or purpose; moves litigation risk and legal-cost burden onto both sides; transfers expressive latitude from rights owners to the public.
% ABSENT_VOICES: Individual photographers and visual artists appear only when an organization funds their case; unregistered and non-market creators whose works circulate without representation have no seat; non-US rights holders subject to the doctrine's reach through global distribution lack any forum; Congress holds formal authority but does not participate in the case-law evolution that actually sets the rules.
% DISAPPEARANCE_RATIONALE: If meaning-adding uses lost their privileged status overnight, platforms would face mass licensing demands or mass takedowns, remix genres would migrate behind licensed templates or paywalls, documentary and library practices would restructure around clearance budgets, and a licensing market for previously free reuse categories would spring up — the current shape of online cultural production depends on the defense staying available.
% FOUNDING_PROBLEM: Reconcile exclusive-rights incentives with criticism, commentary, parody, news reporting, teaching, scholarship, and research that cannot practically clear rights case by case — the problem framed in the nineteenth-century case-law tradition and codified as four factors in the 1976 statute.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting set by the Supreme Court's Campbell opinion recognizing parody's protected social value, by Judge Leval's 1990 formulation predating the platform coalition that now funds the reading's defense, and by rights-holder-side counsel and scholars who concede the safety-valve function while disputing its breadth. No fully disinterested attestation exists, but corroboration is not confined to the beneficiary set.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__transformative_use_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__transformative_use_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__transformative_use_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fair_use_four_factor_test__transformative_use_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__transformative_use_reading, 0.57, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__transformative_use_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__transformative_use_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__transformative_use_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Time mapping: t0 = 1994 (the reading's adoption into controlling doctrine), t29 = 2023 (its recalibration); intermediate points at five-year steps. Extractiveness ends at 0.57: the rule transfers licensing control over meaning-adding uses to users and platforms without compensation, but the transfer is bounded by a threshold that excludes outright substitution, and the 2023 recalibration trimmed it back from its mid-2010s peak. Suppression is 0.48 and carries no time series: the enforcement picture is stable — maintenance runs through litigation exposure rather than any built-up coercive apparatus, and the same machinery that binds rights holders (no veto over transformative reuse) liberates users, so the scalar nets out moderate. Theater_ratio ends at 0.54: as factor-one dominance hardened, the four-factor balancing ritual grew increasingly performative — all four factors are weighed aloud while one decides — a textbook proxy-drift signature, partially reversing after 2023 when market-harm evidence regained genuine decision weight. Accessibility_collapse is 0.52: understanding the rule collapses a rights holder's refusal option almost entirely, but contractual, technical, and bulk-licensing alternatives persist at the margins. Resistance is 0.70: rights industries contest the reading continuously through funded litigation, amici campaigns, and licensing-model innovation. Claim and metrics are independently authored: the claimed type is tangled_rope because the structure holds a genuine coordination function (solving the clearance impossibility for criticism, parody, scholarship, and indexing) together with asymmetric, threshold-dependent extraction from a declared victim set, held in place by active case-by-case enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the platform and remixer seats the rule is the load-bearing wall of contemporary cultural production; from the individual photographer's seat it is a license nobody signed — the work leaves their control through someone else's court victory, and coalition remedies are weak because the paying class is atomized (each photographer faces the threshold alone, with suit costs exceeding typical license income). The judiciary's seat experiences a workable, administrable balancing method; the academy's seat documents that one factor decides most outcomes while four are ceremonially weighed. These are divergences the engine computes from the structural data; nothing here reconciles them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (remix creators, platforms, filmmakers, libraries) drive those seats toward the subsidized end of directionality; the payer declarations (photographers, agencies, music rights holders) drive theirs toward the target end, amplified by trapped and constrained exits — a photographer cannot recall a published image or afford the suit that would test the threshold, while agencies and labels cushion losses through catalog scale and blanket deals. Platform arbitrage exit keeps its exposure low, though it is a beneficiary seat regardless. The judiciary declares no beneficiary or victim position, so its directionality follows the story-level fallback; no directionality overrides were needed because the beneficiary/victim structure plus exit differentiation already separates the seats. Suppression enters the computation unscaled, as a raw structural property; only extractiveness is scaled by directionality and scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling exclusive rights with expressive uses that cannot practically clear rights case by case — remains live, so no mandate-atrophy resolution is declared. The rising theater ratio is the drift signal to watch: hardening factor-one dominance made the stated four-factor method progressively more performative relative to its function, and the 2023 recalibration partially reversed the trend rather than resolving it. Classifying the arrangement as a tangled rope rather than a rope keeps the threshold-dependent paying set visible; classifying it as a snare would erase the genuine coordination problem the rule solves. The classification therefore prevents symmetric mislabeling in both directions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positionality,
    'This constraint is one reading of the fair_use_four_factor_test kernel — how would instantiating the creator_centric_reading or user_centric_reading instead change the structural facts?',
    'Author the sibling stories as separate constraints and compare computed classifications; the delta surfaces in victim sets and effective extraction, never inside this file.',
    'A creator-centric instantiation would move the beneficiary set toward rights holders and raise measured costs for platforms and remixers; a user-centric instantiation would widen protected uses beyond meaning-adding ones and push extraction lower. This file''s classification must not be read as a verdict on the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positionality, conceptual, 'Committer-frame record: one of three live readings of the four-factor kernel.').

omega_variable(
    transformation_threshold_location,
    'Where does ''adds new meaning or purpose'' sit — minimal aesthetic alteration or a new communicative function — and which rights-holder classes land on the paying side at each placement?',
    'Track summary-judgment outcomes and circuit-level patterns across successive cases; the operative threshold reveals itself in which plaintiff classes keep losing.',
    'A low threshold pulls licensing photographers and visual artists firmly into the paying set and raises effective extraction; a high threshold shrinks the paying set toward outright substitution and lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformation_threshold_location, conceptual, 'The victim-set boundary moves with the contested location of the meaning threshold.').

omega_variable(
    warhol_recalibration_trajectory,
    'Does the post-2023 qualification of transformativeness primacy stabilize this reading or continue eroding it?',
    'Measure lower-court adherence to the renewed licensing-market emphasis in subsequent appellate decisions.',
    'Continued erosion pushes effective extraction down and restores factor-four weight; stabilization preserves the current metric profile and the current paying set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warhol_recalibration_trajectory, empirical, 'Whether the drift away from Campbell-era primacy is settling or accelerating.').

omega_variable(
    platform_capture_share,
    'What share of the value freed by subordinated market harm accrues to platforms rather than to individual creators?',
    'Revenue-attribution studies on transformative-format content cross-checked against direct creator compensation data.',
    'A high platform share supports treating the arrangement''s gains as concentrated in one seat; a diffuse share supports a broad public-benefit account and would weaken any single-seat receipt claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_capture_share, empirical, 'Concentration of realized gains between platform intermediaries and individual creators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__transformative_use_reading, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(fair_tr_t0, observed).
narrative_ontology:measurement(fair_tr_t5, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 5, 0.36).
narrative_ontology:measurement_basis(fair_tr_t5, observed).
narrative_ontology:measurement(fair_tr_t10, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 10, 0.44).
narrative_ontology:measurement_basis(fair_tr_t10, observed).
narrative_ontology:measurement(fair_tr_t15, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement_basis(fair_tr_t15, observed).
narrative_ontology:measurement(fair_tr_t20, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement_basis(fair_tr_t20, observed).
narrative_ontology:measurement(fair_tr_t25, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 25, 0.58).
narrative_ontology:measurement_basis(fair_tr_t25, observed).
narrative_ontology:measurement(fair_tr_t29, fair_use_four_factor_test__transformative_use_reading, theater_ratio, 29, 0.54).
narrative_ontology:measurement_basis(fair_tr_t29, observed).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(fair_be_t0, observed).
narrative_ontology:measurement(fair_be_t5, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(fair_be_t5, observed).
narrative_ontology:measurement(fair_be_t10, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement_basis(fair_be_t10, observed).
narrative_ontology:measurement(fair_be_t15, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement_basis(fair_be_t15, observed).
narrative_ontology:measurement(fair_be_t20, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(fair_be_t20, observed).
narrative_ontology:measurement(fair_be_t25, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(fair_be_t25, observed).
narrative_ontology:measurement(fair_be_t29, fair_use_four_factor_test__transformative_use_reading, base_extractiveness, 29, 0.57).
narrative_ontology:measurement_basis(fair_be_t29, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_four_factor_test__transformative_use_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__transformative_use_reading, resource_allocation).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__creator_centric_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__transformative_use_reading, fair_use_four_factor_test__user_centric_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'fair use' decomposes into at least three structurally distinct weighings of one statutory kernel. This file instantiates the transformative-dominance reading; the creator-centric and user-centric files instantiate the others. Their epsilon values differ because the victim set shifts with the transformation threshold: this reading's epsilon counts uncompensated appropriation borne by rights holders whose works receive meaning-adding reuse, whereas a creator-centric instantiation would count platform and remixer losses instead. All three are linked via affects_constraints per the constraint-family rule; neither sibling's content is averaged into this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
