% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__coordination_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__coordination_reading
 *   human_readable: Derivative Works Boundary — Coordination Reading (Narrow Fixed-Recasting Zone)
 *   domain: legal/intellectual_property/technological
 *
 * SUMMARY:
 *   This story instantiates the coordination reading of the derivative-work
 *   kernel: the statutory category reaches only fixed recastings that
 *   substantially incorporate original expression, leaving transformative
 *   works and intermediate steps — criticism, parody, indexing, accessibility
 *   copying, machine-learning training — outside infringement. Epsilon's
 *   referent is the standing United States arrangement (the 1976 Act's
 *   definitions as narrowed by the fair-use case line), assessed by this
 *   reading's own lights, which is why epsilon is low: the arrangement's
 *   dominant operation enables rather than takes. The colloquial label 'the
 *   derivative work boundary' decomposes into three structurally distinct
 *   constraints — this narrow reading, the enclosure reading under which any
 *   use of expression prepares a derivative work, and the hybrid carve-out
 *   reading keyed to commercial exploitation — with different epsilon values,
 *   different beneficiary/victim structures, and different failure modes; per
 *   the epsilon-invariance principle they are separate files linked through
 *   network.affects_constraints, not one constraint with a measurement
 *   parameter. The interval maps 0 to the 1976 Act's codification year and 48
 *   to 2024, spanning the photocopy, home-video, sampling, search-engine, and
 *   machine-learning waves.
 *
 * KEY AGENTS:
 *   - us_copyright_legislature: agenda-setter (institutional/arbitrage) — writes and could redraw the statutory boundary
 *   - federal_courts: administering agenda-setter and analytical observer (institutional/analytical) — interprets the line case by case
 *   - transformative_creators: principal beneficiary (moderate/constrained) — recast existing expression without licensing
 *   - ai_developers: principal beneficiary (powerful/arbitrage) — train on corpora under the intermediate-use shield
 *   - corporate_rights_holders: dual-positioned cost-bearer (institutional/constrained) — pays on the freed zone, collects on the protected core
 *   - individual_authors: identity-locked cost-bearer (moderate/identity_locked) — fuses livelihood and selfhood with work control
 *   - grassroots_remix_communities: excluded voice (powerless/trapped) — lives in the free zone with no seat in its governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__coordination_reading, 0.22).
domain_priors:suppression_score(derivative_work_statutory_boundary__coordination_reading, 0.16).
domain_priors:theater_ratio(derivative_work_statutory_boundary__coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0.16).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__coordination_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__coordination_reading, rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__coordination_reading, "Derivative Works Boundary — Coordination Reading (Narrow Fixed-Recasting Zone)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__coordination_reading, "legal/intellectual_property/technological").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__coordination_reading, '8dded2d8-66a9-4e1e-857c-bcc12538d06a').
narrative_ontology:cs_kernel_codification('8dded2d8-66a9-4e1e-857c-bcc12538d06a', formalized).
narrative_ontology:cs_authority_grounding('8dded2d8-66a9-4e1e-857c-bcc12538d06a', lineage).
narrative_ontology:cs_interpretation_layer_present('8dded2d8-66a9-4e1e-857c-bcc12538d06a').
narrative_ontology:cs_reading_relation('8dded2d8-66a9-4e1e-857c-bcc12538d06a', derivative_work_statutory_boundary__enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('8dded2d8-66a9-4e1e-857c-bcc12538d06a', derivative_work_statutory_boundary__hybrid_carveout_reading, coexists_with).
narrative_ontology:cs_axiom('8dded2d8-66a9-4e1e-857c-bcc12538d06a', foundational, only_fixed_substantial_recastings_infringe).
narrative_ontology:cs_axiom_status(only_fixed_substantial_recastings_infringe, holdable).
narrative_ontology:cs_axiom_grounding('8dded2d8-66a9-4e1e-857c-bcc12538d06a', only_fixed_substantial_recastings_infringe, conventional).
narrative_ontology:cs_axiom('8dded2d8-66a9-4e1e-857c-bcc12538d06a', foundational, intermediate_copying_without_dissemination_noninfringing).
narrative_ontology:cs_axiom_status(intermediate_copying_without_dissemination_noninfringing, holdable).
narrative_ontology:cs_axiom_grounding('8dded2d8-66a9-4e1e-857c-bcc12538d06a', intermediate_copying_without_dissemination_noninfringing, instrumental).
narrative_ontology:cs_axiom('8dded2d8-66a9-4e1e-857c-bcc12538d06a', secondary, no_ex_ante_license_for_transformative_use).
narrative_ontology:cs_axiom_status(no_ex_ante_license_for_transformative_use, holdable).
narrative_ontology:cs_axiom_grounding('8dded2d8-66a9-4e1e-857c-bcc12538d06a', no_ex_ante_license_for_transformative_use, conventional).
narrative_ontology:cs_reference_frame('8dded2d8-66a9-4e1e-857c-bcc12538d06a', narrow_fixed_recasting_zone).
narrative_ontology:cs_drift_state('8dded2d8-66a9-4e1e-857c-bcc12538d06a', contemporary_ai_litigation_wave, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('8dded2d8-66a9-4e1e-857c-bcc12538d06a', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ai_developers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, ml_researchers).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, technology_platforms).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, libraries_archives).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, downstream_audiences).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, corporate_rights_holders).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__coordination_reading, individual_authors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, corporate_rights_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__coordination_reading, individual_authors).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, campbell_transformative_use_premise).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__coordination_reading, idea_expression_dichotomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and revises the statutory definitions that fix where the derivative-work line sits (17 U.S.C. §§ 101, 106(2)) and holds periodic hearings on whether machine-learning ingestion and other intermediate uses should require licenses. It can redraw the boundary by amendment, but faces entrenched coalitions on both sides and frequently chooses to let courts absorb the pressure instead.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, us_copyright_legislature, agenda_setter,
    institutional, generational, arbitrage, national).

% Interprets the boundary case by case — from Campbell v. Acuff-Rose through Authors Guild v. Google and the Warhol decision to the pending artificial-intelligence training suits. It administers the line by granting or denying relief and simultaneously appraises whether the doctrinal settlement still serves the progress clause, issuing reasoned opinions that become the working text of the rule.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, federal_courts, observer).

% Parodists, documentarians, critics, biographers, remix artists, and fan-fiction writers who recast existing expression into new works carrying new meaning or purpose. The boundary as read here keeps their unlicensed quotation and reuse lawful whenever the result transforms rather than substitutes for the source. Leaving the arrangement would mean either abandoning reference to existing culture or negotiating clearances they cannot afford, so they operate inside it continuously.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, transformative_creators, beneficiary,
    moderate, biographical, constrained, global).

% Companies and labs that train large models on web-scraped books, articles, images, and code. Under this reading, ingestion that stops at intermediate statistical representations — absent reproducing expression in outputs — falls outside the derivative-work category. They hold multiple fallbacks: switching to licensed datasets, shifting training to jurisdictions with text-and-data-mining exceptions, or generating synthetic corpora, which lets them reroute around adverse rulings faster than most actors.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ai_developers, beneficiary,
    powerful, biographical, arbitrage, global).

% Academic groups studying corpora, building benchmarks, replicating results, and publishing analyses that necessarily copy and redistribute fragments of copyrighted material. The reading shields this intermediate activity; researchers can also pivot topics, collaborate across borders, or move institutions with comparatively little friction.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ml_researchers, beneficiary,
    moderate, biographical, mobile, global).

% Publishing houses, studios, record labels, news organizations, and image agencies holding large catalogs. On one side of the line they cannot charge for the uses the boundary frees — training runs, quotation-driven criticism, short-form remixes — and they fund litigation and legislative campaigns to widen the category. On the other side they retain exclusive, enforceable licensing over fixed, market-facing recastings such as translations, dramatizations, sequels, and abridgments, and they collect there. They cannot exit the copyright system itself; they rebalance portfolios toward the formats they still control.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, corporate_rights_holders, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, corporate_rights_holders, beneficiary).

% Novelists, journalists, illustrators, and photographers whose income and self-conception are fused with control over their life's work. They experience unlicensed machine ingestion of their catalogs as dispossession they cannot consent to or revoke, and many organize against it. At the same time their own adaptation and translation deals sit squarely inside the protected core. Walking away from the question is not available to them: the works carry their names, and how the works are used is bound up with who they understand themselves to be.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, individual_authors, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__coordination_reading, individual_authors, beneficiary).

% Operators of user-generated-content services, search indexes, snippet engines, and generative product lines. Their business models rest on hosting transformative user works and on intermediate copying for indexing and retrieval that the narrow boundary keeps defensible. They can geofence services, restructure product lines, or negotiate blanket licenses jurisdiction by jurisdiction, giving them more routing freedom than any other seat.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, technology_platforms, beneficiary,
    institutional, biographical, arbitrage, global).

% Research libraries, national collections, and disability-services repositories that digitize holdings, run full-text search over millions of volumes, and produce accessible-format copies for print-disabled patrons. Mission- and budget-bound, they cannot relocate or restructure around an adverse ruling; their intermediate copying depends on the boundary staying narrow.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, libraries_archives, beneficiary,
    organized, generational, constrained, national).

% Readers, viewers, listeners, and tool users who receive the compounded output: searchable book archives, dense remix cultures, cheaper commentary, AI-assisted creation and research tools. They are diffuse and rarely organized around the issue, consuming whatever the arrangement yields without having chosen it; their benefit persists whether or not they ever attend to the underlying rule.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, downstream_audiences, beneficiary,
    powerless, generational, mobile, global).

% Fan-fiction writers, amateur vidders, meme makers, and noncommercial archivists who live entirely inside the zone the boundary frees but have no seat in congressional hearings or trade negotiations, where industry associations and major laboratories speak instead. If present they would press for an even wider free zone and for curbs on automated takedown machinery that currently chills lawful transformative speech. They are subject to whichever reading prevails and have no forum through which to exit that subjection.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, grassroots_remix_communities, excluded,
    powerless, biographical, trapped, global).

% Law-and-economics and critical intellectual-property scholars mapping the incentive-versus-access trade-off, modeling clearance costs, and auditing whether the doctrinal line tracks its stated justifications. They hold no material stake beyond analytic reputation and publish the assessments that courts and agencies later cite.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__coordination_reading, ip_policy_scholars, observer,
    moderate, biographical, analytical, global).

narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a guaranteed zone of unlicensed transformative and intermediate reuse of published expression — criticism, parody, scholarship, indexing, accessibility copying, model training — so cumulative creation and research proceed without per-use negotiation, while preserving an exclusive licensing zone for fixed, market-substituting recastings such as translations, dramatizations, and sequels.
% TRANSFER_FUNCTION: Moves freedom-to-build: unlicensed reuse opportunities pass from catalog owners to transformative creators, platforms, archives, and model developers. Reciprocally, exclusive control over fixed substantial recastings is delivered to rights holders as an enforceable entitlement backed by damages and injunction.
% ABSENT_VOICES: Grassroots remix communities and noncommercial archivists are absent from the hearings where industry associations and major AI laboratories speak; foreign creators bound by exported platform terms also lack a seat. Present, they would argue the free zone remains too narrow and that takedown enforcement chills lawful transformative expression.
% DISAPPEARANCE_RATIONALE: If the narrow boundary flipped overnight to the broadest competing reading, every quotation-fed critique, remix, archive scan, and training run would require an ex ante license. Clearance costs would strand small creators, halt much machine-learning research, and push platforms into blanket-fee regimes; the generative-technology stack would rebuild itself around licensing departments or migrate to permissive jurisdictions, and the shape of public discourse — which quotes, clips, and recuts constantly — would visibly contract.
% FOUNDING_PROBLEM: Reconciling author incentive with cumulative culture: the 1976 Act codified derivative works to protect adaptation markets, while the courts built the fair-use safety valve to keep transformative space open. The founding bargain was that protection reaches fixed, substituting recastings but not uses that transform meaning or stop at intermediate steps.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the Supreme Court's transformative-use articulations (Campbell v. Acuff-Rose; Authors Guild v. Google), Copyright Office studies on artificial intelligence and copyright, and the litigation dockets themselves attest that the line-drawing problem recurs with each reproduction technology. Rights-holder trade groups attest it from the opposing interest — disputing the current placement of the line, not the existence or liveness of the problem. No participant attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__coordination_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__coordination_reading_tests).
:- end_tests(derivative_work_statutory_boundary__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because the reading's dominant operation removes negotiation burdens rather than imposing payments: the costs it generates are bounded opportunity costs borne by catalog owners alongside a retained exclusive core, plus remedial damages against unauthorized fixed recastings — not a systematic transfer. Suppression is low (0.16): the arrangement coerces only at the narrow covered core and relies on ordinary litigation rather than excluding alternatives; voluntary licensing, Creative Commons, and contractual clearinghouses all remain open, hence accessibility_collapse 0.28. Resistance is high (0.62) because well-resourced rights-holder coalitions actively litigate and lobby against the narrow line — this is a rope that must be defended, not a fact that merely obtains. Theater_ratio is low (0.12): the standard is applied functionally in the overwhelming majority of cases, with a modest performative fringe of 'transformative' labels stretched over substitutive uses. The measurement series run on one shared grid (seven points at eight-year spacing) with all three metrics authored at every point; the trajectories are deliberately gentle and monotonic — the rule's text was stable across the interval while the economic value of what it freed grew with digital scale, so measured extractiveness crept upward without any doctrinal event driving a step change. Suppression_requirement is included because enforcement capacity genuinely changed shape (from occasional suits to mass digital litigation and automated takedown), though only mildly; a flat scalar would have hidden that hardening.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the engine derives this from the structural data. From the creator, researcher, and laboratory seats the arrangement presents as enabling infrastructure: the thing that makes building on prior culture lawful by default. From the catalog-owner seat the same line presents as uncompensated appropriation of a licensing market — corporate rights holders experience the freed zone as a taking and the protected core as their compensation, which is why they are dual-positioned. Individual authors add an identity dimension the corporate seat lacks: the dispute is not only about revenue but about authorship itself. Courts occupy a mediating seat, administering the line while appraising it. No authored claim adjudicates among these perceptions; the divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (transformative creators, AI developers, ML researchers, platforms, archives, audiences) derive low directionality — the arrangement subsidizes their activity. Declared cost-bearers (corporate rights holders, individual authors) derive high directionality on the freed zone, moderated by their secondary beneficiary position on the protected core; their constrained and identity_locked exits keep them nearer the target end than a mobile holder would sit. No directionality_overrides are authored: the derivation from beneficiary/victim declarations plus exit options captures each seat's relationship, and an override would key on the power atom alone — overriding 'institutional', say, to soften the rights-holder seat would simultaneously distort platforms, courts, and the legislature, which share that atom but not that relationship. The asymmetry between the dual-positioned institutional seat and the single-positioned institutional seats is carried by the structural declarations, not by override arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling author incentive with cumulative culture — is live: each reproduction technology has reopened the identical line-drawing fight, and the R5 interview records live status corroborated from outside the benefiting parties, so no mandate-decay flag arises and none should. Classification discipline cuts both ways here. Reading the rights-holders' costs as extraction would mislabel a coordination mechanism as a snare: what they forgo is a class of licensing opportunities deliberately left outside the property grant, while their core adaptation market stays intact — a bounded, designed limitation, not coerced transfer. Conversely, reading the free zone as mere subsidy would miss that the arrangement actively suppresses nothing and excludes nobody from contracting, which is what separates it from arrangements whose openness is cover. The constraint is not transitional — the free zone is steady-state infrastructure with no sunset logic — so scaffold is structurally unavailable, and its function is exercised at scale daily, so piton's atrophy test fails plainly. Identity-lock note: individual authors' exit is locked by professional-relational fusion of self with the work; if that frame broke — authors collectively adopting a remixer self-conception, as some copyleft movements model — their seat would soften toward symmetric, lowering aggregate resistance and easing the boundary's defense.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'What structurally changes if the enclosure_reading or hybrid_carveout_reading displaces this coordination reading as the operative constraint?',
    'Track which reading appellate courts codify and which legislative proposals (text-and-data-mining licensing regimes) advance; recompile the sibling stories and compare computed classifications across seats.',
    'Under the enclosure reading the beneficiary/victim sets invert — creators and laboratories become cost-bearers, catalog owners collect on every use — and epsilon rises steeply, drifting the arrangement toward extraction-shaped operation. Under the hybrid reading the seats split by commerciality, producing a two-tier zone in which noncommercial actors keep the subsidy and commercial actors face ex ante licensing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: this story is one reading of a contested kernel; sibling readings would restructure the constraint entirely.').

omega_variable(
    model_weights_as_fixed_recasting,
    'Do trained model weights constitute a ''fixed'' form that substantially incorporates stored expression, thereby placing intermediate training inside the derivative-work category?',
    'Memorization and regurgitation evidence from model audits, combined with appellate treatment of whether weights are copies in the relevant statutory sense; watch for output-similarity findings that pierce the intermediate-step shield.',
    'If weights are treated as substantially incorporating source expression, the training permissibility collapses, the free zone shrinks to human transformative uses, epsilon rises sharply, and this reading converges toward the hybrid or enclosure positions without formal abandonment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(model_weights_as_fixed_recasting, empirical, 'Load-bearing uncertainty for the ML-training limb of the narrow boundary.').

omega_variable(
    commerciality_drift_toward_carveout,
    'Will factor-one fair-use analysis absorb commerciality distinctions until the hybrid_carveout_reading governs in practice even while the definitional reading remains nominally in force?',
    'Longitudinal coding of fair-use and derivative-work outcomes by defendant commerciality in the post-Warhol case law; measure outcome divergence between commercial and noncommercial defendants on comparable uses.',
    'Practical convergence with the hybrid reading would split the seat map by commerciality — noncommercial beneficiaries retain the free zone while commercial actors face de facto licensing — changing computed classification without any formal textual change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commerciality_drift_toward_carveout, empirical, 'Whether commerciality re-enters through application despite exclusion from definition.').

omega_variable(
    international_tdm_fragmentation,
    'Does the narrow boundary survive extraterritorially where EU-style text-and-data-mining exceptions with opt-outs govern, reintroducing licensing for intermediate use?',
    'Compare training-data acquisition costs, opt-out prevalence, and research output across jurisdictions operating under divergent intermediate-use regimes.',
    'Jurisdictional fragmentation raises effective extraction globally even under unchanged domestic doctrine, and pushes multi-jurisdictional actors toward arbitrage exits, altering seat-level exit options and therefore per-seat classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_tdm_fragmentation, empirical, 'Extraterritorial durability of the coordination reading.').

omega_variable(
    transformative_label_gaming,
    'Is invocation of ''transformative use'' drifting toward performance — covering substitutive uses that borrow expression for its original purpose?',
    'Outcome coding comparing success rates of transformative-use claims in genuine commentary cases against substitution cases; monitor the theater-ratio series for acceleration beyond the gentle slope authored here.',
    'Rising theatricality would erode the reading''s low-theater profile, supply rights holders with credible bad-faith examples, and accelerate the enclosure counter-movement — degrading the coordination function the boundary exists to provide.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_label_gaming, empirical, 'Performative-drift risk inside the free zone''s own justification vocabulary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__coordination_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dwsb_coord_tr_t0, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement(dwsb_coord_tr_t8, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(dwsb_coord_tr_t16, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement(dwsb_coord_tr_t24, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(dwsb_coord_tr_t32, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 32, 0.09).
narrative_ontology:measurement(dwsb_coord_tr_t40, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(dwsb_coord_tr_t48, derivative_work_statutory_boundary__coordination_reading, theater_ratio, 48, 0.12).

% Extraction over time
narrative_ontology:measurement(dwsb_coord_be_t0, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 0, 0.13).
narrative_ontology:measurement(dwsb_coord_be_t8, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 8, 0.14).
narrative_ontology:measurement(dwsb_coord_be_t16, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 16, 0.16).
narrative_ontology:measurement(dwsb_coord_be_t24, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(dwsb_coord_be_t32, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 32, 0.19).
narrative_ontology:measurement(dwsb_coord_be_t40, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 40, 0.2).
narrative_ontology:measurement(dwsb_coord_be_t48, derivative_work_statutory_boundary__coordination_reading, base_extractiveness, 48, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(dwsb_coord_su_t0, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(dwsb_coord_su_t8, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 8, 0.11).
narrative_ontology:measurement(dwsb_coord_su_t16, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 16, 0.12).
narrative_ontology:measurement(dwsb_coord_su_t24, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 24, 0.13).
narrative_ontology:measurement(dwsb_coord_su_t32, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 32, 0.14).
narrative_ontology:measurement(dwsb_coord_su_t40, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 40, 0.15).
narrative_ontology:measurement(dwsb_coord_su_t48, derivative_work_statutory_boundary__coordination_reading, suppression_requirement, 48, 0.16).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__coordination_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'derivative work boundary' decomposes into three epsilon-distinct constraints. This file is the coordination reading (narrow category, low epsilon, enabling-dominant). The enclosure reading (broad category, high epsilon, collection-dominant) and the hybrid carve-out reading (commerciality-keyed, intermediate epsilon, seat-splitting) instantiate different predicates over the same statutory text. The formalized 1976 Act text anchors all three as the common kernel; the coordination reading is the baseline against which the other two define themselves as widening amendments, and enclosure arguments are routinely advanced as evidence in hybrid proceedings. Per the epsilon-invariance principle each reading holds a single stable epsilon and its own beneficiary structure; measuring the boundary under different readings yields different constraints, not one constraint with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
