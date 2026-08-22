% ============================================================================
% CONSTRAINT STORY: lausanne_minority_protections__restrictive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lausanne_minority_protections__restrictive_reading, []).

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
 *   constraint_id: lausanne_minority_protections__restrictive_reading
 *   human_readable: Restrictive Reading of the Lausanne Minority Protections (Individual Worship Only)
 *   domain: international_law/religious_governance/minority_rights
 *
 * SUMMARY:
 *   This story instantiates the RESTRICTIVE READING of the Lausanne
 *   minority-protections kernel: the claim that Articles 37-45 secure only
 *   individual freedom of worship, leaving institutional autonomy, property
 *   ownership, and theological education to general Turkish law. The epsilon
 *   referent is the standing arrangement under contest — the operative regime
 *   in which minority foundations are locked to 1936 asset declarations, the
 *   Patriarchate lacks legal personality, and Halki Seminary has been closed
 *   since 1971 — assessed as that arrangement actually operates, not as the
 *   expansive or guarantor siblings would render it. Per the
 *   epsilon-invariance principle, the colloquial label 'Lausanne protections'
 *   decomposes into three structurally distinct constraints: this file
 *   carries the restrictive scope reading (epsilon ~0.78, minority
 *   institutions in the victim set, state apparatus as consolidating
 *   beneficiary); the expansive reading (functional continuity of pre-1923
 *   governance) carries a different epsilon and a nearly inverted
 *   beneficiary/victim structure; the guarantor reading (external
 *   supervision) changes the enforcement locus rather than the scope. Each is
 *   a separate file; all three are linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship):
 *   turkish_state_apparatus — primary beneficiary and agenda-setter
 *   (institutional/arbitrage), authors the domestic interpretation and
 *   collects consolidated control; ecumenical_patriarchate — primary target
 *   (moderate/identity_locked), bears personality denial, property loss,
 *   educational foreclosure; armenian_apostolic_institutions and
 *   jewish_community_foundations — targets (moderate/constrained);
 *   minority_seminarians — target (powerless/trapped); guarantor_states —
 *   excluded actor whose supervisory role the reading denies;
 *   european_court_of_human_rights — analytical observer pricing individual
 *   denials without adopting any reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, 0.78).
domain_priors:suppression_score(lausanne_minority_protections__restrictive_reading, 0.72).
domain_priors:theater_ratio(lausanne_minority_protections__restrictive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(lausanne_minority_protections__restrictive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lausanne_minority_protections__restrictive_reading, snare).
narrative_ontology:human_readable(lausanne_minority_protections__restrictive_reading, "Restrictive Reading of the Lausanne Minority Protections (Individual Worship Only)").
narrative_ontology:topic_domain(lausanne_minority_protections__restrictive_reading, "international_law/religious_governance/minority_rights").

domain_priors:requires_active_enforcement(lausanne_minority_protections__restrictive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(lausanne_minority_protections__restrictive_reading, 'e78abad1-c2fd-4a74-ae0c-adc374a53d4e').
narrative_ontology:cs_kernel_codification('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', fixed_text).
narrative_ontology:cs_authority_grounding('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', extraction).
narrative_ontology:cs_interpretation_layer_present('e78abad1-c2fd-4a74-ae0c-adc374a53d4e').
narrative_ontology:cs_reading_relation('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', lausanne_minority_protections__expansive_reading, forecloses).
narrative_ontology:cs_reading_relation('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', lausanne_minority_protections__guarantor_reading, influences).
narrative_ontology:cs_axiom('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', foundational, lausanine_protections_individually_scoped).
narrative_ontology:cs_axiom_status(lausanine_protections_individually_scoped, holdable).
narrative_ontology:cs_axiom_grounding('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', lausanine_protections_individually_scoped, conventional).
narrative_ontology:cs_axiom('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', foundational, institutional_matters_domestically_reserved).
narrative_ontology:cs_axiom_status(institutional_matters_domestically_reserved, holdable).
narrative_ontology:cs_axiom_grounding('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', institutional_matters_domestically_reserved, conventional).
narrative_ontology:cs_reference_frame('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', sovereign_individual_worship_floor).
narrative_ontology:cs_drift_state('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', contemporary_strasbourg_jurisprudence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e78abad1-c2fd-4a74-ae0c-adc374a53d4e', '').
narrative_ontology:cs_kernel_id(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, armenian_apostolic_institutions).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, jewish_community_foundations).
narrative_ontology:constraint_victim(lausanne_minority_protections__restrictive_reading, minority_seminarians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, the Directorate General of Foundations, and the education ministry administer the general legal order applied to minority institutions: they register foundations, adjudicate property acquisitions, license schools, and determine which entities hold legal personality. Through its own judiciary the state authors the domestic interpretation of the treaty and collects consolidated control over minority institutional assets and functions. It faces no forum it accepts as competent to revise that interpretation, and it can amend the implementing statutes at will.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus, beneficiary).

% Administers a worldwide Orthodox communion from Istanbul. Turkish courts treat the Patriarchate itself as lacking legal personality, so it cannot hold title directly; property sits with a shrinking pool of foundations locked to their 1936 asset declarations. Its theological school at Halki has been closed since 1971 under the uniform higher-education law, forcing clergy formation abroad. Relocation would dissolve the office's constitutive claim; the institution's identity is fused with physical presence in the city, so exit is unthinkable from where it stands.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, ecumenical_patriarchate, payer,
    moderate, civilizational, identity_locked, global).

% Patriarchate, churches, schools, and foundations operate under the same foundation-law regime. The 1974 Court of Cassation doctrine annulled post-1936 acquisitions and triggered waves of liquidation; community emigration steadily shrinks the base that sustains the remainder. Strasbourg litigation is available but slow, and a favorable judgment prices a particular denial without returning the interpretive seat to the community.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, armenian_apostolic_institutions, payer,
    moderate, generational, constrained, national).

% Community trusts running hospitals, old-age homes, and schools were registered under the 1936 declarations; later acquisitions were annulled under the same doctrine. Partial restitutions followed the 2008 foundation-law amendments and the 2011 decree, but a rapidly shrinking population concentrates the maintenance burden on ever-fewer trustees with no domestic avenue to widen the trust's permitted holdings.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, jewish_community_foundations, payer,
    moderate, biographical, constrained, national).

% Would-be clergy who cannot enter a domestic seminary: Halki has been closed for five decades and parallel Armenian and rabbinical formation is constrained. Training requires emigration, and few return to communities too small to sustain them. Their vocational path is decided entirely by arrangements in which they hold no seat.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, minority_seminarians, payer,
    powerless, immediate, trapped, national).

% Signatory powers, and Greece claiming succession to guarantor status, assert a supervisory interest in the treaty's minority clauses. The restrictive reading classifies every covered institutional matter as domestic, leaving them diplomatic protest and third-party litigation rather than any supervisory procedure. Their exclusion from interpretation is precisely what this reading exists to maintain.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, guarantor_states, excluded,
    institutional, generational, mobile, continental).

% Hears property and education complaints from minority foundations — Fener Rum Lisesi Vakfi v. Turkey (2007) among them — and finds violations under Convention headings without adopting any reading of the treaty itself. It prices particular denials case by case while leaving the underlying interpretive contest untouched.
narrative_ontology:constraint_stakeholder(lausanne_minority_protections__restrictive_reading, european_court_of_human_rights, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(lausanne_minority_protections__restrictive_reading, turkish_state_apparatus).
narrative_ontology:fixing_cost_class(lausanne_minority_protections__restrictive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Administers a single uniform legal order across the territory: one foundation law, one education-licensing regime, one property registry, applied identically to every association regardless of confession — resolving the post-imperial problem of governing a multi-confessional population under one sovereign legal system without plural jurisdictions.
% TRANSFER_FUNCTION: Moves real property, endowment income, legal personality, and clerical-training capacity from minority religious institutions to state administrative control (Directorate General of Foundations, treasury, education ministry), and moves decision-making authority over minority institutional life from community organs to Turkish courts and ministries.
% ABSENT_VOICES: The minority institutions themselves and the guarantor powers sit outside the interpretive forum: the reading is authored by Turkish courts and administrative bodies in proceedings where the communities appear only as litigants, never as co-interpreters of the treaty they live under. Diaspora communities absorbing the downstream decay of closed schools and emptied foundations likewise hold no seat.
% DISAPPEARANCE_RATIONALE: If the restrictive arrangement vanished overnight, minority foundations would re-register post-1936 holdings, the Patriarchate would acquire recognized legal personality, Halki and parallel seminaries would reopen under community governance, and the Directorate General of Foundations would lose its custodial portfolio over minority assets — the state's administrative architecture for minority institutional life would have to be rebuilt around recognized communal self-government.
% FOUNDING_PROBLEM: The protections were built to solve the post-Ottoman transition problem: after the population exchange and wartime persecutions, to give the remaining non-Muslim populations enforceable guarantees against renewed persecution, expulsion, and expropriation within the new Turkish state.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: European Court of Human Rights judgments against Turkey on minority-foundation property, European Commission accession progress reports, U.S. International Religious Freedom reports, and UN treaty-body observations all attest that the protective problem remains live because the protected institutions have been hollowed out. The Turkish state attests the opposite — that the founding problem was resolved by equal citizenship under general law — which is the contest itself.
narrative_ontology:disappearance_verdict(lausanne_minority_protections__restrictive_reading, world_rearranges).
narrative_ontology:founding_problem_status(lausanne_minority_protections__restrictive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(lausanne_minority_protections__restrictive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(lausanne_minority_protections__restrictive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(lausanne_minority_protections__restrictive_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lausanne_minority_protections__restrictive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(lausanne_minority_protections__restrictive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lausanne_minority_protections__restrictive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because the arrangement transfers assets, legal personality, and training capacity from minority institutions to state control while the state itself sets the interpretive frame — the classic signature of extraction decoupled from any service rendered to the extracted. Suppression (0.72) is structural throughout: court doctrines, registry refusals, and licensing law, not internalized belief; the communities plainly want their institutions back, which is why suppression must be continuously administered. Theater (0.42) is substantial but sub-piton: recurring official pledges that Halki will reopen and successive 'reform' packages (2008 foundation law, 2011 decree) perform liberalization while the core denials persist — yet the enforcement machinery underneath is genuinely functional, so the ratio stays below proxy-replacement levels. Accessibility collapse is moderate (0.45): domestic alternatives are effectively closed, but Strasbourg litigation, diaspora advocacy, and informal communal operation remain partially available at high cost. Resistance (0.55) is real and sustained — litigation that has produced adverse judgments, diplomatic pressure that produced the partial restitutions — which is what a defended construct, not a natural fact, looks like. The measurement series run on one shared grid (T=0, 13, 48, 51, 85, 100 years from 1923, anchoring the 1936 declarations, the 1971 Halki closure, the 1974 Cassation doctrine, and the 2008 reform window) with every tracked metric authored at every point. The trajectory is monotone intensification with a single reform blip at T=85, not cyclical: the 2008-2011 relaxations were externally compelled concessions, after which extraction re-accumulated. Suppression_requirement is tracked because the story specifically traces enforcement-capacity hardening (ratchet through 1974, partial EU-pressure relaxation, re-hardening).
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute sharply different types from identical structural data. From the state apparatus's seat the arrangement presents as lawful uniformity — one legal order, equally applied, domestically interpreted — a rope-like experience of neutral administration. From the minority institutional seats the same structure operates as enforced extraction with suppressed exits: a snare. The guarantor states experience a third thing: a supervisory vacuum maintained by definitional fiat. The engine computes this divergence from power, exit options, and directional position; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus is the sole declared beneficiary and the agenda-setter: it writes the reading, administers the machinery, and receives the transferred assets and control, placing it near the full-beneficiary end (d near 0.0) with arbitrage-grade exit since it authored the frame it lives under. The four victim groups sit near the full-target end (d approaching 1.0), differentiated by exit: the Patriarchate is identity_locked (its constitutive claim fuses it to Istanbul, trapping it at maximal exposure), the seminarians are trapped (vocational path wholly dependent on the arrangement), and the Armenian and Jewish institutions are constrained (litigation and emigration exist but are slow and costly). Guarantor states are excluded rather than coordinated — the reading's enforcement object is precisely their exclusion from interpretation. Scope is national, which moderately amplifies effective extraction through verification difficulty, but the decisive amplifier is the identity-lock and trap profile of the targets.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting surviving non-Muslim populations from renewed persecution — has not died; it has been inverted, with the protective instrument repurposed as the administrative vehicle for institutional attrition. Because the mandate is contested rather than dead, this is not a clean mandatrophy case: the mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges, and the rearrangement verdict confirms the arrangement is load-bearing for someone (the state), not a zombie nobody profits from. The classification discipline matters here in both directions: labeling the arrangement rope (uniform law fairly applied) would erase the asymmetric extraction that the 1974 Cassation doctrine and the Halki closure exhibit; labeling it a piton would be wrong because the state demonstrably profits and actively defends the structure — concentrated capture, not inertial neglect. The snare claim keeps both truths visible: a real uniform-order coordination function exists, and it operates as the delivery mechanism for one-directional transfer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does the treaty text of Articles 37-45 admit the restrictive reading (individual worship only) or the expansive reading (functional continuity of pre-1923 institutional governance)? This constraint is one reading of kernel lausanne_minority_protections; the sibling readings instantiate different constraints with different epsilon values and victim sets.',
    'Authoritative interpretation — travaux preparatoires analysis, comparative treatment of mirror-image clauses (Western Thrace), or a binding international adjudication of the clauses'' scope rather than unilateral domestic construction.',
    'If the expansive reading prevails, the victim set collapses toward empty (property restored, seminaries reopened, personality recognized) and the state apparatus flips from beneficiary to constrained payer; if the restrictive reading stands, this story''s snare classification holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the Lausanne kernel the text itself supports.').

omega_variable(
    international_concern_clause_scope,
    'Article 44 declares the protected provisions ''obligations of international concern'' and places settlement disputes with the PCIJ/ICJ — does that clause import external supervision (moving this constraint toward the guarantor reading), or does the domestic-jurisdiction reservation dominate?',
    'ICJ advisory analysis or scholarly consensus on the operative effect of the Article 44 clause versus the reciprocity and domestic-administration provisions.',
    'If Article 44 imports supervision, the enforcement locus shifts externally and the state''s self-adjudication — the mechanism sustaining the extraction — is removed, materially lowering sustainable suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_concern_clause_scope, conceptual, 'Whether the treaty''s own supervision clause defeats the domestic-matter characterization.').

omega_variable(
    reciprocity_conditioning,
    'Turkey conditions minority treatment on Greece''s treatment of the Western Thrace Muslim minority; does observable reciprocity asymmetry actually track the claimed justification, or is it a post hoc cover for unilateral narrowing?',
    'Comparative audit of Western Thrace muftiate, waqf property, and minority-school outcomes against Istanbul minority-institution outcomes, controlling for each state''s overall rule-of-law baseline.',
    'If reciprocity fails as an empirical account, the restrictive reading loses its principal external justification and stands exposed as unilateral extraction; if it partially holds, part of the arrangement''s persistence is bilateral bargaining rather than pure capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_conditioning, empirical, 'Whether the reciprocity justification survives comparative evidence.').

omega_variable(
    reform_sincerity,
    'Are the recurring reform gestures — the 2008 foundation-law amendments, the 2011 restitution decree, decades of official pledges to reopen Halki — a genuine transition toward expansive compliance, or performative maintenance of the restrictive structure?',
    'Track whether pledged measures complete: seminary reopening with degree-granting status, wholesale property return, statutory legal personality. Completion indicates transition; indefinite pledge-without-execution indicates theater.',
    'If sincere, the constraint trends scaffold-like (transitional support during a managed shift) and theater_ratio should fall; if performative, theater_ratio continues rising and the snare classification hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_sincerity, empirical, 'Transition versus performance in the reform record.').

omega_variable(
    uniform_order_coordination_boundary,
    'Is the uniform-legal-order coordination function structurally separable from the denial of minority institutional autonomy — could communal self-governance in religion, property, and education coexist with a single criminal, commercial, and civil law — or are they inseparable?',
    'Comparative evidence from states that grant recognized religious-community legal personality and foundation autonomy inside a uniform civil order (most European states), demonstrating the functions coexist.',
    'If separable, the measured extraction is pure rent riding a thin coordination cover (snare confirmed); if inseparable, part of the measured extraction is the price of the coordination itself and the correct classification shifts toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(uniform_order_coordination_boundary, conceptual, 'Separability of the uniform-order coordination from the institutional denial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lausanne_minority_protections__restrictive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laus_tr_t0, lausanne_minority_protections__restrictive_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(laus_tr_t13, lausanne_minority_protections__restrictive_reading, theater_ratio, 13, 0.14).
narrative_ontology:measurement(laus_tr_t48, lausanne_minority_protections__restrictive_reading, theater_ratio, 48, 0.22).
narrative_ontology:measurement(laus_tr_t51, lausanne_minority_protections__restrictive_reading, theater_ratio, 51, 0.26).
narrative_ontology:measurement(laus_tr_t85, lausanne_minority_protections__restrictive_reading, theater_ratio, 85, 0.38).
narrative_ontology:measurement(laus_tr_t100, lausanne_minority_protections__restrictive_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(laus_be_t0, lausanne_minority_protections__restrictive_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(laus_be_t13, lausanne_minority_protections__restrictive_reading, base_extractiveness, 13, 0.45).
narrative_ontology:measurement(laus_be_t48, lausanne_minority_protections__restrictive_reading, base_extractiveness, 48, 0.68).
narrative_ontology:measurement(laus_be_t51, lausanne_minority_protections__restrictive_reading, base_extractiveness, 51, 0.75).
narrative_ontology:measurement(laus_be_t85, lausanne_minority_protections__restrictive_reading, base_extractiveness, 85, 0.7).
narrative_ontology:measurement(laus_be_t100, lausanne_minority_protections__restrictive_reading, base_extractiveness, 100, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(laus_su_t0, lausanne_minority_protections__restrictive_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(laus_su_t13, lausanne_minority_protections__restrictive_reading, suppression_requirement, 13, 0.45).
narrative_ontology:measurement(laus_su_t48, lausanne_minority_protections__restrictive_reading, suppression_requirement, 48, 0.62).
narrative_ontology:measurement(laus_su_t51, lausanne_minority_protections__restrictive_reading, suppression_requirement, 51, 0.7).
narrative_ontology:measurement(laus_su_t85, lausanne_minority_protections__restrictive_reading, suppression_requirement, 85, 0.65).
narrative_ontology:measurement(laus_su_t100, lausanne_minority_protections__restrictive_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lausanne_minority_protections__restrictive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__expansive_reading).
narrative_ontology:affects_constraint(lausanne_minority_protections__restrictive_reading, lausanne_minority_protections__guarantor_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Lausanne minority protections' decomposes into three structurally distinct constraints per the epsilon-invariance principle. This file (restrictive_reading) carries the scope-limiting reading: epsilon ~0.78, victims = minority institutions, beneficiary = state apparatus. The expansive_reading sibling carries the functional-continuity claim with a nearly inverted beneficiary/victim structure and much lower epsilon assessed from the minority seat. The guarantor_reading sibling holds scope constant but relocates enforcement externally, changing the sustainability of suppression rather than the extraction structure. The upstream/downstream dynamic runs from the treaty text's ambiguity through the restrictive reading's domestic entrenchment, which degrades the operating environment of the guarantor reading (each domestic-adjudication assertion lowers the traction of external supervision). All three files cross-link via affects_constraints; the sibling constraint_ids follow the same naming convention as this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
