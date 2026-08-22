% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__enclosure_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Derivative Work Statutory Boundary - Enclosure Reading (Any Expressive Use Requires Authorization)
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   A regime in which the statutory derivative-work boundary is drawn at its
 *   widest: any incorporation of copyrighted expression into a new work -
 *   sampling, quotation, translation, format-shifting, sequelization, reuse
 *   of protectable stylistic elements - constitutes preparation of a
 *   derivative work, activating the rights-holder's exclusive control and a
 *   clearance requirement that operates before creation. Enforcement runs
 *   through infringement litigation backed by statutory damages untethered
 *   from proved harm, pre-publication demand letters, and platform-level
 *   upload filtering. Operationally, licensing markets channel payment for
 *   expressive inputs to catalog owners and their intermediaries, while
 *   categories of downstream work that cannot clear rights contract, delay,
 *   or move into informal circulation. KEY AGENTS (by structural
 *   relationship): incumbent_rights_holders - primary beneficiary and
 *   agenda-setter (institutional/arbitrage), collects license fees, damages,
 *   and settlements and shapes the boundary through litigation and lobbying;
 *   licensing_intermediaries - secondary beneficiary (institutional/mobile),
 *   takes commissions on cleared uses; downstream_creators - primary target
 *   (moderate/constrained), bears clearance costs and liability exposure;
 *   user_generated_content_platforms - mass-scale target with partial
 *   arbitrage (powerful/arbitrage); educational_archival_institutions -
 *   mission-bound target (organized/constrained);
 *   legislative_judicial_apparatus - agenda-setter
 *   (institutional/constrained), writes and construes the boundary;
 *   transformative_use_communities - excluded seat
 *   (powerless/identity_locked); ip_policy_scholars - analytical observer.
 *   This file instantiates one reading of the
 *   derivative_work_statutory_boundary kernel; the family decomposition and
 *   sibling deltas are recorded in commentary.kernel_context and
 *   network.dual_formulation_note.
 *
 * KEY AGENTS:
 *   - incumbent_rights_holders: primary beneficiary and agenda-setter (institutional/arbitrage) - collects license fees, damages, and settlements; shapes the boundary through litigation strategy and lobbying
 *   - licensing_intermediaries: secondary beneficiary (institutional/mobile) - operates clearance machinery and collects commissions
 *   - downstream_creators: primary target (moderate/constrained) - bears pre-creation clearance costs and statutory-damages exposure
 *   - user_generated_content_platforms: mass-scale target with arbitrage options (powerful/arbitrage)
 *   - educational_archival_institutions: mission-bound target (organized/constrained) - digitization and teaching chilled by unresolved rights
 *   - legislative_judicial_apparatus: agenda-setter (institutional/constrained) - legislates and construes the boundary under treaty and precedent constraints
 *   - transformative_use_communities: excluded seat (powerless/identity_locked) - practice constituted by engagement with existing works, no seat in negotiation
 *   - ip_policy_scholars: analytical observer (analytical/analytical) - tracks clearance costs and innovation effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.78).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.76).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Derivative Work Statutory Boundary - Enclosure Reading (Any Expressive Use Requires Authorization)").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '37feede9-f5be-4177-8c92-6214795a3135').
narrative_ontology:cs_kernel_codification('37feede9-f5be-4177-8c92-6214795a3135', fixed_text).
narrative_ontology:cs_authority_grounding('37feede9-f5be-4177-8c92-6214795a3135', lineage).
narrative_ontology:cs_interpretation_layer_present('37feede9-f5be-4177-8c92-6214795a3135').
narrative_ontology:cs_reading_relation('37feede9-f5be-4177-8c92-6214795a3135', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('37feede9-f5be-4177-8c92-6214795a3135', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('37feede9-f5be-4177-8c92-6214795a3135', foundational, any_expression_use_is_derivative_preparation).
narrative_ontology:cs_axiom_status(any_expression_use_is_derivative_preparation, holdable).
narrative_ontology:cs_axiom_grounding('37feede9-f5be-4177-8c92-6214795a3135', any_expression_use_is_derivative_preparation, deontological).
narrative_ontology:cs_axiom('37feede9-f5be-4177-8c92-6214795a3135', secondary, authorization_precedes_creation).
narrative_ontology:cs_axiom_status(authorization_precedes_creation, holdable).
narrative_ontology:cs_axiom_grounding('37feede9-f5be-4177-8c92-6214795a3135', authorization_precedes_creation, conventional).
narrative_ontology:cs_reference_frame('37feede9-f5be-4177-8c92-6214795a3135', author_exclusive_expression_control).
narrative_ontology:cs_drift_state('37feede9-f5be-4177-8c92-6214795a3135', contemporary_transformative_use_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('37feede9-f5be-4177-8c92-6214795a3135', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, downstream_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, user_generated_content_platforms).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, educational_archival_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own large catalogs of recorded music, film, publishing, and software. License adaptations, translations, formats, and sequels; initiate infringement actions and negotiate settlements; fund trade associations and legislative campaigns that shape the boundary and its procedures. Income arrives as license fees, damages, and settlements. Diversified portfolios across media and territories allow restructuring when rules shift in any single jurisdiction.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders, agenda_setter).

% Run the clearance machinery: collective rights organizations, stock-footage and sample libraries, reprographic rights agencies. Process authorization requests, maintain the databases that make searching possible, and take a commission on each cleared use. Services can be repriced or relocated as markets change.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries, beneficiary,
    institutional, biographical, mobile, global).

% Make new work out of existing expression: musicians building on recordings, video essayists quoting footage, novelists answering earlier novels, developers extending game worlds. Every expressive borrowing carries liability exposure with statutory damages far beyond personal means. Where rights can be located and afforded, they pay; where not, projects are abandoned or inputs silently dropped. Shifting to wholly original material changes what the work can say and who will hear it.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, downstream_creators, payer,
    moderate, biographical, constrained, global).

% Host billions of uploads that may incorporate protected expression. Operate identification and takedown systems, respond to demand letters, and settle claims on behalf of users. Scale brings negotiating leverage and options unavailable to smaller actors: geofencing content, acquiring catalogs, or shifting investment toward owned material.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, user_generated_content_platforms, payer,
    powerful, generational, arbitrage, global).

% Libraries, museums, universities, and archives preserve, digitize, exhibit, and teach with materials containing protected expression. Missions span longer than careers and budgets are fixed; unresolved rights stall digitization queues indefinitely, and holdings tie institutions to particular jurisdictions and donor terms.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, educational_archival_institutions, payer,
    organized, civilizational, constrained, national).

% Legislatures write the definitions, damage levels, and procedures; courts construe the boundary case by case and manage its interaction with fair-use defenses. Both are bound by constitutional grants, treaty commitments, precedent, and the reliance interests earlier decisions created.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, legislative_judicial_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Fan-fiction writers, vidders, remixers, and amateur translators create outside formal channels, and engagement with existing works is the substance of their practice rather than a means to it. They hold no seat in negotiations or hearings; their works circulate informally and their views reach policy mainly through proxy organizations.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, transformative_use_communities, excluded,
    powerless, biographical, identity_locked, global).

% Information-law researchers and economists track clearance costs, litigation outcomes, and innovation effects across the system, publishing analyses any seat can cite. Speaking requires no license and their income depends on no particular outcome.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ip_policy_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real but narrow coordination problem: a bright-line rule tells creators in advance exactly when authorization is needed, removing case-by-case uncertainty about which uses require clearance, and a licensing market exists to execute the resulting transactions.
% TRANSFER_FUNCTION: Moves money (license fees, statutory damages, settlement payments) and creative latitude from downstream creators, platforms, and institutions to incumbent rights-holders and licensing intermediaries; moves liability risk onto anyone whose work incorporates existing expression.
% ABSENT_VOICES: Transformative-use communities - fan-fiction writers, vidders, remixers, hobbyist translators - hold no seat in licensing negotiations or legislative hearings dominated by industry associations; orphan-works users and unrepresented future creators are likewise absent. Their objections surface mainly through proxy organizations filing amicus briefs.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, sampling, quotation, documentary reuse, and informal adaptation would resume without clearance within weeks; the licensing market for adaptations and the filtering infrastructure built to enforce it would lose their object; catalog owners would lose adaptation revenue and negotiating leverage; whole genres currently suppressed by clearance cost would re-enter circulation.
% FOUNDING_PROBLEM: Close a gap around the reproduction right: unauthorized recastings - translations, abridgments, dramatizations, musical arrangements - appropriated an author's expression while technically avoiding literal copying, and the derivative-work right was built to reach them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: federal appellate opinions applying the doctrine far beyond classic recastings, the copyright office's orphan-works proceeding, library and archive association filings on stalled digitization, and transaction-cost studies of sampling and footage clearance all attest that the anti-recasting problem is substantially solved while the arrangement now governs ordinary reuse. Industry submissions attesting a live problem exist but originate in the beneficiary set and are discounted accordingly.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__enclosure_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78) because the transfer is decoupled from upstream marginal cost: a cleared use pays the same whether the rights-holder incurs anything or not, statutory damages detach payment from proved loss, and clearance is demanded before creation so bargaining occurs under threat of ruinous liability. Suppression (0.76) is authored as a raw structural property - the engine scales only extractiveness - reflecting dependence on active machinery: litigation, takedown procedures, automated filtering, and the chilling effect of doctrinal uncertainty, which suppresses alternatives without ever adjudicating them. Theater ratio (0.32) is moderate-low: protecting authors against parasitic recasting is a real function for a core of cases, but a growing share of enforcement activity defends catalog-wide licensing positions rather than identifiable author harm. Accessibility collapse (0.62): wholly original creation remains open, so alternatives never collapse completely, but once a creator understands that any expressive input triggers liability, every route that builds on existing work closes. Resistance (0.58) is substantial and organized: remix communities, platforms, libraries, and scholars contest the boundary continuously, and periodic fair-use victories show the arrangement is defended rather than self-executing. The claimed type (snare) is stated from structure - identifiable victims, a thin coordination function relative to the transfer, persistence dependent on enforcement - while the metrics are authored descriptively; any divergence between the claim and computed seat-level types is left to the engine. All three tracked series share one grid (t=0..50, step 10; roughly 1976-2026 at one year per unit). The suppression_requirement series is authored because the story specifically tracks enforcement-capacity build-up - damage levels, procedural ratchets, filtering infrastructure - not a static enforcement picture.
 *
 * PERSPECTIVAL GAP:
 *   The catalog-owner seat experiences the arrangement as the legitimate exercise of property it holds, defends, and monetizes; the downstream-creator seat meets the same rules as a pre-creation toll gate with catastrophic downside and no adjudication until after the work exists. The platform seat prices the arrangement as negotiable overhead at scale; the archive seat experiences it as permanently stalled missions. These divergences follow from differences in power, exit, and role that the engine computes per seat; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (incumbent_rights_holders, licensing_intermediaries) sit near the beneficiary end; declared victims sit near the target end, differentiated by exit: user_generated_content_platforms' arbitrage-grade exit pulls their effective position well below trapped or constrained targets of equal nominal exposure, while educational_archival_institutions' constrained exit and civilizational horizon keep them near full-target despite organized power. downstream_creators combine moderate power with constrained exit and biographical stakes. The excluded transformative_use_communities carry identity-locked exit - their creative practice is constituted by engagement with existing works - pushing them toward the full-target end despite powerlessness. The legislative_judicial_apparatus administers without collecting, deriving near-symmetric. No directionality_overrides are authored: the derivation chain from declared roles, power, and exit reproduces the intended ordering, and overrides key on the power atom alone, which would misfire across the three institutional seats that hold different structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - stopping parasitic recastings such as unauthorized translations, abridgments, and dramatizations that evaded the reproduction right - is at minimum contested as solved; the arrangement now governs ordinary creative influence across every medium. The classification guards against mislabeling in both directions: calling the arrangement pure coordination launders a transfer concentrated on identifiable victims; calling it simple confiscation ignores the functioning licensing market and the residual author-protection core that gives the coordination story its plausibility. The snare claim rests on the combination - thin coordination relative to a broad, enforcement-dependent transfer. The R5 mismatch consumer should read founding_problem_status=contested against disappearance_verdict=world_rearranges and cross-check the resulting capture/zombie flag against the computed theater path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the enclosure_reading of the derivative_work_statutory_boundary kernel - the claim that any use of copyrighted expression in creating new work constitutes preparation of a derivative work. How would classification change under a sibling reading?',
    'Re-author the story under coordination_reading (only fixed recastings substantially incorporating original expression count) or hybrid_carveout_reading (commerciality-conditioned boundary) and recompute epsilon, the beneficiary/victim sets, and the type from the altered trigger scope.',
    'Under coordination_reading, epsilon falls sharply as most contested uses leave the trigger set and the arrangement trends toward coordination cost; under hybrid_carveout_reading the victim set splits by commerciality and epsilon lands intermediate. This story''s snare verdict is conditional on the enclosure trigger scope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame routing: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    trigger_scope_disagreement_location,
    'Where exactly do the readings of the kernel disagree - which structural element carries the contest?',
    'Locate the dispute in the trigger-scope element: what act counts as preparation - any expressive use (this reading), fixed substantial recasting (coordination_reading), or commerciality-conditioned use (hybrid_carveout_reading). Adjudication data: appellate constructions of the derivative-work definition and its interaction with fair-use defenses.',
    'Because the disagreement sits in trigger scope rather than in remedies or ownership, resolving it rewrites the victim set wholesale rather than adjusting magnitudes; foreclosure between readings follows from the same element.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trigger_scope_disagreement_location, conceptual, 'The kernel contest is located in the definition of the regulated act (trigger scope), not in remedies or ownership.').

omega_variable(
    reading_vs_doctrinal_baseline,
    'Does the enclosure reading describe operative doctrine or a maximalist aspirational position relative to settled fair-use jurisprudence?',
    'Compare the reading''s trigger scope against appellate fair-use outcomes in the transformative-use line and against enforcement practice: if routine expressive reuse routinely survives litigation, the operative arrangement is narrower than the reading claims.',
    'If the reading overstates operative law, the measured epsilon reflects the reading''s claimed regime rather than enforced reality; the story then measures the enclosure program''s target state, which matters for any cross-reading comparison within the kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_doctrinal_baseline, empirical, 'Gap between the enclosure reading''s claimed trigger scope and enforced doctrine.').

omega_variable(
    ex_ante_licensing_administrability,
    'Is ex ante licensing across all expressive inputs administrable at feasible transaction cost, or does the clearance burden function as a categorical barrier?',
    'Transaction-cost studies of licensing markets (music sampling, footage archives, text and data mining), clearance failure rates, and surveys of abandoned projects.',
    'If administrable, the transfer resembles priced access and the coordination component strengthens; if not, the arrangement operates as a bottleneck that suppresses entire categories of downstream work, supporting the snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ex_ante_licensing_administrability, empirical, 'Whether the pre-creation licensing requirement is a price or a barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dwsb_enclosure_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(dwsb_enclosure_tr_t0, observed).
narrative_ontology:measurement(dwsb_enclosure_tr_t10, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement_basis(dwsb_enclosure_tr_t10, observed).
narrative_ontology:measurement(dwsb_enclosure_tr_t20, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(dwsb_enclosure_tr_t20, observed).
narrative_ontology:measurement(dwsb_enclosure_tr_t30, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 30, 0.26).
narrative_ontology:measurement_basis(dwsb_enclosure_tr_t30, observed).
narrative_ontology:measurement(dwsb_enclosure_tr_t40, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 40, 0.29).
narrative_ontology:measurement_basis(dwsb_enclosure_tr_t40, observed).
narrative_ontology:measurement(dwsb_enclosure_tr_t50, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement_basis(dwsb_enclosure_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(dwsb_enclosure_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(dwsb_enclosure_be_t0, observed).
narrative_ontology:measurement(dwsb_enclosure_be_t10, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(dwsb_enclosure_be_t10, observed).
narrative_ontology:measurement(dwsb_enclosure_be_t20, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(dwsb_enclosure_be_t20, observed).
narrative_ontology:measurement(dwsb_enclosure_be_t30, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(dwsb_enclosure_be_t30, observed).
narrative_ontology:measurement(dwsb_enclosure_be_t40, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 40, 0.73).
narrative_ontology:measurement_basis(dwsb_enclosure_be_t40, observed).
narrative_ontology:measurement(dwsb_enclosure_be_t50, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 50, 0.78).
narrative_ontology:measurement_basis(dwsb_enclosure_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(dwsb_enclosure_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(dwsb_enclosure_su_t0, observed).
narrative_ontology:measurement(dwsb_enclosure_su_t10, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(dwsb_enclosure_su_t10, observed).
narrative_ontology:measurement(dwsb_enclosure_su_t20, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 20, 0.57).
narrative_ontology:measurement_basis(dwsb_enclosure_su_t20, observed).
narrative_ontology:measurement(dwsb_enclosure_su_t30, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement_basis(dwsb_enclosure_su_t30, observed).
narrative_ontology:measurement(dwsb_enclosure_su_t40, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(dwsb_enclosure_su_t40, observed).
narrative_ontology:measurement(dwsb_enclosure_su_t50, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 50, 0.76).
narrative_ontology:measurement_basis(dwsb_enclosure_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'derivative work boundary' decomposes, per the epsilon-invariance principle, into three structurally distinct readings of one kernel. This file instantiates the enclosure_reading with high epsilon (any expressive use triggers the requirement; victims span all downstream creation). coordination_reading narrows the trigger to fixed substantial recastings, collapsing epsilon toward coordination cost; hybrid_carveout_reading conditions the boundary on commercial exploitation, splitting the victim set. The enclosure reading is downstream of the others in rhetorical practice - maximalist positions cite the breadth of the statutory definition as evidence against narrower readings - so this story links to both siblings. Each member holds a single stable epsilon; the contest lives in the trigger-scope element, routed to omegas rather than folded into this classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
