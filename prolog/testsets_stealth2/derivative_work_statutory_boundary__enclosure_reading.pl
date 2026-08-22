% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: derivative_work_statutory_boundary__enclosure_reading
 *   human_readable: Enclosure Reading of the Derivative Work Boundary: Any Use of Expression Is a Preparable Derivative Work
 *   domain: intellectual_property/technology_governance/information_economics
 *
 * SUMMARY:
 *   Under this reading, the act of using copyrighted expression in creating a
 *   new work is itself the regulated event: sampling a recording, writing fan
 *   fiction, translating, adapting, quoting at length, building a dataset of
 *   expressive works for training or search — each constitutes preparation of
 *   a derivative work, and each requires authorization before creation
 *   begins. The rule operates as a pre-creation gate: clearance cost is
 *   incurred at the point of maximum uncertainty, before any work exists to
 *   generate revenue. Enforcement runs through cease-and-desist campaigns,
 *   platform-level takedown and content-identification systems, and test-case
 *   litigation funded by catalog owners; compliance runs through licensing
 *   departments and clearance houses embedded in every institution that
 *   touches culture at scale. The stated justification is that plenary
 *   control over downstream use protects the incentive to create the first
 *   work; the observable operation is a licensing market in which
 *   authorization decisions over cultural reuse concentrate in catalog-owning
 *   incumbents, estates, and intermediaries.
 *
 * KEY AGENTS:
 *   - incumbent_rights_holders: Primary beneficiary (institutional/arbitrage) — collects licensing revenue and sets enforcement strategy through funded test cases
 *   - licensing_intermediaries: Secondary beneficiary (institutional/mobile) — takes transaction fees on every authorized use; market grows with the boundary
 *   - creator_estates: Beneficiary (organized/arbitrage) — collects in perpetuity with no new works at stake
 *   - federal_courts: Agenda-setter (institutional/analytical) — sets the boundary's operative scope case by case
 *   - copyright_legislature: Agenda-setter (institutional/analytical) — wrote the definition and declines clarification under concentrated lobbying
 *   - downstream_creators: Primary target (moderate/constrained) — bears clearance cost before creation, at maximum uncertainty
 *   - remix_and_fan_creators: Primary target (powerless/trapped) — no non-infringing version of their practice exists
 *   - technology_platform_developers: Target (powerful/constrained) — processes expression at scale; can negotiate blanket licenses but not exit
 *   - libraries_archives_educators: Target (organized/constrained) — preservation and teaching uses become licensable events
 *   - unrepresented_transformative_creators: Excluded voice (powerless/trapped) — no seat in clearance, doctrine, or negotiation
 *   - ip_policy_scholars: Analytical observer — documents divergence between incentive rationale and royalty flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__enclosure_reading, 0.78).
domain_priors:suppression_score(derivative_work_statutory_boundary__enclosure_reading, 0.82).
domain_priors:theater_ratio(derivative_work_statutory_boundary__enclosure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__enclosure_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__enclosure_reading, snare).
narrative_ontology:human_readable(derivative_work_statutory_boundary__enclosure_reading, "Enclosure Reading of the Derivative Work Boundary: Any Use of Expression Is a Preparable Derivative Work").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__enclosure_reading, "intellectual_property/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__enclosure_reading, '60bd51e9-bb5e-4fc3-8440-74f18fbcecf4').
narrative_ontology:cs_kernel_codification('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', fixed_text).
narrative_ontology:cs_authority_grounding('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', extraction).
narrative_ontology:cs_interpretation_layer_present('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4').
narrative_ontology:cs_reading_relation('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', derivative_work_statutory_boundary__coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', derivative_work_statutory_boundary__hybrid_carveout_reading, forecloses).
narrative_ontology:cs_axiom('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', foundational, expression_employment_is_exercise_of_derivative_right).
narrative_ontology:cs_axiom_status(expression_employment_is_exercise_of_derivative_right, holdable).
narrative_ontology:cs_axiom_grounding('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', expression_employment_is_exercise_of_derivative_right, conventional).
narrative_ontology:cs_axiom('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', foundational, pre_creation_licensing_requirement).
narrative_ontology:cs_axiom_status(pre_creation_licensing_requirement, holdable).
narrative_ontology:cs_axiom_grounding('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', pre_creation_licensing_requirement, conventional).
narrative_ontology:cs_axiom('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', secondary, plenary_scope_maximizes_creation_incentives).
narrative_ontology:cs_axiom_status(plenary_scope_maximizes_creation_incentives, holdable).
narrative_ontology:cs_axiom_grounding('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', plenary_scope_maximizes_creation_incentives, instrumental).
narrative_ontology:cs_reference_frame('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', maximal_literal_statutory_scope).
narrative_ontology:cs_drift_state('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', transformative_use_jurisprudence_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('60bd51e9-bb5e-4fc3-8440-74f18fbcecf4', '2026-06-11T12:00:00Z').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__enclosure_reading, creator_estates).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, downstream_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, remix_and_fan_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, technology_platform_developers).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__enclosure_reading, libraries_archives_educators).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, incentive_maximization_hypothesis).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__enclosure_reading, plenary_expression_control_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Owns deep catalogs of recordings, films, publishing backlists, and character IP. Its revenue model increasingly runs on licensing adaptations, remakes, samples, quotations, and training corpora, so every expansion of what counts as a licensable use raises the value of the catalog. Enforces the boundary through cease-and-desist letters, platform takedown systems, and selectively funded test-case litigation, and shapes the boundary's interpretation through those cases. Exit is easy: it can reprice or restructure licenses, move catalogs between licensing regimes, and operate in whichever jurisdictions enforce most strongly.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders, agenda_setter).

% Publishers, collecting societies, clearance houses, and stock-content platforms that take transaction fees and administrative cuts on every authorized use. Their business exists because the boundary makes clearance mandatory before creation; when the boundary widens, their addressable market widens with it. They can reposition as the boundary moves and bear no creative risk themselves.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, licensing_intermediaries, beneficiary,
    institutional, biographical, mobile, global).

% Heirs and estates controlling works of deceased creators under life-plus-seventy terms. They collect licensing revenue on downstream uses indefinitely without any ongoing creative activity and have no new works whose incentives need protecting, which makes them the seat with the purest interest in the broadest possible reading of the boundary.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, creator_estates, beneficiary,
    organized, generational, arbitrage, global).

% Interpret the statutory definition case by case. Each technology wave — photocopying, digital sampling, search indexing, video platforms, machine learning — arrives as a new application of the same statutory text, and the courts' doctrinal choices set the boundary's operative scope. They neither collect licensing revenue nor bear clearance costs.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Wrote the derivative work definition and the exclusive right that attaches to preparing one, and has repeatedly declined to clarify the boundary despite reform proposals, leaving scope-setting to litigation. Subject to concentrated lobbying from catalog owners and diffuse, largely unorganized pressure from creator constituencies.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, copyright_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Documentarians, translators, biographers, novelists writing companion or sequel works, video essayists, and game modders whose creative practice inherently engages existing expression. Under the rule every project requires a clearance decision before creation begins, at the point of maximum uncertainty and minimum revenue. Exit means abandoning the medium or restricting work to public-domain material.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, downstream_creators, payer,
    moderate, biographical, constrained, global).

% Samplers, mashup artists, fan fiction writers, and meme creators whose entire form is transformation of protected expression. Under the rule every work they produce is a preparation requiring authorization they cannot afford to obtain; there is no non-infringing version of what they do. They lack the resources to license or to litigate, so their practical options are cessation, invisibility, or illegality.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, remix_and_fan_creators, payer,
    powerless, immediate, trapped, global).

% Search engines indexing text, video platforms hosting user uploads, digitization projects, and developers training systems on expressive corpora. Their products process existing expression at scale, so under the rule each processing step is a licensable preparation. They can negotiate blanket licenses and fund defenses, but they cannot exit the information ecosystem their products operate in, and clearance at their scale is a structural cost passed to users.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, technology_platform_developers, payer,
    powerful, biographical, constrained, global).

% Institutions performing preservation copying, format shifting, digitization, course reserves, and digital exhibits. Under the rule even preservation transformations become licensable events; they operate under shrinking confidence in unlicensed exceptions, fixed budgets that cannot absorb clearance at scale, and a mission that requires them to keep handling protected expression.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, libraries_archives_educators, payer,
    organized, generational, constrained, national).

% Would object that the boundary converts cultural reference itself into infringement, but they have no seat in licensing negotiations, no trade association of their own, and no litigation budget. Their interests reach doctrine only through amicus briefs filed by organizations they did not choose and through occasional test cases brought on others' resources.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, unrepresented_transformative_creators, excluded,
    powerless, immediate, trapped, global).

% Legal academics and economists studying the boundary's incentive and access effects. They neither collect licensing revenue nor bear clearance costs; their role is documenting the divergence between the incentive rationale and observed royalty flows, and modeling what creation rates look like under different boundary scopes.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__enclosure_reading, ip_policy_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__enclosure_reading, incumbent_rights_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a bright-line, ex ante definition of which downstream creative acts require authorization: a creator or platform can know before building on existing expression whether a license is needed and from whom, which makes rights clearance predictable and supports a standardized licensing market.
% TRANSFER_FUNCTION: Moves licensing revenue, and decision authority over the reuse of expression, from downstream creators and technology developers to catalog-owning rights holders, estates, and licensing intermediaries. It also moves the timing of payment to before creation, so clearance cost is borne at the riskiest point of a project, before any work exists to fund it.
% ABSENT_VOICES: Unrepresented transformative creators, audiences, and future creators are absent: no seat in licensing negotiations, no advocate in standard-setting, no budget in litigation. The public domain itself has no representative. Their objection — that the boundary converts cultural reference into a payable event — enters the record only through proxies such as amicus briefs and occasional crowdfunded defenses.
% DISAPPEARANCE_RATIONALE: If the rule that any use of expression constitutes a derivative preparation vanished overnight, remix, sampling, fan, and corpus-based creation would expand immediately without clearance; the licensing market for adaptations would contract to genuine recastings; the takedown and content-identification machinery built to enforce the rule would idle; and cultural production would reorganize around attribution norms rather than authorization norms. The beneficiary seats' revenue models would not survive the transition intact.
% FOUNDING_PROBLEM: The derivative work right was codified to stop verbatim or near-verbatim recastings — unauthorized translations, dramatizations, abridgments, and arrangements — that substituted for the original in its own market and diverted the author's expected revenue from those specific formats.
% FOUNDING_PROBLEM_CORROBORATION: The narrow founding problem is corroborated outside the beneficiary set by the codifying statute's legislative history and by copyright scholarship across the spectrum, including scholars who criticize expansion. The operative extension — that any use of copyrighted expression in creating new work is preparation of a derivative work, commercial or not, transformative or not — is attested in that form only by rights holders, their counsel, and licensing intermediaries; no corroborating source outside the benefiting parties endorses the extension, and organized scholarship outside that set explicitly rejects it. That asymmetry is itself signal.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extraction is high (0.78) because the rule converts every act of cultural reference into a payable event and the rate is set by the party controlling the catalog, decoupled from any marginal cost of authorization — a sample, a translation, a training example costs the rights holder nothing to license. Suppression is higher (0.82) because the rule's persistence depends on foreclosing the unauthorized alternative: unlicensed transformation survives only as a litigated exception, and the pre-creation timing of the gate means the cheapest rational response is not to begin. Theater ratio is 0.42: the incentive-to-create framing remains real for a narrowing band of working creators but is increasingly theatrical as royalty flows concentrate in catalogs and estates whose incentive needs are nil; enforcement activity mixes genuine anti-piracy work with defense of licensing exclusivity. Accessibility collapse is moderate (0.6): original-only creation and the public domain remain fully available and fair use occasionally holds, so alternatives are narrowed rather than erased. Resistance is high (0.7): the reading has lost significant cases, faces an organized open-culture counter-movement, and has repeatedly failed to obtain statutory confirmation of its scope. The victims are numerous but atomized; their coalition potential partially materializes through creator organizations and fair-use defense funds, which is what keeps resistance at 0.7 rather than lower. The measurement series run on one shared grid (t=0..48, seven points, all three metrics authored at every point) so no end-state value is silently substituted at earlier times; the trajectories show extraction and enforcement capacity accumulating together across the interval.
 *
 * PERSPECTIVAL GAP:
 *   From the catalog-owning beneficiary seat the rule is the incentive system that makes catalogs licensable assets and turns an enforcement budget into an investment with measurable return. From the remix-creator seat the same rule is a prohibition with no lawful version of their practice. From the platform seat it is a per-unit tax negotiable only at scale and passed to users. From the court seat it is a definitional question that arrives fresh with each technology wave. From the estate seat it is pure annuity. The engine computes per-seat classifications from these structural positions; the divergence between the beneficiary seat (coordination it administers and profits from) and the trapped payer seats (a gate on their practice's existence) is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries sit at the d-near-zero end: rights holders, estates, and intermediaries are subsidized by the rule, which manufactures demand for their authorization and grows their addressable market every time the boundary widens. Victims sit at the d-near-one end, differentiated by exit: remix creators are trapped (their form has no non-infringing version), downstream creators constrained (clearance is the price of the medium), platforms powerful but constrained (they can negotiate but not exit the ecosystem), libraries organized but budget-bound. The courts and legislature are agenda-setters with no direct flow in either direction — near symmetric by derivation — though every doctrinal choice they make moves every other seat's d. No directionality overrides are declared: the beneficiary and victim declarations plus the per-seat exit options produce the correct directionality for every seat without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this reading as a snare prevents the incentive cover story from laundering extraction: 'protecting creation' is the coordination story, but the coordination function actually performed is rights-clearance predictability, while the transfer function moves authorization rents upstream to parties with no creative activity at stake — estates and catalogs above all. The mandatrophy question is only partially live: the founding problem (market substitution by verbatim recastings) still exists in a narrow band, so this is not an atrophied mandate maintained by inertia — the constraint is actively maintained and profitable for its maintainers, which is snare, not piton. But the mandate has been expanded far past its founding scope, and the mismatch between founding scope and operative scope is precisely where the extraction lives. The classification keeps the genuine coordination residue (clearance predictability, recorded in the coordination-function answer and the resource_allocation Boltzmann type) visible without letting it justify the plenary scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is the enclosure_reading instantiation of the derivative_work_statutory_boundary kernel. How much of the measured extraction survives under the sibling coordination_reading, which defines the same boundary so that only fixed recastings substantially incorporating original expression are derivative works?',
    'Adjudication of the definitional scope — whether ''recast, transformed, or adapted'' reaches transformative and intermediate uses — by statute or a controlling precedent that adopts one reading expressly. Under the coordination reading the victim set shrinks to market-substituting recastings and pre-creation gating collapses to a narrow band.',
    'Under the coordination reading this constraint would reclassify toward rope (genuine substitution-prevention coordination with low extraction and a small victim set); the snare classification is contingent on the enclosure scope holding. The hybrid_carveout reading would produce an intermediate profile keyed to commercial exploitation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'The constraint''s type and extraction are contingent on which reading of the derivative work boundary is adopted; this story authors the enclosure reading only.').

omega_variable(
    royalty_flow_destination,
    'Does licensing revenue actually reach working creators whose incentives the rule claims to protect, or does it concentrate in catalog owners, estates, and intermediaries with no creative activity at stake?',
    'Royalty distribution audits, creator earnings studies, and contract-level analysis of licensing terms separating creator shares from catalog and administrative shares.',
    'If revenue concentrates upstream, the incentive justification is cover and the snare classification strengthens; if it flows to active creators, a portion of measured extraction is functioning incentive payment and the effective extraction is lower than the authored value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(royalty_flow_destination, empirical, 'Whether the extraction''s destination matches the constraint''s incentive rationale.').

omega_variable(
    chilling_vs_enforcement_gap,
    'How much downstream creation is deterred before it begins (never attempted, never measured) versus proceeding unlicensed and unenforced?',
    'Creator surveys on abandoned projects, platform takedown and compliance data, and cross-jurisdiction comparison of creation rates under different enforcement intensities.',
    'High chilling raises true suppression above the structural measure — the gate operates on projects that never exist to be counted; low chilling means the rule functions as a tax on the visible rather than a gate on the possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_vs_enforcement_gap, empirical, 'The unmeasured share of suppression: projects deterred at the pre-creation gate.').

omega_variable(
    fair_use_durability,
    'Can fair use durably contain this reading''s scope, or is the safety valve itself eroding through contractual override, technical protection measures, and platform terms stricter than the underlying law?',
    'Track fair-use success rates, the share of creator-platform relationships governed by terms that waive statutory exceptions, and litigation outcomes over successive technology waves.',
    'If the valve erodes, effective suppression rises toward 1.0 and the constraint hardens from a contested boundary into a closed gate; if the valve holds, effective extraction stays below the authored ceiling and the constraint remains partially self-limiting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_durability, conceptual, 'Whether the exception structure can durably bound the enclosure scope.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (statutory liability plus litigation cost plus takedown infrastructure) or internalized (creators who have learned that any reference is infringement and self-censor without any enforcement action)?',
    'Post-reform suppression trajectory: if creator caution persists after the liability scope narrows — for example after transformative-use rulings that clearly excuse a practice — the internalized share is substantial; if creation responds immediately to legal change, suppression is structural.',
    'Internalized suppression travels with creators after they exit the enforcement''s reach and would persist under statutory clarification; structural suppression remediates with the liability rule. The split determines whether reform of the rule alone is sufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized share of the constraint''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__enclosure_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(enclosure_boundary_tr_t0, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(enclosure_boundary_tr_t8, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(enclosure_boundary_tr_t16, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(enclosure_boundary_tr_t24, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(enclosure_boundary_tr_t32, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement(enclosure_boundary_tr_t40, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(enclosure_boundary_tr_t48, derivative_work_statutory_boundary__enclosure_reading, theater_ratio, 48, 0.42).

% Extraction over time
narrative_ontology:measurement(enclosure_boundary_be_t0, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(enclosure_boundary_be_t8, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(enclosure_boundary_be_t16, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(enclosure_boundary_be_t24, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(enclosure_boundary_be_t32, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(enclosure_boundary_be_t40, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement(enclosure_boundary_be_t48, derivative_work_statutory_boundary__enclosure_reading, base_extractiveness, 48, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(enclosure_boundary_su_t0, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(enclosure_boundary_su_t8, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(enclosure_boundary_su_t16, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(enclosure_boundary_su_t24, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(enclosure_boundary_su_t32, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(enclosure_boundary_su_t40, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 40, 0.78).
narrative_ontology:measurement(enclosure_boundary_su_t48, derivative_work_statutory_boundary__enclosure_reading, suppression_requirement, 48, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__enclosure_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__coordination_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__enclosure_reading, derivative_work_statutory_boundary__hybrid_carveout_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the derivative work right' decomposes into three structurally distinct boundary claims, per the epsilon-invariance principle: measuring the boundary as 'any use of expression' yields high extraction with a large victim set; measuring it as 'only fixed recastings' yields low extraction with a small victim set; measuring it as 'commercial use only' yields an intermediate profile keyed to exploitation mode. These are not one constraint viewed from three angles — they are three constraints with different epsilon values, different victim sets, and different failure modes, sharing one statutory text. This file instantiates the enclosure reading. The enclosure reading is downstream of the statutory text's literal breadth, which the other readings cite as the authority they interpret or narrow; it is linked to both siblings via network.affects_constraints as one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
