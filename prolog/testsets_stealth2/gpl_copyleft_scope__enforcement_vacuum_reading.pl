% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__enforcement_vacuum_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__enforcement_vacuum_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__enforcement_vacuum_reading
 *   human_readable: GPL Copyleft Scope — Interpretive Plurality Regime (Enforcement Vacuum Reading)
 *   domain: intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   Three decades of GPL Section 2(b) disputes have produced settlements,
 *   procedural dismissals, and dueling interpretive documents — but no
 *   appellate merits ruling on the scope question. This story authors the
 *   arrangement that absence constitutes: an interpretive-plurality regime in
 *   which the strong-copyleft and narrow-scope readings both remain live, and
 *   the boundary any given adopter faces is set by which interpretive
 *   community can enforce in that context — FSF-aligned projects with
 *   standing and budget on one side, industry-dominated ecosystems practicing
 *   their own reading on the other. The regime solves a real second-order
 *   coordination problem (heterogeneous practices coexist without constant
 *   litigation, keeping the ecosystem assembled) while imposing an asymmetric
 *   certainty tax on clarity-seeking adopters and leaving contributors'
 *   protections contingent on enforcement capacity. EPSILON REFERENT: the
 *   standing plurality arrangement itself, assessed by this reading's lights
 *   — not either doctrinal sibling, and not the alternative arrangement
 *   either camp would install if it won. Claim/metric independence: the
 *   constraint is CLAIMED as tangled_rope (genuine coordination function plus
 *   asymmetric extraction requiring active enforcement capacity); the metrics
 *   are authored independently at low-to-moderate extractive intensity, per
 *   this reading's structural expectation. KEY AGENTS (by structural
 *   relationship): - fsf_gnu_project: Agenda setter and secondary beneficiary
 *   (organized/identity_locked) — authors the license text and official
 *   interpretive line, declines final judgments -
 *   software_freedom_conservancy: Agenda setter (organized/identity_locked) —
 *   the active enforcement capacity that makes 'who can enforce' operative -
 *   pragmatic_commercial_adopters: Primary beneficiary with secondary payer
 *   position (powerful/mobile) — exploits ambiguity for architectural
 *   flexibility - clarity_seeking_enterprise_adopters: Primary target
 *   (powerful/constrained) — pays the certainty tax -
 *   independent_gpl_contributors: Target with secondary beneficiary position
 *   (moderate/identity_locked) — grant conditions vindicated only when
 *   capacity appears - oss_compliance_advisors: Beneficiary
 *   (organized/mobile) — collects the vacuum's most reliable fee income -
 *   permissive_license_ecosystems: Excluded voice (institutional/arbitrage) —
 *   opted out of the bargain, objects from outside - appellate_courts:
 *   Analytical observer (institutional/analytical) — the unfired resolver
 *
 * KEY AGENTS:
 *   - fsf_gnu_project: Agenda setter and secondary beneficiary (organized/identity_locked) — holds copyrights, publishes the license and FAQ interpretations, strategically avoids definitive rulings
 *   - software_freedom_conservancy: Agenda setter (organized/identity_locked) — executes compliance and litigation for member projects; its budget and standing are the scarce resource deciding which violations meet a capable opponent
 *   - pragmatic_commercial_adopters: Primary beneficiary, secondary payer (powerful/mobile) — ships GPL-adjacent combinations under industry-standard readings, receiving latitude a strict reading would deny
 *   - clarity_seeking_enterprise_adopters: Primary payer (powerful/constrained) — buys audits and opinions, restructures to avoid linking, pays dual-license fees, or forgoes GPL components
 *   - independent_gpl_contributors: Payer with secondary beneficiary position (moderate/identity_locked) — their copyleft conditions hold only where enforcement capacity exists
 *   - oss_compliance_advisors: Beneficiary (organized/mobile) — law firms, consultancies, and tooling vendors whose revenue scales with the number of unsettled questions
 *   - permissive_license_ecosystems: Excluded (institutional/arbitrage) — MIT/Apache/BSD-centered ecosystems competing for the same contributors while rejecting the reciprocal bargain
 *   - appellate_courts: Observer (institutional/analytical) — could settle the scope question with one merits ruling; cases keep leaving their dockets via settlement and procedural dismissal first
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__enforcement_vacuum_reading, 0.3).
domain_priors:theater_ratio(gpl_copyleft_scope__enforcement_vacuum_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(gpl_copyleft_scope__enforcement_vacuum_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__enforcement_vacuum_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__enforcement_vacuum_reading, "GPL Copyleft Scope — Interpretive Plurality Regime (Enforcement Vacuum Reading)").
narrative_ontology:topic_domain(gpl_copyleft_scope__enforcement_vacuum_reading, "intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__enforcement_vacuum_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__enforcement_vacuum_reading, 'be881d13-350b-4329-91a2-635c595cf4ab').
narrative_ontology:cs_kernel_codification('be881d13-350b-4329-91a2-635c595cf4ab', fixed_text).
narrative_ontology:cs_authority_grounding('be881d13-350b-4329-91a2-635c595cf4ab', distributed).
narrative_ontology:cs_reading_relation('be881d13-350b-4329-91a2-635c595cf4ab', gpl_copyleft_scope__strong_copyleft_reading, influences).
narrative_ontology:cs_reading_relation('be881d13-350b-4329-91a2-635c595cf4ab', gpl_copyleft_scope__narrow_scope_reading, coexists_with).
narrative_ontology:cs_axiom('be881d13-350b-4329-91a2-635c595cf4ab', foundational, precedent_avoidance_preserves_copyleft_leverage).
narrative_ontology:cs_axiom_status(precedent_avoidance_preserves_copyleft_leverage, holdable).
narrative_ontology:cs_axiom_grounding('be881d13-350b-4329-91a2-635c595cf4ab', precedent_avoidance_preserves_copyleft_leverage, instrumental).
narrative_ontology:cs_axiom('be881d13-350b-4329-91a2-635c595cf4ab', foundational, operative_scope_set_by_enforcement_capacity).
narrative_ontology:cs_axiom_status(operative_scope_set_by_enforcement_capacity, holdable).
narrative_ontology:cs_axiom_grounding('be881d13-350b-4329-91a2-635c595cf4ab', operative_scope_set_by_enforcement_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('be881d13-350b-4329-91a2-635c595cf4ab', licensed_interpretive_pluralism).
narrative_ontology:cs_drift_state('be881d13-350b-4329-91a2-635c595cf4ab', contemporary_post_consumer_electronics_suit_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('be881d13-350b-4329-91a2-635c595cf4ab', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_gnu_project).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, software_freedom_conservancy).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_commercial_adopters).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, oss_compliance_advisors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_enterprise_adopters).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, independent_gpl_contributors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__enforcement_vacuum_reading, independent_gpl_contributors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_commercial_adopters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the copyrights on the GNU toolchain and publishes the GPL, including the FAQ positions that define the movement's official line on linking and aggregation. Since the early 2000s it has declined to take infringement cases to final judgment, preferring negotiated resolutions that leave the scope question formally open. It draws mission legitimacy and volunteer alignment from the license's continued operation; walking away from the license would mean ceasing to be what the organization is.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_gnu_project, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, fsf_gnu_project, beneficiary).

% Runs compliance and enforcement on behalf of member projects: investigates suspected violations, sends notice-and-cure letters, negotiates settlements, and occasionally files suit — as in its case against a consumer-electronics manufacturer over GPL sources for smart TVs. Its budget and standing are the scarce resource that decides whether any given alleged violation meets a capable opponent. Settlement income and member support flow to it when enforcement succeeds.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, software_freedom_conservancy, agenda_setter,
    organized, generational, identity_locked, global).

% Large firms shipping products that combine GPL code with proprietary components under industry-standard interpretations of the scope question. They rely on the absence of a definitive ruling to treat their linking practices as lawful, gaining architectural latitude a strict reading would deny. They carry some legal-reserve and advisory spend and accept episodic settlement risk, but on net they receive more latitude than they pay for.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_commercial_adopters, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, pragmatic_commercial_adopters, payer).

% Firms — often in regulated industries or heavy acquisition pipelines — that refuse to ship anything resting on an unsettled interpretation. They buy licensing audits and written opinions, restructure products to avoid linking altogether, pay dual-licensing fees, or drop GPL components for weaker permissive substitutes. Every ambiguous question is a cost center for them; their alternatives are slower, costlier rebuilds or inferior third-party code.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, clarity_seeking_enterprise_adopters, payer,
    powerful, biographical, constrained, global).

% Volunteer developers who contribute code conditioned on downstream users honoring the license. Whether those conditions hold against a well-resourced infringer depends on whether some enforcement organization has the funding and standing to act in that case; in enforcement-poor contexts their conditions go unvindicated. They also benefit from the shared body of code the license sustains, and many are bound to the project by conviction rather than contract.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, independent_gpl_contributors, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_copyleft_scope__enforcement_vacuum_reading, independent_gpl_contributors, beneficiary).

% Law firms, consultancies, and tooling vendors selling scope opinions, audit reports, remediation plans, and compliance pipelines. Their revenue scales with the number of unsettled questions: each ambiguous boundary is a billable interpretation. They lose business if the questions get answered and gain it when new combination patterns raise new ones.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, oss_compliance_advisors, beneficiary,
    organized, biographical, mobile, global).

% Projects and corporate sponsors centered on MIT, Apache, and BSD licenses, who declined the reciprocal bargain altogether. They regard the entire scope controversy as self-inflicted friction that permissive licensing avoids, and they compete for the same contributors and corporate users. They are outside the GPL interpretive conversation by construction and say so from there.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, permissive_license_ecosystems, excluded,
    institutional, generational, arbitrage, global).

% Federal and state benches that could settle the scope question with a single merits ruling on the license's combination clause. Cases keep arriving at their doorsteps and keep leaving via settlement, procedural dismissal, or remand before any such ruling is reached. They observe the dispute without yet having adjudicated its core.
narrative_ontology:constraint_stakeholder(gpl_copyleft_scope__enforcement_vacuum_reading, appellate_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_copyleft_scope__enforcement_vacuum_reading, oss_compliance_advisors).
narrative_ontology:fixing_cost_class(gpl_copyleft_scope__enforcement_vacuum_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a reciprocal code commons: the GPL converts private modification rights into guaranteed downstream freedoms, solving the free-rider problem that would otherwise drain shared code into proprietary products. The scope vacuum adds a second-order coordination service: by leaving the combination boundary officially unsettled, it lets heterogeneous practices — strict free-software stacks and pragmatic commercial hybrids — coexist in one ecosystem without constant litigation.
% TRANSFER_FUNCTION: Moves compliance burden and risk from the ecosystem at large onto clarity-seeking adopters (audit spend, avoided integrations, dual-license fees, advisory retainers); moves discretionary scope-assertion power to whichever enforcement-capable actor is present in a given context; and moves effective combination latitude to pragmatic adopters beyond what the strict reading would authorize.
% ABSENT_VOICES: End users the license nominally protects have no seat in scope contests; volunteer contributors without legal representation appear only through advocacy proxies; permissive-license ecosystems are structurally outside the conversation and would object that the contest is avoidable friction; small businesses deterred from GPL adoption by counsel costs are absent entirely.
% DISAPPEARANCE_RATIONALE: A definitive overnight resolution — whichever way it went — would reprice thousands of products at once: under the strict reading, vast numbers of shipped combinations become infringing and architectures must be untangled or licensed; under the narrow reading, the copyleft's protective perimeter contracts and contribution incentives weaken. Due-diligence standards, license-choice defaults, and acquisition representations would all be rewritten within quarters.
% FOUNDING_PROBLEM: The GPL was written to stop proprietary appropriation of collectively built code: the copyleft condition exists so that derivatives of free code stay free. The scope question — what counts as a derivative when code links, aggregates, or communicates — was inherited from copyright doctrine that never produced a definitive software answer, and the enforcement posture that emerged (settle, never litigate to judgment) was built to protect the license from an adverse definitive ruling.
% FOUNDING_PROBLEM_CORROBORATION: Independent corroboration exists: academic copyright scholarship treats the derivative-work boundary for software linking as unsettled; courts decide analogous scope questions case-by-case without a controlling GPL holding; and the public docket record shows three decades of GPL disputes ending in settlements and procedural dismissals with no appellate merits ruling — verifiable by anyone, including parties hostile to the license. No attestation from outside the benefiting parties contradicts the live-status finding.
narrative_ontology:disappearance_verdict(gpl_copyleft_scope__enforcement_vacuum_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_copyleft_scope__enforcement_vacuum_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_copyleft_scope__enforcement_vacuum_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_copyleft_scope__enforcement_vacuum_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).
:- end_tests(gpl_copyleft_scope__enforcement_vacuum_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.38 — low-to-moderate, matching this reading's structural claim that the plurality regime taxes a minority seat while subsidizing another. The certainty tax falls on clarity-seeking adopters (audit spend, avoided integrations, dual-license fees) and contributors bear contingent exposure, but most participants remain net beneficiaries of the commons the license sustains, and exit is cheap (permissive stacks are fully available), which caps epsilon. Suppression is authored at 0.30 as a raw structural property — unscaled by power or scope: the residual coercive force is demand letters, license-termination threats, and reputational pressure, not barriers to leaving the ecosystem. Theater is 0.30: the license's protective function is real, but a growing share of activity around the scope question (compatibility matrices, compliance webinars, SBOM rituals, annual audit renewals) performs diligence rather than changing anyone's legal position. Accessibility collapse is 0.30 — unusually low for a construct: understanding the vacuum expands rather than collapses the option space, since pragmatists read it as permission, clarity-seekers can route around it, and permissive alternatives absorb defectors. Resistance is 0.45: license-switching waves, corporate contributor-agreement strategies, permissive migration, and the systematic refusal of well-resourced parties to fund a test case all push back without threatening the regime. The measurement series share one ten-point grid (1991-2026) so every metric is authored at every examined time point. The trajectories show a cyclical enforcement pattern — build-up (the BusyBox-era campaign), deliberate de-escalation, revival (the consumer-electronics suit), procedural stall — and the oscillation is itself part of the maintenance: periodic enforcement preserves leverage without ever resolving scope, an intermittent-reinforcement dynamic rather than noise. Base properties reflect the post-2021-cycle steady state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural inputs. Clarity-seeking adopters (payer, constrained) should classify the regime as a persistent tax on their risk preference; pragmatic adopters (beneficiary, mobile) should compute a subsidy — the same ambiguity is their architectural freedom. Contributors (payer, identity_locked) sit nearer the full-target end than their moderate power alone suggests, because their convictions remove the exit that would otherwise damp their effective burden; conversely, contributors acting as a class (through foundations or maintainer coalitions) are the one coalition path that could redistribute enforcement capacity and shift the regime. The enforcement organizations (agenda_setter, identity_locked) experience the arrangement as an asset they administer. The appellate bench (observer) sees an unresolved docket, not a constraint at all. The engine derives these divergences from the declared roles, exits, and locks; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real receipts: pragmatic adopters receive combination latitude; advisors receive fee income; the enforcement organizations receive settlements, membership support, and mission leverage. Victim declarations map to real burdens: clarity-seekers pay the certainty tax; contributors' grant conditions are vindicated only contingently. Derived directionalities should place advisors nearest the beneficiary pole (pure collection, mobile exit), pragmatic adopters low despite their secondary payer position (mobile exit damps it), enforcement organizations low (they administer the arrangement), clarity-seekers near the target pole (constrained exit amplifies), and contributors high (identity lock removes exit). No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms already separate every seat, and the two dual-positioned agents carry secondary roles rather than overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — deterring proprietary appropriation while avoiding an adverse definitive ruling — is live, so no mandatrophy declaration is authored, and the mismatch consumer should find status=live paired with verdict=world_rearranges: no zombie flag. The tangled_rope claim is what prevents mislabeling in both directions: calling this a rope would hide the certainty-tax asymmetry and the contingency of contributor protection; calling it a snare would ignore the genuine commons-coordination function and the cheap, heavily used exit into permissive licensing. If the founding problem dies — a definitive ruling landing, or proprietary appropriation ceasing to be attempted — the regime would either dissolve into the winning reading's constraint or persist as theatrical compliance, and the classification should then migrate accordingly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the enforcement_vacuum_reading of kernel gpl_copyleft_scope; which of the three readings would a definitive adjudication endorse, and how would this constraint''s referent dissolve into the winner''s?',
    'A final appellate merits ruling on GPL Section 2(b) scope, or equivalent authoritative codification (legislative clarification or a mass contractual restatement adopted across the ecosystem).',
    'If the strong reading wins, this constraint is replaced by a high-burden coupling rule aimed at proprietary combiners; if the narrow reading wins, by a near-zero-burden boundary rule; either outcome dissolves the plurality regime this story prices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, empirical, 'Committer-frame locator: this is one reading of the gpl_copyleft_scope kernel; the sibling readings are separate constraints with their own epsilon values.').

omega_variable(
    plurality_persistence_vs_transient_gap,
    'Is interpretive plurality a durable structural feature of the licensing regime (jurisdictional fragmentation, settlement economics, mutual avoidance by all capacity-holding parties) or a transient epistemic gap that any single ruling would close?',
    'Observe whether practice converges across jurisdictions after any definitive ruling anywhere: track license-interpretation behavior in other jurisdictions following a foreign merits decision.',
    'If structural, this constraint survives individual rulings and the moderate-burden profile persists indefinitely; if transient, the constraint is a waiting room and its costs are temporary friction pending resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plurality_persistence_vs_transient_gap, conceptual, 'Whether the enforcement vacuum is a stable feature or an interim state of ignorance.').

omega_variable(
    enforcement_capacity_distribution,
    'How does enforcement capacity actually distribute across contexts — enforcement-organization funding and standing, industry counter-capacity, jurisdictional variation — and which interpretive community prevails where?',
    'Systematic audit of GPL enforcement actions, organizational funding disclosures, standing rulings, and settlement terms across jurisdictions and industry sectors.',
    'Determines the variance of effective burden across seats: a capacity distribution favoring industry ecosystems pushes the regime toward de facto narrow scope; one favoring enforcement organizations restores strong-reading leverage in their domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_distribution, empirical, 'The empirical distribution of enforcement capacity that makes the ''who can enforce'' clause operative.').

omega_variable(
    clarity_tax_composition,
    'Of the burden clarity-seeking adopters bear, how much is transferred (advisory fees, dual-license payments) versus deadweight (abandoned integrations, inferior substitute architectures, delayed products)?',
    'Enterprise survey and procurement data comparing compliance spend against measured integration foregone and substitution costs.',
    'Calibrates the receipt picture: a high transferred share confirms the advisory seat as primary recipient; a high deadweight share pushes the receipt picture toward diffuse and weakens the capture reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clarity_tax_composition, empirical, 'Composition of the certainty tax between transferred rents and deadweight loss.').

omega_variable(
    strategic_ambiguity_intentionality,
    'Was the enforcement vacuum maintained deliberately (a strategic retreat from final judgments after early enforcement campaigns) or did it emerge from accident, funding limits, and procedural friction?',
    'Archival review of enforcement-policy statements and organizational deliberations across 2001-2021, triangulated against litigation-history reconstruction.',
    'Deliberate maintenance strengthens the curated-ambiguity reading of the regime (someone operates the vacuum for leverage); a purely emergent vacuum leans toward inertial persistence and softens the agenda-setting attribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_ambiguity_intentionality, conceptual, 'Whether the plurality regime is actively curated or merely accumulated.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__enforcement_vacuum_reading, 1991, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_vacuum_tr_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1991, 0.08).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t1991, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t1998, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t1998, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2003, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2003, 0.17).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2003, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2007, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2007, 0.23).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2007, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2011, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2011, 0.25).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2011, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2015, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2019, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2019, 0.29).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2019, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2021, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2021, 0.33).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2021, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2023, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2023, 0.31).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2023, observed).
narrative_ontology:measurement(gpl_vacuum_tr_t2026, gpl_copyleft_scope__enforcement_vacuum_reading, theater_ratio, 2026, 0.3).
narrative_ontology:measurement_basis(gpl_vacuum_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(gpl_vacuum_be_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1991, 0.2).
narrative_ontology:measurement_basis(gpl_vacuum_be_t1991, observed).
narrative_ontology:measurement(gpl_vacuum_be_t1998, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 1998, 0.24).
narrative_ontology:measurement_basis(gpl_vacuum_be_t1998, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2003, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2003, 0.3).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2003, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2007, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2007, 0.37).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2007, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2011, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2011, 0.33).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2011, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2015, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2019, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2019, 0.41).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2019, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2021, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2021, 0.44).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2021, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2023, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2023, 0.41).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2023, observed).
narrative_ontology:measurement(gpl_vacuum_be_t2026, gpl_copyleft_scope__enforcement_vacuum_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(gpl_vacuum_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl_vacuum_su_t1991, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1991, 0.12).
narrative_ontology:measurement_basis(gpl_vacuum_su_t1991, observed).
narrative_ontology:measurement(gpl_vacuum_su_t1998, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 1998, 0.16).
narrative_ontology:measurement_basis(gpl_vacuum_su_t1998, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2003, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2003, 0.22).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2003, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2007, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2007, 0.36).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2007, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2011, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2011, 0.27).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2011, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2015, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2015, 0.24).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2015, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2019, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2019, 0.26).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2019, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2021, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2021, 0.34).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2021, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2023, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2023, 0.3).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2023, observed).
narrative_ontology:measurement(gpl_vacuum_su_t2026, gpl_copyleft_scope__enforcement_vacuum_reading, suppression_requirement, 2026, 0.3).
narrative_ontology:measurement_basis(gpl_vacuum_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__enforcement_vacuum_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__enforcement_vacuum_reading, gpl_copyleft_scope__narrow_scope_reading).

% DUAL FORMULATION NOTE:
% 'GPL copyleft scope' is one colloquial label covering three structurally distinct constraints (epsilon-invariance decomposition): the strong-copyleft rule (high burden for proprietary combiners), the narrow-scope rule (near-zero burden for aggregators), and the plurality regime this file authors (moderate-low burden, taken as a certainty tax and contingent exposure). The doctrinal siblings are upstream in justification — each cites copyright doctrine as its warrant — while this reading is downstream in operation: it is the regime that exists because neither sibling has been definitively vindicated. Each file links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
