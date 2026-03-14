% ============================================================================
% CONSTRAINT STORY: institutional_accommodation_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_accommodation_access, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: institutional_accommodation_access
 *   human_readable: Institutional Accommodation Access Constraints
 *   domain: disability_rights/organizational_governance
 *
 * SUMMARY:
 *   Institutional accommodation access for disabled employees reveals a
 *   structural constraint operating between legal mandate (ADA-type
 *   frameworks requiring workplace accommodation) and institutional incentive
 *   (cost minimization through delay, gatekeeping, and functional
 *   inadequacy). The constraint exhibits the full spectrum of DR
 *   classification depending on observer position: disabled employees
 *   experience pure extraction (Snare), HR departments experience mixed
 *   coordination and extraction (Tangled Rope), the institution benefits from
 *   arbitrage between compliance theater and minimal cost (Rope), disability
 *   advocates experience organized resistance (Tangled Rope), the
 *   bureaucratic apparatus operates theatrically (Piton), and the analytical
 *   observer risks naturalizing institutional cost-shifting as inherent
 *   accessibility difficulty (Mountain). The growing theater ratio (0.48 →
 *   0.64) reflects increasing performative accommodation documentation
 *   without corresponding functional access improvement — a signature of
 *   Goodhart drift where compliance metrics (accommodations approved) replace
 *   functional metrics (can employee actually work?).
 *
 * KEY AGENTS:
 *   - Disabled Employees: Primary victims (powerless/trapped) — employment-dependent, require disclosure to request accommodation, face career penalty for being visibly accommodated, structurally unable to exit without income loss
 *   - Non-Disabled Employees: Secondary beneficiaries (institutional/arbitrage) — benefit from preferential access to roles and advancement; experience accommodation requests as resource competition
 *   - Institutional Administrators: Primary beneficiaries (institutional/arbitrage) — control accommodation pace and scope; benefit from compliance-without-cost theater; arbitrage between legal minimum and actual functional access
 *   - HR Department: Mixed agent (moderate/constrained) — legally required to coordinate accommodation but constrained by cost-control incentives and organizational resistance; genuine coordination function constrained by cost-shifting imperatives
 *   - Disability Rights Coalition: Organized victim (organized/constrained) — legal authority and public pressure provide exit options, but constrained by resource asymmetry and normalized institutional resistance to accommodation
 *   - Accessibility Quality (Abstract): Victim (powerless/trapped) — the structural good of genuine workplace accessibility cannot exit or organize; bears cost of institutional theater masking functional access failure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_accommodation_access, 0.52).
domain_priors:suppression_score(institutional_accommodation_access, 0.58).
domain_priors:theater_ratio(institutional_accommodation_access, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_accommodation_access, extractiveness, 0.52).
narrative_ontology:constraint_metric(institutional_accommodation_access, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(institutional_accommodation_access, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_accommodation_access, tangled_rope).
narrative_ontology:human_readable(institutional_accommodation_access, "Institutional Accommodation Access Constraints").
narrative_ontology:topic_domain(institutional_accommodation_access, "disability_rights/organizational_governance").

domain_priors:requires_active_enforcement(institutional_accommodation_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_accommodation_access, institutional_administrators).
narrative_ontology:constraint_beneficiary(institutional_accommodation_access, non_disabled_employees).
narrative_ontology:constraint_victim(institutional_accommodation_access, disabled_employees).
narrative_ontology:constraint_victim(institutional_accommodation_access, accessibility_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISABLED EMPLOYEE (SNARE) — Trapped by employment dependency and legal requirement to disclose disability to request accommodation. Faces suppression through procedural complexity, gatekeeping delays, and career penalty for requesting access. No viable exit: quitting means loss of income; staying means enduring extraction. Accommodation becomes contingent on institutional goodwill rather than guaranteed right.
constraint_indexing:constraint_classification(institutional_accommodation_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HR DEPARTMENT (TANGLED ROPE) — Constrained by legal compliance (ADA/similar frameworks) creating genuine coordination function, but also benefits from discretionary control over accommodation decisions. Experiences mixed incentives: legal duty to accommodate generates coordination benefit; cost-shifting and delay tactics generate extraction. Real agency exists but bounded by budget constraints and organizational culture.
constraint_indexing:constraint_classification(institutional_accommodation_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTION AGGREGATE (ROPE) — Experiences accommodation requirement as coordination mechanism: accessible workforce expands labor supply, legal compliance avoids litigation costs, accommodation norms improve retention. Arbitrage available through compliance theater (meeting letter-of-law while minimizing cost). Net beneficiary: the institution captures compliance credit while controlling accommodation quality.
constraint_indexing:constraint_classification(institutional_accommodation_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DISABILITY RIGHTS COALITION (TANGLED ROPE) — Organized agents (disability advocates, legal enforcement bodies) see both coordination function (accessibility standards create predictable workplace environments) and asymmetric extraction (institutions control accommodation pace and quality). Coalition has exit options through legal action and public pressure, but constrained by resource asymmetry and normalized institutional resistance.
constraint_indexing:constraint_classification(institutional_accommodation_access, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ACCOMMODATION THEATER (PITON) — The performative apparatus of accommodation bureaucracy (request forms, delay-justified reviews, 'interactive process' meetings) persists despite limited functional verification that disabled employees actually receive functional access. The theater maintains itself through institutional inertia and compliance theater — institutions show ADA compliance without ensuring workplace functionality. High theater ratio reflects that accommodation decisions are driven by legal liability minimization rather than actual access.
constraint_indexing:constraint_classification(institutional_accommodation_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational scope, some accommodation friction appears inherent to human organizational coordination: bodies are diverse, institutions are material systems with constraints, and perfect accessibility may be physically impossible in some contexts. This perspective risks naturalizing what is actually a contingent institutional choice (cost-shifting to disabled workers) as a law of nature (accessibility is expensive, so some access denial is inevitable). False summit detection will flag this as naturalization rather than genuine NL constraint.
constraint_indexing:constraint_classification(institutional_accommodation_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_accommodation_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_accommodation_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_accommodation_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_accommodation_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_accommodation_access, TR),
    TR >= 0.70.

:- end_tests(institutional_accommodation_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, reflects baseline extraction through accommodation gatekeeping (institutional delay, documentation burdens, cost-shifting) plus the accumulation of functional inadequacy. The trajectory from 0.38 to 0.52 shows increasing extraction as compliance theater displaces functional access — institutions develop more sophisticated ways to document accommodations while maintaining functional barriers. Suppression (0.58): Moderate-high, reflects combined structural barriers (bureaucratic gatekeeping, resource constraints, career penalty for visibility) and internalized suppression (disabled employees internalize shame about requiring accommodation, fear disclosure, accept inadequate accommodations to avoid being seen as burdensome). Theater ratio (0.48 → 0.64): High trajectory indicates that accommodation bureaucracy increasingly performs compliance rather than delivers access. Initial theater reflects basic documentation requirements; final theater reflects sophisticated compliance theater (accommodations approved but functionally inadequate, interactive processes conducted without actual functional outcome commitment). Claimed type (Tangled Rope): The constraint has genuine coordination function (legal mandate ensures some accommodation attempt, accessibility standards provide predictability) AND genuine extraction (cost-shifting, delay tactics, functional inadequacy). It requires active enforcement to maintain (both the legal mandate AND the institutional resistance create active tension).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence: disabled employees see pure extraction (Snare) while institutional administrators see pure coordination (Rope). This gap reveals the constraint's hybrid nature. The disabled employee's snare classification is not wrong — they genuinely experience extraction with limited exit options. The administrator's rope classification is not wrong — they genuinely experience coordination benefit from legal compliance. Both are true simultaneously because they inhabit different structural positions relative to the accommodation flow. The gap also reveals how institutional theater works: the piton classification (performative accommodation bureaucracy) explains how the rope and snare can coexist — the institution develops sophisticated compliance theater that appears as rope to administrators (efficient coordination documented through forms and meetings) while remaining snare to disabled employees (the theater is a cost they bear, not a functional access pathway). The disability rights coalition and HR department both see tangled rope because they occupy mixed positions: HR is both agent of institutional control and executor of legal mandate; the coalition is both victim (accommodation requests routinely delayed/denied) and organized agent (can escalate through legal action). The mountain classification at civilizational scope is a false summit: the analytical observer risks naturalizing institutional cost-shifting as inherent accessibility difficulty ('perfect accommodation is impossible, so some denial is inevitable') when the constraint is actually a contingent choice about cost allocation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the accommodation access flow. Disabled employees experience maximum extraction (high d, high f(d)): they are trapped (employment dependency, no arbitrage options) and victims (bear full cost of extraction through denied/delayed/inadequate accommodations). Institutional administrators experience negative effective extraction (low d, negative f(d)): they are beneficiaries (control accommodation scope) with arbitrage options (compliance theater allows cost-shifting). HR departments experience moderate extraction (mid-range d): legally required to accommodate (victim status) but cost-constrained (beneficiary status of cost control). Disability rights coalition experiences constrained extraction (moderate-high d): organized (can apply legal/public pressure) but constrained by resource asymmetry. The analytical observer at civilizational scope experiences minimal derived d due to analytical/analytical tuple (observer role reduces directionality), but the false summit detection flags the mountain classification as naturalization rather than genuine NL constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: This constraint resolves mandatrophy by exposing how legal compliance (ADA-type mandates) creates coordination theater while preserving extraction. The mandate itself appears to prevent mandatrophy by creating enforcement mechanism: disabled employees have legal rights, so the constraint should be rope (pure coordination toward meeting legal minimum). However, the constraint persists as snare + tangled_rope because institutional actors (administrators, HR) have discovered that legal compliance can be satisfied through theater (documentation, process, approved accommodations) while minimizing functional access (denying requests, delaying implementation, providing inadequate accommodations). The mandatrophy resolution identifies that the constraint is NOT a false positive for extraction — it genuinely extracts through functional inadequacy despite legal compliance. The theater ratio trajectory (0.48 → 0.64) confirms this: compliance documentation is increasing while functional access may be static or declining. The legal mandate prevented mandatrophy from fully degrading the constraint into pure piton (which would require theater ≥ 0.70 AND extraction collapse), but it did not prevent the constraint from becoming a tangled rope with high theater — legal requirement ensures some accommodation attempt (coordination), while institutional theater ensures extraction persists through means other than overt denial (functional inadequacy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    accommodation_cost_allocation,
    'Are accommodation costs legitimately distributed as shared institutional burden or systematically shifted to disabled employees through denial, delay, and functional inadequacy?',
    'Comparative institutional analysis: audit organizations with proactive accessibility investment vs reactive accommodation-request compliance. Measure actual functional access (can employee perform job duties?) vs documented accommodations (accommodations formally approved but functionally inadequate).',
    'If costs legitimately shared: constraint reclassifies toward Rope (genuine coordination with shared burden). If systematically shifted: constraint confirms as Snare from disabled employee perspective (extraction masked by legal compliance theater).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accommodation_cost_allocation, empirical, 'Cost allocation for accommodations: legitimate sharing vs extraction through denial/inadequacy').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of accommodation requests primarily structural (bureaucratic gatekeeping, resource barriers) or internalized (disabled employees internalize shame, fear disclosure, accept inadequate accommodations to avoid being seen as burdensome)?',
    'Qualitative research: exit interviews, accommodation non-utilization rates, comparison of accommodation rates pre/post organizational culture shifts toward disability inclusion. Post-exit trajectory: do employees report suppression effects (internalized shame, acceptance of inadequacy) persisting after leaving the organization?',
    'If structural: suppression score (0.58) accurately reflects barrier magnitude. If heavily internalized: structural suppression is lower than reported value suggests, but effective suppression higher due to cognitive capture. Affects identity_locked exit option classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Suppression mechanism: structural gatekeeping vs internalized shame and fear of disclosure').

omega_variable(
    legal_compliance_vs_functional_access,
    'Does institutional compliance with formal accommodation law (ADA documentation, interactive process) correlate with actual functional access (disabled employees can perform duties, participate fully in workplace)?',
    'Audit organizations on two dimensions: legal compliance metric (accommodation requests approved, documented accommodations in place) vs functional access metric (can disabled employee perform job duties without unreasonable hardship, participate in advancement opportunities, maintain health?). Measure correlation.',
    'If high correlation: accommodation system functions as intended (Rope dominates). If low correlation: theater ratio (0.64) confirmed — legal compliance theater masks functional extraction (Snare dynamics persist). Theater-compliance gap justifies Piton classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_compliance_vs_functional_access, empirical, 'Correlation between legal accommodation compliance and actual functional workplace access').

omega_variable(
    identity_lock_disability_disclosure,
    'To what degree do disabled employees experience disclosure of disability (necessary for requesting accommodation) as identity-threatening rather than merely costly? Does the identity-fusion with ''non-disabled employee'' status create exit barriers beyond material employment dependency?',
    'Qualitative research: employee narratives about disclosure decision-making. Diagnostic signal: employees report that NOT disclosing feels like denying core identity aspects (chronic pain, sensory processing, mobility needs constitute part of how they understand themselves), creating psychological barriers to functional workplace disclosure distinct from the material barriers. Compare to constrained exit option (material costs only).',
    'If significant identity-lock: exit option for some disabled employees shifts from constrained to identity_locked. Classification shifts from standard Snare (trapped + suppression) to Snare with identity-lock mechanism. Omega resolution informs whether cognitive escape (identity reframing, disability pride movements) provides exit pathway distinct from structural exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_disability_disclosure, conceptual, 'Identity-lock mechanism in disability disclosure: psychological vs structural exit barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_accommodation_access, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(accom_tr_t0, institutional_accommodation_access, theater_ratio, 0, 0.48).
narrative_ontology:measurement(accom_tr_t3, institutional_accommodation_access, theater_ratio, 3, 0.56).
narrative_ontology:measurement(accom_tr_t6, institutional_accommodation_access, theater_ratio, 6, 0.64).
narrative_ontology:measurement(accom_tr_t9, institutional_accommodation_access, theater_ratio, 9, 0.64).

% Extraction over time
narrative_ontology:measurement(accom_be_t0, institutional_accommodation_access, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(accom_be_t3, institutional_accommodation_access, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(accom_be_t6, institutional_accommodation_access, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(accom_be_t9, institutional_accommodation_access, base_extractiveness, 9, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_accommodation_access, resource_allocation).
narrative_ontology:affects_constraint(institutional_accommodation_access, workplace_discrimination_barriers).
narrative_ontology:affects_constraint(institutional_accommodation_access, accessible_technology_adoption).

% DUAL FORMULATION NOTE:
% Institutional accommodation access decomposes into multiple structurally distinct constraints: accommodation_request_gatekeeping (theater of process without function), functional_workplace_access (whether accommodations enable job performance), and visible_disability_stigma (internalized suppression from disclosure requirement). Each has different ε values. This story captures the aggregate extractiveness of the system as a tangled rope; specific decomposition would separate the snare (gatekeeping theater), rope (genuine coordination for approved accommodations), and piton (degraded accessibility infrastructure) into distinct stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_accommodation_access, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
